library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))
source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "favorites.R"))
source(file.path("..", "..", "utils", "export_history.R"))

templates_dir <- file.path("..", "..", "templates")
have_templates <- dir.exists(templates_dir)

with_favorites_path <- function(code) {
  path <- tempfile("favorites_", fileext = ".json")
  old <- Sys.getenv("SHINY_FAVORITES_PATH", unset = NA)
  Sys.setenv(SHINY_FAVORITES_PATH = path)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_FAVORITES_PATH") else Sys.setenv(SHINY_FAVORITES_PATH = old)
    unlink(path)
  }, add = TRUE)
  force(code)
}

with_history_dir <- function(code) {
  dir <- tempfile("export_history_")
  old <- Sys.getenv("SHINY_EXPORT_HISTORY_DIR", unset = NA)
  Sys.setenv(SHINY_EXPORT_HISTORY_DIR = dir)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_EXPORT_HISTORY_DIR") else Sys.setenv(SHINY_EXPORT_HISTORY_DIR = old)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)
  force(code)
}

sample_df <- function() {
  data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("A", "B"), 2),
    revenue = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

test_that("favorites_path honors SHINY_FAVORITES_PATH and defaults to state/favorites.json", {
  with_favorites_path({
    expect_equal(favorites_path(), Sys.getenv("SHINY_FAVORITES_PATH"))
  })
  old <- Sys.getenv("SHINY_FAVORITES_PATH", unset = NA)
  Sys.unsetenv("SHINY_FAVORITES_PATH")
  on.exit(if (!is.na(old)) Sys.setenv(SHINY_FAVORITES_PATH = old), add = TRUE)
  expect_equal(favorites_path(), file.path("state", "favorites.json"))
})

test_that("favorites_list is empty before anything is saved", {
  with_favorites_path({
    expect_equal(favorites_list(), list())
  })
})

test_that("add/list/remove round-trip through the JSON file", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "Revenue Q1-Q4", chart_type = "stacked_bar"))
    id2 <- favorites_add(list(label = "Another chart", chart_type = "line"))

    entries <- favorites_list()
    expect_length(entries, 2)
    expect_equal(entries[[1]]$label, "Revenue Q1-Q4")
    expect_true(nzchar(entries[[1]]$id))
    expect_true(nzchar(entries[[1]]$created_at))

    favorites_remove(id1)
    remaining <- favorites_list()
    expect_length(remaining, 1)
    expect_equal(remaining[[1]]$label, "Another chart")

    favorites_remove(id2)
    expect_equal(favorites_list(), list())
  })
})

test_that("favorite asset path lives beside favorites.json, keyed by id", {
  with_favorites_path({
    expect_equal(
      normalizePath(favorites_assets_dir(), winslash = "/", mustWork = FALSE),
      normalizePath(file.path(dirname(favorites_path()), "favorite_assets"), winslash = "/", mustWork = FALSE)
    )
    expect_equal(basename(favorite_asset_path("abc123")), "abc123.png")
  })
})

test_that("removing a favorite also deletes its PNG snapshot, if any", {
  with_favorites_path({
    id <- favorites_add(list(label = "Has a snapshot"))
    dir.create(favorites_assets_dir(), recursive = TRUE, showWarnings = FALSE)
    writeBin(as.raw(1:4), favorite_asset_path(id))
    expect_true(file.exists(favorite_asset_path(id)))

    favorites_remove(id)
    expect_false(file.exists(favorite_asset_path(id)))
  })
})

test_that("removing a favorite with no PNG snapshot doesn't error", {
  with_favorites_path({
    id <- favorites_add(list(label = "No snapshot"))
    expect_false(file.exists(favorite_asset_path(id)))
    expect_error(favorites_remove(id), NA)
  })
})

test_that("favorites_remove_all empties the list and deletes every PNG snapshot", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "One"))
    favorites_add(list(label = "Two"))
    dir.create(favorites_assets_dir(), recursive = TRUE, showWarnings = FALSE)
    writeBin(as.raw(1:4), favorite_asset_path(id1))

    favorites_remove_all()

    expect_equal(favorites_list(), list())
    expect_false(file.exists(favorite_asset_path(id1)))
  })
})

test_that("favorites_remove_ids removes only the given ids, leaving the rest untouched", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "One"))
    id2 <- favorites_add(list(label = "Two"))
    id3 <- favorites_add(list(label = "Three"))

    favorites_remove_ids(c(id1, id3))

    remaining <- favorites_list()
    expect_length(remaining, 1)
    expect_equal(remaining[[1]]$id, id2)
  })
})

test_that("favorites persist across a fresh read (simulating an app restart)", {
  with_favorites_path({
    favorites_add(list(label = "Survives restart"))
    # A brand-new call to favorites_list() re-reads from disk, not memory.
    expect_equal(favorites_list()[[1]]$label, "Survives restart")
  })
})

test_that("favorites_capture builds a slide block when a template matches", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  expect_equal(entry$label, "Revenue")
  expect_false(is.na(entry$template_name))
  expect_true(grepl('"template"', entry$slide_block, fixed = TRUE))
  expect_equal(entry$tc_table$columns[1], "")
  expect_gt(nrow(favorites_table_as_df(entry$tc_table)), 0)
})

test_that("favorites_capture has no slide block when no template matches", {
  entry <- favorites_capture(
    data = data.frame(category = "CT", series = "A", v = 1, stringsAsFactors = FALSE),
    chart_type = "waterfall",
    category_col = "category", series_col = "series", value_col = "v",
    templates_dir = templates_dir
  )
  expect_true(is.na(entry$template_name))
  expect_null(entry$slide_block)
})

test_that("favorites_capture labels a favorite with the chart's own title, not the sub-tab, when one is supplied", {
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    figure_title = "Revenue by Product", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  expect_equal(entry$label, "Revenue by Product")
})

test_that("favorites_capture falls back to the sub-tab label when the chart has no title", {
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  expect_equal(entry$label, "Revenue")
})

test_that("favorites_capture stores module_id when supplied", {
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    module_id = "revenue_chart_dl", filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  expect_equal(entry$module_id, "revenue_chart_dl")
})

test_that("favorites_selections_inline renders a compact, truncated one-liner", {
  expect_equal(favorites_selections_inline(NULL), "")
  expect_equal(favorites_selections_inline(list()), "")
  # empty values are dropped
  expect_equal(
    favorites_selections_inline(list(jaar = "2023", leeg = "", pop = character(0))),
    "jaar: 2023"
  )
  # multiple options joined; multi-value collapsed
  expect_equal(
    favorites_selections_inline(list(jaar = "2023", groep = c("A", "B"))),
    paste0("jaar: 2023 ", intToUtf8(0x00B7), " groep: A, B")
  )
  # long strings are truncated with an ellipsis
  long <- favorites_selections_inline(list(x = paste(rep("y", 300), collapse = "")), max_chars = 20)
  expect_true(nchar(long) <= 20)
  expect_equal(substr(long, nchar(long), nchar(long)), intToUtf8(0x2026))
})

test_that("favorites_table_as_df passes a live data.frame through unchanged", {
  df <- sample_df()
  expect_identical(favorites_table_as_df(df), df)
})

test_that("table storage/restore round-trips columns and values, including an empty header", {
  m <- data.frame(lab = c("A", "B"), `2023` = c(42, 30.5), check.names = FALSE)
  names(m)[1] <- ""

  stored <- favorites_table_to_storage(m)
  expect_equal(stored$columns, c("", "2023"))

  restored <- favorites_table_as_df(stored)
  expect_equal(names(restored), c("", "2023"))
  expect_equal(restored[[1]], c("A", "B"))
  expect_equal(restored[["2023"]], c(42, 30.5))
})

test_that("the empty first-column header survives a real JSON round-trip", {
  # This is the exact bug this shape avoids: jsonlite renames an empty
  # data.frame column name to its positional index ("1") when a data.frame is
  # serialized directly, which favorites_table_to_storage() sidesteps.
  m <- data.frame(lab = "A", `2023` = 42, check.names = FALSE)
  names(m)[1] <- ""
  stored <- favorites_table_to_storage(m)

  path <- tempfile(fileext = ".json")
  jsonlite::write_json(stored, path, auto_unbox = TRUE)
  reread <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  expect_equal(reread$columns[[1]], "")
  restored <- favorites_table_as_df(reread)
  expect_equal(names(restored)[1], "")
})

test_that("deck ZIP with no favorites still produces a valid, explanatory ZIP", {
  z <- tempfile(fileext = ".zip")
  favorites_build_deck_zip(z, entries = list())
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("README.txt" %in% files)
})

test_that("deck ZIP includes the table even for favorites without a template", {
  z <- tempfile(fileext = ".zip")
  entries <- list(
    # No raw_table field -- mimics a favorite saved before that field existed.
    list(label = "No template chart", dashboard_title = "D", tab_label = "T",
         subtab_label = "S", chart_type = "waterfall", template_name = NA_character_,
         selections = list(), slide_order = "auto", slide_block = NULL,
         tc_table = sample_df())
  )
  favorites_build_deck_zip(z, entries = entries)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  expect_false("log.txt" %in% files)
  expect_true("NO_TEMPLATE.txt" %in% files)
  expect_false(any(grepl("deck", files)))
  # No raw_table on this entry -> the raw workbook is skipped, not an error.
  expect_false("favorites_raw_tables.xlsx" %in% files)
})

test_that("deck ZIP includes a raw-tables workbook alongside the think-cell one", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
  z <- tempfile(fileext = ".zip")
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  favorites_build_deck_zip(z, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  expect_true("favorites_raw_tables.xlsx" %in% files)

  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  raw <- as.data.frame(readxl::read_excel(
    file.path(extract_dir, "favorites_raw_tables.xlsx"), sheet = "Revenue"
  ))
  # The raw workbook holds the original long-format plot data, not the
  # think-cell pivoted matrix.
  expect_true(all(c("quarter", "product", "revenue") %in% names(raw)))
  expect_equal(nrow(raw), nrow(sample_df()))
  })
})

test_that("deck ZIP includes a chart_<label>.png for favorites with a captured snapshot", {
  with_favorites_path({
    dir.create(favorites_assets_dir(), recursive = TRUE, showWarnings = FALSE)
    # A fake PNG (content doesn't matter for this test -- just that it's
    # copied into the zip under the right name).
    writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47)), favorite_asset_path("with_snap"))

    entries <- list(
      list(id = "with_snap", label = "Has Snapshot", dashboard_title = "D",
           tab_label = "T", subtab_label = "S", chart_type = "line",
           template_name = NA_character_, selections = list(), slide_order = "auto",
           slide_block = NULL, tc_table = sample_df()),
      list(id = "no_snap", label = "No Snapshot", dashboard_title = "D",
           tab_label = "T", subtab_label = "S", chart_type = "line",
           template_name = NA_character_, selections = list(), slide_order = "auto",
           slide_block = NULL, tc_table = sample_df())
    )

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries)
    files <- utils::unzip(z, list = TRUE)$Name

    expect_true("chart_Has Snapshot.png" %in% files)
    expect_false("chart_No Snapshot.png" %in% files)
  })
})

test_that("deck ZIP falls back to template + combined .ppttc when no renderer is available", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
  z <- tempfile(fileext = ".zip")
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  favorites_build_deck_zip(z, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("favorites_deck.ppttc" %in% files)
  expect_true(any(grepl("README_render_deck", files)))
  expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  expect_false("log.txt" %in% files)
  })
})

test_that("the fallback deck .ppttc references co-located templates, not the absolute capture-time path", {
  # Regression test for the same bug class as tc_build_slide_zip(): the
  # slide_block captured at star-time embeds an absolute template path valid
  # only on the machine that captured it. The shipped favorites_deck.ppttc
  # must instead reference each template by the bare file name copied
  # alongside it.
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
  z <- tempfile(fileext = ".zip")
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  # Sanity check on the "before" state: the captured block embeds a full
  # (short-)path, not just the bare template file name.
  captured_template_value <- sub('.*"template":"([^"]*)".*', "\\1", entry$slide_block)
  expect_gt(nchar(captured_template_value), nchar(entry$template_name))

  favorites_build_deck_zip(z, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)
  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")

  expect_true(grepl(sprintf('"template":"%s"', entry$template_name), ppttc, fixed = TRUE))
  expect_false(grepl(captured_template_value, ppttc, fixed = TRUE))
  })
})

test_that("each favorite in a deck download gets its own datasheet log in the corner cell", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
  z <- tempfile(fileext = ".zip")
  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    selections = list(view = "quarterly"),
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  favorites_build_deck_zip(z, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)
  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")

  expect_true(grepl('"string":"LOG \\| ', ppttc))
  expect_true(grepl("dashboard=D; tab=T; sub-tab=Revenue", ppttc, fixed = TRUE))
  expect_true(grepl("view=quarterly", ppttc, fixed = TRUE))
  })
})

test_that("every favorite in one 'Download all favorites' click shares a single favorite_download_id", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
  z <- tempfile(fileext = ".zip")
  entry1 <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  entry2 <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Cost",
    filename_prefix = "cost_chart", templates_dir = templates_dir
  )
  favorites_build_deck_zip(z, entries = list(entry1, entry2), ppttc_exe = NA, templates_dir = templates_dir)

  history <- export_history_list()
  expect_length(history, 2)
  fdl_ids <- unique(vapply(history, function(e) tc_or(e$favorite_download_id, ""), character(1)))
  expect_length(fdl_ids, 1)
  expect_true(nzchar(fdl_ids))
  expect_true(startsWith(fdl_ids, "favdl_"))
  # ...and every download_id among them is distinct.
  expect_length(unique(vapply(history, function(e) e$id, character(1))), 2)

  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")
  expect_equal(lengths(regmatches(ppttc, gregexpr("FavoriteDownloadID", ppttc)))[[1]], 2)
  })
})

test_that("downloading a favorites deck auto-logs each renderable favorite to Export History", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- favorites_capture(
      data = sample_df(), chart_type = "stacked_bar",
      category_col = "quarter", series_col = "product", value_col = "revenue",
      agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir
    )

    expect_equal(export_history_list(), list())

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)

    history <- export_history_list()
    expect_length(history, 1)
    expect_equal(history[[1]]$subtab_label, "Revenue")
    expect_equal(history[[1]]$chart_type, "stacked_bar")

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")
    expect_true(grepl(paste0("download_id=", history[[1]]$id), ppttc, fixed = TRUE))
    expect_true(grepl(sprintf('"string":"%s"', history[[1]]$id), ppttc, fixed = TRUE))
  })
})

test_that("each favorites-deck download mints a fresh history entry, not a reused one", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- favorites_capture(
      data = sample_df(), chart_type = "stacked_bar",
      category_col = "quarter", series_col = "product", value_col = "revenue",
      agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir
    )

    z1 <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z1, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)
    z2 <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z2, entries = list(entry), ppttc_exe = NA, templates_dir = templates_dir)

    history <- export_history_list()
    expect_length(history, 2)
    expect_false(identical(history[[1]]$id, history[[2]]$id))
  })
})

test_that("a favorite with no matching template is not logged to Export History", {
  with_history_dir({
    entry <- favorites_capture(
      data = data.frame(category = "CT", series = "A", v = 1, stringsAsFactors = FALSE),
      chart_type = "waterfall",
      category_col = "category", series_col = "series", value_col = "v",
      templates_dir = templates_dir
    )
    expect_null(entry$slide_block)

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = list(entry))
    expect_equal(export_history_list(), list())

    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("NO_TEMPLATE.txt" %in% files)
    expect_false(any(grepl("deck", files)))
  })
})

test_that("a working ppttc executable renders one combined deck for multiple favorites", {
  skip_if_not(have_templates, "templates directory not available")
  skip_on_os("windows")  # stub is a POSIX shell script
  with_history_dir({
  exe <- tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", 'out="$3"; printf "PK\\003\\004" > "$out"'), exe)
  Sys.chmod(exe, "0755")

  entry <- favorites_capture(
    data = sample_df(), chart_type = "stacked_bar",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )

  z <- tempfile(fileext = ".zip")
  favorites_build_deck_zip(z, entries = list(entry, entry), ppttc_exe = exe, templates_dir = templates_dir)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("favorites_deck.pptx" %in% files)
  expect_false(any(grepl("README_render_deck", files)))
  })
})
