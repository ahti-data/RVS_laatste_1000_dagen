library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))
source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "favorites.R"))

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

test_that("deck ZIP combines table + log for favorites without a template", {
  z <- tempfile(fileext = ".zip")
  entries <- list(
    list(label = "No template chart", dashboard_title = "D", tab_label = "T",
         subtab_label = "S", chart_type = "waterfall", template_name = NA_character_,
         selections = list(), slide_order = "auto", slide_block = NULL,
         tc_table = sample_df())
  )
  favorites_build_deck_zip(z, entries = entries)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("favorites_table.xlsx" %in% files)
  expect_true("log.txt" %in% files)
  expect_true("NO_TEMPLATE.txt" %in% files)
  expect_false(any(grepl("deck", files)))
})

test_that("deck ZIP falls back to template + combined .ppttc when no renderer is available", {
  skip_if_not(have_templates, "templates directory not available")
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
  expect_true("favorites_table.xlsx" %in% files)
  expect_true("log.txt" %in% files)
})

test_that("the fallback deck .ppttc references co-located templates, not the absolute capture-time path", {
  # Regression test for the same bug class as tc_build_slide_zip(): the
  # slide_block captured at star-time embeds an absolute template path valid
  # only on the machine that captured it. The shipped favorites_deck.ppttc
  # must instead reference each template by the bare file name copied
  # alongside it.
  skip_if_not(have_templates, "templates directory not available")
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

test_that("a working ppttc executable renders one combined deck for multiple favorites", {
  skip_if_not(have_templates, "templates directory not available")
  skip_on_os("windows")  # stub is a POSIX shell script
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
