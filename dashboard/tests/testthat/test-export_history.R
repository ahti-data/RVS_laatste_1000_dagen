library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))
source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "favorites.R"))
source(file.path("..", "..", "utils", "export_history.R"))

templates_dir <- file.path("..", "..", "templates")
have_templates <- dir.exists(templates_dir)

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

sample_matrix <- function() {
  m <- data.frame(lab = c("A", "B"), `2023` = c(42, 30.5), `2024` = c(NA, 22),
                  check.names = FALSE, stringsAsFactors = FALSE)
  names(m) <- c("", "2023", "2024")
  m
}

test_that("export_history_dir honors SHINY_EXPORT_HISTORY_DIR and defaults to state/export_history", {
  with_history_dir({
    expect_equal(export_history_dir(), Sys.getenv("SHINY_EXPORT_HISTORY_DIR"))
  })
  old <- Sys.getenv("SHINY_EXPORT_HISTORY_DIR", unset = NA)
  Sys.unsetenv("SHINY_EXPORT_HISTORY_DIR")
  on.exit(if (!is.na(old)) Sys.setenv(SHINY_EXPORT_HISTORY_DIR = old), add = TRUE)
  expect_equal(export_history_dir(), file.path("state", "export_history"))
})

test_that("export_history_list is empty before anything is logged", {
  with_history_dir({
    expect_equal(export_history_list(), list())
  })
})

test_that("add/list/get/remove round-trip through one-file-per-entry storage", {
  with_history_dir({
    id1 <- export_history_add(list(label = "Chart One", chart_type = "line"))
    Sys.sleep(1.1)  # ensure a distinct created_at for ordering
    id2 <- export_history_add(list(label = "Chart Two", chart_type = "bar"))

    expect_true(nzchar(id1))
    expect_true(nzchar(id2))
    expect_true(file.exists(file.path(export_history_dir(), paste0(id1, ".json"))))

    entries <- export_history_list()
    expect_length(entries, 2)
    # Most recently created first.
    expect_equal(entries[[1]]$label, "Chart Two")
    expect_equal(entries[[2]]$label, "Chart One")

    got <- export_history_get(id1)
    expect_equal(got$label, "Chart One")
    expect_null(export_history_get("does_not_exist"))

    export_history_remove(id1)
    remaining <- export_history_list()
    expect_length(remaining, 1)
    expect_equal(remaining[[1]]$label, "Chart Two")
  })
})

test_that("export_history_add respects a pre-set id instead of generating a new one", {
  with_history_dir({
    id <- export_history_add(list(id = "exp_fixed_id", label = "Pinned"))
    expect_equal(id, "exp_fixed_id")
    expect_true(file.exists(file.path(export_history_dir(), "exp_fixed_id.json")))
    expect_equal(export_history_get("exp_fixed_id")$label, "Pinned")
  })
})

test_that("a corrupt entry file is skipped, not fatal, when listing", {
  with_history_dir({
    export_history_add(list(label = "Good entry"))
    dir.create(export_history_dir(), recursive = TRUE, showWarnings = FALSE)
    writeLines("not valid json {{{", file.path(export_history_dir(), "exp_broken.json"))

    entries <- export_history_list()
    expect_length(entries, 1)
    expect_equal(entries[[1]]$label, "Good entry")
  })
})

test_that("tc_history_capture resolves the template and a sensible label", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  expect_equal(entry$label, "Revenue")
  expect_false(is.na(entry$template_name))
  expect_equal(entry$tc_data_table$columns[1], "")
  expect_null(entry$slide_matrix_table)
})

test_that("tc_history_capture stores slide_matrix separately when supplied", {
  skip_if_not(have_templates, "templates directory not available")
  slide_m <- data.frame(lab = "X", `2023` = 1, check.names = FALSE)
  names(slide_m)[1] <- ""
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line", slide_matrix = slide_m,
    filename_prefix = "chart", templates_dir = templates_dir
  )
  expect_false(is.null(entry$slide_matrix_table))
  restored <- favorites_table_as_df(entry$slide_matrix_table)
  expect_equal(restored[[1]], "X")
})

test_that("tc_history_capture stores favorite_download_id and module_id", {
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    filename_prefix = "my_chart", favorite_download_id = "favdl_xyz", module_id = "my_chart_dl"
  )
  expect_equal(entry$favorite_download_id, "favdl_xyz")
  expect_equal(entry$module_id, "my_chart_dl")
})

test_that("tc_history_capture falls back to filename_prefix when no subtab_label is set", {
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "waterfall",
    filename_prefix = "my_chart"
  )
  expect_equal(entry$label, "my_chart")
  expect_true(is.na(entry$template_name))
})

test_that("export_history_redownload rebuilds a zip carrying the entry's chart_id", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  entry$id <- "exp_redownload_test"

  z <- tempfile(fileext = ".zip")
  export_history_redownload(entry, z, templates_dir = templates_dir, ppttc_exe = NA)

  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
  expect_true(grepl("download_id=exp_redownload_test", ppttc, fixed = TRUE))
  expect_true(grepl('"string":"exp_redownload_test"', ppttc, fixed = TRUE))
})

test_that("export_history_redownload re-embeds a stored favorite_download_id", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir,
    favorite_download_id = "favdl_stored"
  )
  entry$id <- "exp_redownload_test2"

  z <- tempfile(fileext = ".zip")
  export_history_redownload(entry, z, templates_dir = templates_dir, ppttc_exe = NA)

  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
  expect_true(grepl('"FavoriteDownloadID"', ppttc))
  expect_true(grepl('"string":"favdl_stored"', ppttc, fixed = TRUE))
})

test_that("tc_history_entry_subtitle joins non-empty breadcrumb parts", {
  expect_equal(
    tc_history_entry_subtitle(list(dashboard_title = "D", tab_label = "T", subtab_label = "S")),
    "D / T / S"
  )
  expect_equal(
    tc_history_entry_subtitle(list(dashboard_title = "", tab_label = "T", subtab_label = "")),
    "T"
  )
  expect_equal(tc_history_entry_subtitle(list()), "")
})

# ---- Regenerate -------------------------------------------------------------
# A plain list(userData = new.env()) stands in for a Shiny session here --
# tc_chart_registry()/*_register()/*_get() only ever touch session$userData,
# so this is a faithful, dependency-free substitute for these tests.
fake_session <- function() list(userData = new.env())

test_that("export_history_resolve_regenerate_target finds a solo entry by its own id", {
  with_history_dir({
    id <- export_history_add(list(label = "Chart One", chart_type = "line"))
    target <- export_history_resolve_regenerate_target(id)
    expect_equal(target$type, "solo")
    expect_length(target$entries, 1)
    expect_equal(target$entries[[1]]$id, id)
  })
})

test_that("export_history_resolve_regenerate_target finds every entry sharing a favorite_download_id", {
  with_history_dir({
    export_history_add(list(label = "A", favorite_download_id = "favdl_group1"))
    export_history_add(list(label = "B", favorite_download_id = "favdl_group1"))
    export_history_add(list(label = "C", favorite_download_id = "favdl_other"))

    target <- export_history_resolve_regenerate_target("favdl_group1")
    expect_equal(target$type, "bulk")
    expect_length(target$entries, 2)
  })
})

test_that("export_history_resolve_regenerate_target returns NULL for an unknown id", {
  with_history_dir({
    expect_null(export_history_resolve_regenerate_target("exp_does_not_exist"))
    expect_null(export_history_resolve_regenerate_target("favdl_does_not_exist"))
    expect_null(export_history_resolve_regenerate_target(""))
  })
})

test_that("export_history_regenerate_entry falls back to a snapshot rebuild when the chart isn't registered", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir,
      module_id = "not_a_registered_module"
    )
    entry$id <- export_history_new_id()

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_entry(entry, z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_false(res$live)

    history <- export_history_list()
    expect_length(history, 1)
    expect_false(identical(history[[1]]$id, entry$id))

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
    expect_true(grepl(paste0("download_id=", history[[1]]$id), ppttc, fixed = TRUE))
  })
})

test_that("export_history_regenerate_entry uses the live build function when the chart is registered", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir,
      module_id = "live_chart_dl"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    session <- fake_session()
    build_calls <- 0
    tc_chart_registry_register(session, "live_chart_dl", list(
      build_zip = function(zip_path) {
        build_calls <<- build_calls + 1
        writeLines("live content", zip_path)
      },
      get_spec = function() stop("not used in this test")
    ))

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_entry(entry, z, session, templates_dir = templates_dir, ppttc_exe = NA)
    expect_true(res$live)
    expect_equal(build_calls, 1)
    expect_equal(readLines(z), "live content")
  })
})

test_that("export_history_regenerate solo path mints a fresh download id, never reusing the pasted one", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate(entry$id, z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_false(res$bulk)

    history <- export_history_list()
    expect_length(history, 2)
    ids <- vapply(history, function(e) e$id, character(1))
    expect_true(entry$id %in% ids)
    expect_true(any(ids != entry$id))
  })
})

test_that("export_history_regenerate bulk path mints one fresh favorite_download_id shared by every member", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    e1 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir, favorite_download_id = "favdl_original"
    )
    e1$id <- export_history_new_id()
    export_history_add(e1)
    e2 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Cost", filename_prefix = "cost_chart",
      templates_dir = templates_dir, favorite_download_id = "favdl_original"
    )
    e2$id <- export_history_new_id()
    export_history_add(e2)

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate("favdl_original", z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_true(res$bulk)
    expect_equal(res$total, 2)

    history <- export_history_list()
    expect_length(history, 4)
    # every regenerated entry shares one *new* favorite_download_id, distinct from the original
    regenerated <- Filter(function(e) {
      !is.null(e$favorite_download_id) && nzchar(e$favorite_download_id) &&
        !identical(e$favorite_download_id, "favdl_original")
    }, history)
    expect_length(regenerated, 2)
    expect_equal(length(unique(vapply(regenerated, function(e) e$favorite_download_id, character(1)))), 1)

    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  })
})
