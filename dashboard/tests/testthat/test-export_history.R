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
  log_txt <- paste(readLines(file.path(extract_dir, "log.txt")), collapse = "\n")
  expect_true(grepl("Chart ID:       exp_redownload_test", log_txt))

  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
  expect_true(grepl('"string":"exp_redownload_test"', ppttc, fixed = TRUE))
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
