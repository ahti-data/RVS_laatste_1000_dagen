library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))
source(file.path("..", "..", "utils", "slide_download.R"))

templates_dir <- file.path("..", "..", "templates")
have_templates <- dir.exists(templates_dir)

# A think-cell matrix shaped exactly like format_tc_data() output:
# first header cell empty, column 1 = row labels, rest = category columns.
sample_matrix <- function() {
  m <- data.frame(
    lab = c("A", "B"),
    `2023` = c(42, 30.5),
    `2024` = c(NA, 22),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(m) <- c("", "2023", "2024")
  m
}

test_that("chart types map to the correct template (or none)", {
  expect_true(tc_chart_type_has_template("line"))
  expect_true(tc_chart_type_has_template("grouped_bar"))
  expect_true(tc_chart_type_has_template("bar"))
  expect_true(tc_chart_type_has_template("stacked_bar"))
  expect_true(tc_chart_type_has_template("stacked_bar_100"))
  expect_false(tc_chart_type_has_template("waterfall"))
  expect_false(tc_chart_type_has_template("scatter"))
})

test_that("tc_template_available also requires the file to exist", {
  skip_if_not(have_templates, "templates directory not available")
  # present in the repo
  expect_true(tc_template_available("line", templates_dir))
  expect_true(tc_template_available("bar", templates_dir))
  expect_true(tc_template_available("grouped_bar", templates_dir))
  # mapped, but the .pptx has not been added yet -> not available
  if (!file.exists(file.path(templates_dir, "template_v_bar_stacked.pptx"))) {
    expect_false(tc_template_available("stacked_bar", templates_dir))
  }
  # never mapped
  expect_false(tc_template_available("scatter", templates_dir))
})

test_that("legacy aliases normalise before template lookup", {
  # 'standaard' / 'area' normalise to line
  expect_true(tc_chart_type_has_template("standaard"))
  expect_true(tc_chart_type_has_template("area"))
})

test_that("ppttc JSON is valid and structured for think-cell", {
  json <- tc_build_ppttc_json(sample_matrix(), "C:\\tmp\\t.pptx", "Title", "Fig")
  # backslashes converted, all automation fields present
  expect_true(grepl('"template":"C:/tmp/t.pptx"', json, fixed = TRUE))
  expect_true(grepl('"SlideTitle"', json))
  expect_true(grepl('"Chart1"', json))
  expect_true(grepl('"FigureTitle"', json))
  # NA -> JSON null; header top-left is null; numbers unquoted
  expect_true(grepl("null", json))
  expect_true(grepl('\\{"number":42\\}', json))
})

test_that("empty titles are omitted but the chart is always present", {
  json <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "", "")
  expect_false(grepl("SlideTitle", json))
  expect_false(grepl("FigureTitle", json))
  expect_true(grepl("Chart1", json))
})

test_that("a chart_id, when supplied, is embedded as a ChartID data block", {
  json <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "Title", "Fig", chart_id = "exp_abc123")
  expect_true(grepl('"ChartID"', json))
  expect_true(grepl('"string":"exp_abc123"', json, fixed = TRUE))
})

test_that("no ChartID block is added when chart_id is omitted or empty", {
  json1 <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "Title", "Fig")
  json2 <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "Title", "Fig", chart_id = "")
  expect_false(grepl("ChartID", json1))
  expect_false(grepl("ChartID", json2))
})

test_that("log records dashboard/tab/subtab and joins multi-value options", {
  log <- tc_build_log(
    "Laatste 1000 dagen", "Iteratie 1", "Zorg Totaal",
    list(tot_pop = "all", tot_variables = c("a", "b")),
    "grouped_bar", "template_v_bar_group.pptx", rendered = TRUE
  )
  expect_true(grepl("Dashboard:      Laatste 1000 dagen", log))
  expect_true(grepl("Sub-tab:        Zorg Totaal", log))
  expect_true(grepl("tot_variables: a, b", log))
})

test_that("log includes a Chart ID line, first, when supplied", {
  log <- tc_build_log(
    "D", "T", "S", list(), "line", "template_line.pptx",
    rendered = TRUE, chart_id = "exp_xyz789"
  )
  expect_true(grepl("Chart ID:       exp_xyz789", log))
  # printed before the rest of the header, so it's the first thing a PM sees
  expect_true(which(grepl("Chart ID:", strsplit(log, "\n")[[1]])) <
                which(grepl("Dashboard:", strsplit(log, "\n")[[1]])))
})

test_that("no Chart ID line when chart_id is omitted", {
  log <- tc_build_log("D", "T", "S", list(), "line", "template_line.pptx", rendered = TRUE)
  expect_false(grepl("Chart ID:", log))
})

test_that("datasheet log is one delimited line built from the same fields as log.txt", {
  line <- tc_build_datasheet_log(
    "Laatste 1000 dagen", "Iteratie 1", "Zorg Totaal", "grouped_bar",
    list(tot_pop = "all", tot_variables = c("a", "b"))
  )
  expect_false(grepl("\n", line))
  expect_true(startsWith(line, "LOG | "))
  expect_true(grepl("dashboard=Laatste 1000 dagen", line, fixed = TRUE))
  expect_true(grepl("tab=Iteratie 1", line, fixed = TRUE))
  expect_true(grepl("sub-tab=Zorg Totaal", line, fixed = TRUE))
  expect_true(grepl("chart_type=grouped_bar", line, fixed = TRUE))
  expect_true(grepl("tot_variables=a, b", line, fixed = TRUE))
})

test_that("datasheet log includes chart_id only when supplied", {
  with_id    <- tc_build_datasheet_log("D", "T", "S", "line", list(), chart_id = "exp_xyz789")
  without_id <- tc_build_datasheet_log("D", "T", "S", "line", list())
  expect_true(grepl("chart_id=exp_xyz789", with_id, fixed = TRUE))
  expect_false(grepl("chart_id=", without_id, fixed = TRUE))
})

test_that("a datasheet_log, when supplied, replaces the null corner cell", {
  json <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "Title", "Fig",
                               datasheet_log = "LOG | dashboard=D; tab=T")
  expect_true(grepl('"string":"LOG | dashboard=D; tab=T"', json, fixed = TRUE))
})

test_that("the corner cell stays null when datasheet_log is omitted or empty", {
  json1 <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "Title", "Fig")
  json2 <- tc_build_ppttc_json(sample_matrix(), "t.pptx", "Title", "Fig", datasheet_log = "")
  expect_true(grepl("^\\[\\{.*\\[\\[null,", json1))
  expect_true(grepl("^\\[\\{.*\\[\\[null,", json2))
})

# ---- ZIP assembly (stub the xlsx writer to avoid a writexl dependency) ------
stub_writer <- function(data, path) writeLines("stub", path)

test_that("no-template chart still yields a ZIP with table + explanation", {
  z <- tempfile(fileext = ".zip")
  res <- tc_build_slide_zip(
    z, sample_matrix(), "waterfall",
    filename_prefix = "bfly", templates_dir = templates_dir,
    write_table_fun = stub_writer
  )
  files <- utils::unzip(z, list = TRUE)$Name
  expect_false(res$rendered)
  expect_true("NO_TEMPLATE.txt" %in% files)
  expect_true("bfly_table.xlsx" %in% files)
  expect_true("log.txt" %in% files)
  expect_false(any(grepl("slide", files)))
})

test_that("supported chart without think-cell ships template + ppttc fallback", {
  skip_if_not(have_templates, "templates directory not available")
  z <- tempfile(fileext = ".zip")
  res <- tc_build_slide_zip(
    z, sample_matrix(), "line",
    filename_prefix = "iter1_tijd", templates_dir = templates_dir,
    ppttc_exe = NA, write_table_fun = stub_writer
  )
  files <- utils::unzip(z, list = TRUE)$Name
  expect_false(res$rendered)
  expect_true("slide_template.pptx" %in% files)
  expect_true("chart_data.ppttc" %in% files)
  expect_true(any(grepl("README", files)))
  expect_true("iter1_tijd_table.xlsx" %in% files)
  expect_true("log.txt" %in% files)
})

test_that("a chart_id passed to tc_build_slide_zip lands in both log.txt and the shipped .ppttc", {
  skip_if_not(have_templates, "templates directory not available")
  z <- tempfile(fileext = ".zip")
  tc_build_slide_zip(
    z, sample_matrix(), "line",
    filename_prefix = "iter1_tijd", templates_dir = templates_dir,
    ppttc_exe = NA, write_table_fun = stub_writer, chart_id = "exp_history42"
  )
  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)

  log_txt <- paste(readLines(file.path(extract_dir, "log.txt")), collapse = "\n")
  expect_true(grepl("Chart ID:       exp_history42", log_txt))

  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
  expect_true(grepl('"ChartID"', ppttc))
  expect_true(grepl('"string":"exp_history42"', ppttc, fixed = TRUE))
})

test_that("tc_build_slide_zip embeds the selection log in the shipped .ppttc's datasheet corner cell", {
  skip_if_not(have_templates, "templates directory not available")
  z <- tempfile(fileext = ".zip")
  tc_build_slide_zip(
    z, sample_matrix(), "line",
    dashboard_title = "Laatste 1000 dagen", tab_label = "Iteratie 1", subtab_label = "Zorg Totaal",
    selections = list(tot_pop = "all"),
    filename_prefix = "iter1_tijd", templates_dir = templates_dir,
    ppttc_exe = NA, write_table_fun = stub_writer, chart_id = "exp_history42"
  )
  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")

  # Corner cell (table[0][0]) carries the log, not null; the figure's actual
  # categories/values are unaffected -- confirmed via the existing chart_id
  # coverage above still passing, plus the presence of the sample data below.
  expect_true(grepl('"table":\\[\\[\\{"string":"LOG \\| dashboard=Laatste 1000 dagen', ppttc))
  expect_true(grepl("tab=Iteratie 1", ppttc, fixed = TRUE))
  expect_true(grepl("sub-tab=Zorg Totaal", ppttc, fixed = TRUE))
  expect_true(grepl("chart_id=exp_history42", ppttc, fixed = TRUE))
  expect_true(grepl('\\{"number":42\\}', ppttc))
})

test_that("the fallback .ppttc references the co-located template, not the absolute path used to resolve it", {
  # Regression test: the shipped chart_data.ppttc must be portable to a
  # different machine, so it must reference "slide_template.pptx" (the file
  # copied alongside it) rather than the absolute path this machine resolved
  # the template to (which is meaningless -- or worse, silently wrong -- on
  # whatever PC opens the bundle).
  skip_if_not(have_templates, "templates directory not available")
  z <- tempfile(fileext = ".zip")
  tc_build_slide_zip(
    z, sample_matrix(), "line",
    filename_prefix = "portable_check", templates_dir = templates_dir,
    ppttc_exe = NA, write_table_fun = stub_writer
  )
  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")

  expect_true(grepl('"template":"slide_template.pptx"', ppttc, fixed = TRUE))
  resolved_path <- tc_template_for_chart_type("line", templates_dir = templates_dir)
  expect_false(grepl(resolved_path, ppttc, fixed = TRUE))
})

test_that("options log is scoped to the active chart via prefix map", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  rv <- shiny::reactiveValues(
    tot_pop = "all", tot_jaar = "2023",
    it3_top50_ranked_by = "x", it3_top50_prest_ranked_by = "y",
    it3_zvwk_cost_type = "ZVW Kosten Totaal", it3_zvwk_cohort = "2023",
    main_nav = "Iteratie 3", iter3_tabs = "ZVW-kosten verdeling",
    plotly_relayout = list(a = 1)  # plumbing
  )
  tc_register_app_context(
    input = rv, dashboard_title = "D", nav_id = "main_nav",
    subtab_by_tab = c("Iteratie 3" = "iter3_tabs"),
    dl_option_prefixes = c(
      "iter1_totaal_dl"     = "^tot_",
      "iter3_top50_main_dl" = "^it3_top50_(?!prest_)",
      "iter3_prest_main_dl" = "^it3_top50_prest_",
      "iter3_zvwk_dl"       = "^it3_zvwk_"
    )
  )
  shiny::isolate({
    zvwk <- tc_ctx_selections("iter3_zvwk_dl")
    expect_equal(sort(names(zvwk)), c("it3_zvwk_cohort", "it3_zvwk_cost_type"))

    # negative lookahead keeps main and prest separate despite shared stem
    top_main <- tc_ctx_selections("iter3_top50_main_dl")
    expect_equal(names(top_main), "it3_top50_ranked_by")
    prest <- tc_ctx_selections("iter3_prest_main_dl")
    expect_equal(names(prest), "it3_top50_prest_ranked_by")

    tot <- tc_ctx_selections("iter1_totaal_dl")
    expect_equal(sort(names(tot)), c("tot_jaar", "tot_pop"))

    # unmapped module -> fallback drops plumbing + nav ids but keeps options
    fb <- tc_ctx_selections("unknown_dl")
    expect_false("plotly_relayout" %in% names(fb))
    expect_false("main_nav" %in% names(fb))
    expect_true("tot_pop" %in% names(fb))
  })
})

test_that("slide matrix uses the reference orientation (categories in header)", {
  # transposed table (categories in first column, series in header) ...
  tbl <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
                    x = c("2019", "2023"), Hart = c(120, 150), Kanker = c(340, 300))
  names(tbl)[1] <- ""
  slide <- tc_slide_orientation(tbl, "grouped_bar")
  # ... becomes series in first column, categories across the header
  expect_equal(names(slide), c("", "2019", "2023"))
  expect_equal(slide[[1]], c("Hart", "Kanker"))
  expect_equal(as.numeric(slide[slide[[1]] == "Hart", -1]), c(120, 150))

  # line charts are already in reference orientation -> unchanged
  expect_identical(tc_slide_orientation(tbl, "line"), tbl)
})

test_that("template listing and override choices work", {
  skip_if_not(have_templates, "templates directory not available")
  files <- tc_list_templates(templates_dir)
  expect_true("template_v_bar.pptx" %in% files)
  expect_true("template_line.pptx" %in% files)

  ch <- tc_template_choices(templates_dir)
  expect_equal(unname(ch[[1]]), "")                      # first = automatic
  expect_equal(names(ch)[[1]], "Automatisch (gedetecteerd)")
  expect_true("template_v_bar.pptx" %in% unname(ch))
})

test_that("tc_detect_slide_type agrees with tc_prepare_slide", {
  d <- data.frame(category = "CT", series = c("A", "B"), v = c(1, 2), stringsAsFactors = FALSE)
  expect_equal(tc_detect_slide_type(d, "grouped_bar", "category", "series"), "bar")
  d2 <- data.frame(category = c("x", "y"), series = rep(c("A", "B"), each = 1),
                   v = c(1, 2), stringsAsFactors = FALSE)
  # 2 categories, 2 series -> stays grouped
  d2 <- data.frame(category = rep(c("x", "y"), each = 2), series = rep(c("A", "B"), 2),
                   v = 1:4, stringsAsFactors = FALSE)
  expect_equal(tc_detect_slide_type(d2, "grouped_bar", "category", "series"), "grouped_bar")
  # missing column -> falls back to declared type, no error
  expect_equal(tc_detect_slide_type(d2, "line", "nope", "series"), "line")
})

test_that("transpose round-trips", {
  m <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
                  x = c("a", "b"), P = c(1, 2), Q = c(3, 4), R = c(5, 6))
  names(m)[1] <- ""
  expect_equal(tc_transpose_matrix(tc_transpose_matrix(m)), m)
})

test_that("category ordering matches the displayed numeric axis", {
  cats <- paste0("-", 1:5)                    # "-1".."-5" as they arrive
  m <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
                  lab = "Overleden",
                  `-1` = 1, `-2` = 2, `-3` = 3, `-4` = 4, `-5` = 5)
  names(m) <- c("", cats)

  # auto -> numeric ascending (fixes -1..-5 to -5..-1)
  expect_equal(names(tc_order_slide_matrix(m, "auto"))[-1],
               c("-5", "-4", "-3", "-2", "-1"))
  # as_is -> untouched
  expect_equal(names(tc_order_slide_matrix(m, "as_is"))[-1], cats)
  # descending
  expect_equal(names(tc_order_slide_matrix(m, "cat_desc"))[-1], cats)

  # non-numeric categories: auto leaves them as displayed (safe)
  mm <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
                   lab = "v", mrt = 3, jan = 1, feb = 2)
  names(mm)[1] <- ""
  expect_equal(names(tc_order_slide_matrix(mm, "auto"))[-1], c("mrt", "jan", "feb"))
})

test_that("tc_numeric_or_na handles numeric strings and decimals", {
  expect_equal(tc_numeric_or_na(c("-1", "-10", "-2")), c(-1, -10, -2))
  expect_equal(tc_numeric_or_na(c("1,5", "2,5")), c(1.5, 2.5))
  expect_null(tc_numeric_or_na(c("jan", "feb")))
})

test_that("plot type is detected from the displayed data", {
  # declared stacked_bar but only one series -> plain bar, categories on x
  d1 <- data.frame(category = c("jan", "feb", "mrt"), series = "dom",
                   export_value = c(100, 120, 90), stringsAsFactors = FALSE)
  p1 <- tc_prepare_slide(d1, "stacked_bar", "category", "series", "export_value", agg_fun = NULL)
  expect_equal(p1$chart_type, "bar")
  expect_equal(names(p1$matrix), c("", "jan", "feb", "mrt"))

  # declared grouped_bar but only one category -> plain bar, series on x
  d2 <- data.frame(category = "CT", series = c("Overleden", "In leven"),
                   export_value = c(340, 120), stringsAsFactors = FALSE)
  p2 <- tc_prepare_slide(d2, "grouped_bar", "category", "series", "export_value", agg_fun = NULL)
  expect_equal(p2$chart_type, "bar")
  expect_equal(names(p2$matrix), c("", "Overleden", "In leven"))
  expect_equal(as.numeric(p2$matrix[1, -1]), c(340, 120))

  # genuine 2x2 grouped bar stays grouped
  d3 <- data.frame(category = rep(c("2019", "2023"), each = 2),
                   series = rep(c("Hart", "Kanker"), 2),
                   export_value = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  p3 <- tc_prepare_slide(d3, "grouped_bar", "category", "series", "export_value", agg_fun = NULL)
  expect_equal(p3$chart_type, "grouped_bar")

  # line stays line
  p4 <- tc_prepare_slide(d3, "line", "category", "series", "export_value", agg_fun = NULL)
  expect_equal(p4$chart_type, "line")
})

test_that("short path is a no-op that normalises on non-Windows", {
  skip_on_os("windows")
  f <- tempfile(fileext = ".pptx"); file.create(f)
  expect_false(grepl("\\\\", tc_short_path(f)))
  expect_true(file.exists(tc_short_path(f)))
})

test_that("a working ppttc executable produces slide.pptx and no fallback", {
  skip_if_not(have_templates, "templates directory not available")
  skip_on_os("windows")  # stub is a POSIX shell script
  exe <- tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", 'out="$3"; printf "PK\\003\\004" > "$out"'), exe)
  Sys.chmod(exe, "0755")
  z <- tempfile(fileext = ".zip")
  res <- tc_build_slide_zip(
    z, sample_matrix(), "grouped_bar",
    filename_prefix = "iter1_basis", templates_dir = templates_dir,
    ppttc_exe = exe, write_table_fun = stub_writer
  )
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true(res$rendered)
  expect_true("slide.pptx" %in% files)
  expect_false("slide_template.pptx" %in% files)
})
