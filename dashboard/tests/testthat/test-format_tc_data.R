library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))

test_that("supported chart types are detected", {
  expect_true(is_tc_chart_type_supported("line"))
  expect_true(is_tc_chart_type_supported("stacked_bar"))
  expect_false(is_tc_chart_type_supported("scatter"))
  expect_false(is_tc_chart_type_supported("pie"))
})

test_that("line chart export uses series rows and category columns", {
  df <- data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("A", "B"), 2),
    revenue = c(10, 20, 30, 40)
  )

  result <- format_tc_data(
    df,
    chart_type = "line",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  )

  expect_equal(colnames(result)[1], "")
  expect_equal(result[[1]], c("A", "B"))
  expect_equal(as.character(result[1, 2:3]), c("10", "30"))
  expect_equal(as.character(result[2, 2:3]), c("20", "40"))
})

test_that("stacked bar chart export is transposed", {
  df <- data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("A", "B"), 2),
    revenue = c(10, 20, 30, 40)
  )

  result <- format_tc_data(
    df,
    chart_type = "stacked_bar",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  )

  expect_equal(colnames(result)[1], "")
  expect_equal(as.character(result[[1]]), c("Q1", "Q2"))
  expect_equal(as.character(result[1, 2]), "10")
  expect_equal(as.character(result[2, 2]), "30")
})

test_that("grouped bar and stacked bar share the same layout", {
  df <- data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("A", "B"), 2),
    revenue = c(10, 20, 30, 40)
  )

  grouped <- format_tc_data(
    df,
    chart_type = "grouped_bar",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  )

  stacked <- format_tc_data(
    df,
    chart_type = "stacked_bar",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  )

  expect_equal(grouped, stacked)
})

test_that("unsupported chart types error", {
  df <- data.frame(
    quarter = "Q1",
    product = "A",
    revenue = 1
  )

  expect_error(
    format_tc_data(
      df,
      chart_type = "pie",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue"
    ),
    "Unsupported think-cell chart type"
  )
})

test_that("facet export returns one matrix per facet level", {
  df <- data.frame(
    region = rep(c("North", "South"), each = 4),
    quarter = rep(rep(c("Q1", "Q2"), each = 2), 2),
    product = rep(c("A", "B"), 4),
    revenue = c(10, 20, 30, 40, 11, 21, 31, 41)
  )

  result <- format_tc_data(
    df,
    chart_type = "line",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    facet_col = "region",
    agg_fun = NULL
  )

  expect_type(result, "list")
  expect_named(result, c("North", "South"))
  expect_equal(as.character(result$North[1, 2:3]), c("10", "30"))
  expect_equal(as.character(result$South[2, 3]), "41")
})

test_that("facet export writes multi-sheet workbook", {
  skip_if_not_installed("writexl")

  df <- data.frame(
    region = rep(c("North", "South"), each = 4),
    quarter = rep(rep(c("Q1", "Q2"), each = 2), 2),
    product = rep(c("A", "B"), 4),
    revenue = c(10, 20, 30, 40, 11, 21, 31, 41)
  )

  tc_data <- format_tc_data(
    df,
    chart_type = "line",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    facet_col = "region",
    agg_fun = NULL
  )

  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  write_tc_xlsx(tc_data, path)

  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("a factor category column exports in factor-level (plotted) order, not alphabetical", {
  # Levels are the reverse of alphabetical, mimicking a chart that orders bars
  # deliberately (e.g. by value). The export must follow the plotted order.
  df <- data.frame(
    quarter = factor(c("Q3", "Q1", "Q3", "Q1"), levels = c("Q3", "Q1")),
    product = c("A", "A", "B", "B"),
    revenue = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- format_tc_data(
    df, chart_type = "line",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL
  )

  # Category columns follow the factor levels (Q3, Q1), not alphabetical (Q1, Q3).
  expect_equal(colnames(result)[-1], c("Q3", "Q1"))
})

test_that("a factor series column exports rows in factor-level order", {
  df <- data.frame(
    quarter = c("Q1", "Q1", "Q2", "Q2"),
    product = factor(c("B", "A", "B", "A"), levels = c("B", "A")),
    revenue = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- format_tc_data(
    df, chart_type = "line",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL
  )

  # Series rows follow the factor levels (B, A), not alphabetical (A, B).
  expect_equal(as.character(result[[1]]), c("B", "A"))
})

test_that("explicit category_order still overrides factor levels", {
  df <- data.frame(
    quarter = factor(c("Q1", "Q2"), levels = c("Q1", "Q2")),
    product = c("A", "A"),
    revenue = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- format_tc_data(
    df, chart_type = "line",
    category_col = "quarter", series_col = "product", value_col = "revenue",
    agg_fun = NULL, category_order = c("Q2", "Q1")
  )

  expect_equal(colnames(result)[-1], c("Q2", "Q1"))
})
