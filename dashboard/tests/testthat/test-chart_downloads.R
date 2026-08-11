library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))
source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "dictionary.R"))
source(file.path("..", "..", "utils", "chart_downloads.R"))

with_dictionary_path <- function(code) {
  path <- tempfile("dictionary_", fileext = ".json")
  old <- Sys.getenv("SHINY_DICTIONARY_PATH", unset = NA)
  Sys.setenv(SHINY_DICTIONARY_PATH = path)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_DICTIONARY_PATH") else Sys.setenv(SHINY_DICTIONARY_PATH = old)
    unlink(path)
  }, add = TRUE)
  force(code)
}

sample_data <- function() {
  data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("Product A", "Product B"), 2),
    revenue = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

test_that("the 'Format from dictionary' checkbox toggles category/series relabeling for downloads", {
  with_dictionary_path({
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")
    dictionary_set_entry("Product A", "product", "Product Alpha")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      df_on <- data()
      expect_setequal(unique(df_on$quarter), c("Kwartaal 1", "Q2"))
      expect_setequal(unique(df_on$product), c("Product Alpha", "Product B"))

      session$setInputs(dictionary_format = FALSE)
      df_off <- data()
      expect_setequal(unique(df_off$quarter), c("Q1", "Q2"))
      expect_setequal(unique(df_off$product), c("Product A", "Product B"))
    })
  })
})

test_that("category_order/series_order are relabeled to match the checkbox, so factor levels still line up", {
  with_dictionary_path({
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")
    dictionary_set_entry("Q2", "quarter", "Kwartaal 2")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL,
      category_order = c("Q2", "Q1")
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_equal(resolved_category_order(), c("Kwartaal 2", "Kwartaal 1"))

      session$setInputs(dictionary_format = FALSE)
      expect_equal(resolved_category_order(), c("Q2", "Q1"))
    })
  })
})

test_that("category_scope/series_scope default to the column name, and an explicit scope overrides it", {
  with_dictionary_path({
    # Entry stored under scope "quarter" -- matches the column name used below,
    # so the default (no explicit category_scope) finds it.
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_equal(scope_or_col(NULL, "quarter"), "quarter")
      expect_true("Kwartaal 1" %in% data()$quarter)
    })

    # Same raw values, but the chart's column is generically named "category"
    # -- omitting category_scope would default to scope "category" and miss
    # every entry stored under "quarter"; passing category_scope = "quarter"
    # explicitly recovers the intended lookup -- exactly the pattern
    # "Zorg Totaal" (iter1_totaal_dl) relies on for real.
    generic_data <- function() {
      df <- sample_data()
      names(df)[names(df) == "quarter"] <- "category"
      df
    }

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart_generic",
      data = shiny::reactive(generic_data()),
      chart_type = "stacked_bar",
      category_col = "category",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_false("Kwartaal 1" %in% data()$category)
    })

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart_generic_scoped",
      data = shiny::reactive(generic_data()),
      chart_type = "stacked_bar",
      category_col = "category",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL,
      category_scope = "quarter"
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_true("Kwartaal 1" %in% data()$category)
    })
  })
})

test_that("a value with no matching dictionary entry passes through unchanged, not through the generic fallback prettifier", {
  with_dictionary_path({
    # No dictionary_set_entry() calls at all -- every value below is a miss.
    already_pretty <- function() {
      data.frame(
        quarter = c("18-29 jaar", "30-39 jaar"),
        product = c("Product A", "Product B"),
        revenue = c(10, 20),
        stringsAsFactors = FALSE
      )
    }

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(already_pretty()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL,
      category_order = c("18-29 jaar", "30-39 jaar")
    ), {
      session$setInputs(dictionary_format = TRUE)
      # dictionary_default_prettify() would have turned this into
      # "18 29 Jaar" (hyphen -> space, "jaar" wrongly capitalized) -- the
      # identity fallback must leave it exactly as given instead.
      expect_setequal(unique(data()$quarter), c("18-29 jaar", "30-39 jaar"))
      expect_equal(resolved_category_order(), c("18-29 jaar", "30-39 jaar"))
    })
  })
})

test_that("relabeling an ordered factor column relabels its levels too, instead of silently losing the order", {
  with_dictionary_path({
    dictionary_set_entry("Q2", "quarter", "Kwartaal 2")
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")

    # An ordered factor, exactly like `stats::reorder(name, value)` produces
    # for "Zorg Totaal"'s own export (so it matches the plot's bar order) --
    # deliberately factor levels in the OPPOSITE order from alphabetical, so
    # a silent fall-back to character/alphabetical order would be caught.
    ordered_data <- function() {
      data.frame(
        quarter = factor(c("Q2", "Q1"), levels = c("Q2", "Q1")),
        product = c("Product A", "Product B"),
        revenue = c(10, 20),
        stringsAsFactors = FALSE
      )
    }

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(ordered_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      relabeled <- data()$quarter
      expect_true(is.factor(relabeled))
      expect_equal(levels(relabeled), c("Kwartaal 2", "Kwartaal 1"))
      expect_equal(as.character(relabeled), c("Kwartaal 2", "Kwartaal 1"))
    })
  })
})
