#' Supported think-cell chart types for dashboard exports.
#'
#' Grouped and stacked bar charts share the same Excel layout in think-cell;
#' the distinction is made in PowerPoint, not in the data matrix.
TC_SUPPORTED_CHART_TYPES <- c(
  "line",
  "bar",
  "stacked_bar",
  "grouped_bar",
  "waterfall"
)

#' Check whether a chart type has think-cell export support.
#'
#' @param chart_type Character chart type identifier.
#' @return Logical scalar.
is_tc_chart_type_supported <- function(chart_type) {
  chart_type %in% TC_SUPPORTED_CHART_TYPES
}

#' Chart types that use a transposed matrix (bar orientation).
#'
#' think-cell column/line charts: rows = series, columns = categories.
#' think-cell bar charts: categories in column 1, series across the header row.
tc_chart_types_transposed <- function() {
  c("bar", "stacked_bar", "grouped_bar")
}

#' Reshape plot data into a think-cell-friendly Excel matrix.
#'
#' @param df Data frame used to build the ggplot (after filtering/aggregation).
#' @param chart_type One of [TC_SUPPORTED_CHART_TYPES] or legacy aliases
#'   (`standaard`, `stacked_column`).
#' @param category_col Column mapped to ggplot `x` (categories).
#' @param series_col Column mapped to ggplot `fill`, `color`, or `group`.
#' @param value_col Column mapped to ggplot `y` (numeric values).
#' @param agg_fun Aggregation function for duplicate category/series pairs.
#'   Set to `NULL` to require unique pairs and skip aggregation.
#' @param category_order Optional character vector to fix category column order.
#' @param series_order Optional character vector to fix series row order.
#' @param waterfall_end_col Optional category name for waterfall end total (`e`).
#' @param waterfall_subtotal_cols Optional category names for waterfall subtotals (`t`).
#' @param facet_col Optional column used by `facet_wrap()` / `facet_grid()`. When
#'   set, returns a named list of matrices (one per facet level) for multi-sheet export.
#'
#' @return Tibble ready for Excel export, or a named list of tibbles when `facet_col`
#'   is set. Cell A1 is empty (first column header `""`).
format_tc_data <- function(
    df,
    chart_type = "line",
    category_col,
    series_col,
    value_col,
    agg_fun = mean,
    category_order = NULL,
    series_order = NULL,
    waterfall_end_col = NULL,
    waterfall_subtotal_cols = NULL,
    facet_col = NULL
) {
  chart_type <- normalize_tc_chart_type(chart_type)

  if (!is_tc_chart_type_supported(chart_type)) {
    stop(
      "Unsupported think-cell chart type: ", chart_type,
      ". Supported types: ", paste(TC_SUPPORTED_CHART_TYPES, collapse = ", ")
    )
  }

  validate_tc_columns(df, category_col, series_col, value_col, facet_col = facet_col)

  if (!is.null(facet_col)) {
    facet_levels <- tc_facet_levels(df[[facet_col]])
    tc_by_facet <- stats::setNames(
      lapply(facet_levels, function(facet_level) {
        facet_df <- df[df[[facet_col]] == facet_level, , drop = FALSE]
        format_tc_data(
          df = facet_df,
          chart_type = chart_type,
          category_col = category_col,
          series_col = series_col,
          value_col = value_col,
          agg_fun = agg_fun,
          category_order = category_order,
          series_order = series_order,
          waterfall_end_col = waterfall_end_col,
          waterfall_subtotal_cols = waterfall_subtotal_cols,
          facet_col = NULL
        )
      }),
      facet_levels
    )

    names(tc_by_facet) <- sanitize_excel_sheet_names(names(tc_by_facet))
    return(tc_by_facet)
  }

  if (chart_type == "waterfall") {
    return(format_tc_waterfall(
      df = df,
      category_col = category_col,
      series_col = series_col,
      value_col = value_col,
      agg_fun = agg_fun,
      category_order = category_order,
      waterfall_end_col = waterfall_end_col,
      waterfall_subtotal_cols = waterfall_subtotal_cols
    ))
  }

  df_clean <- prepare_tc_long_data(
    df = df,
    category_col = category_col,
    series_col = series_col,
    value_col = value_col,
    agg_fun = agg_fun,
    category_order = category_order,
    series_order = series_order
  )

  tc_matrix <- df_clean %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(category_col),
      values_from = tc_value
    )

  tc_matrix <- apply_tc_matrix_layout(
    tc_matrix = tc_matrix,
    series_col = series_col,
    transpose = chart_type %in% tc_chart_types_transposed()
  )

  tc_matrix
}

#' Write a data frame or matrix to an Excel workbook.
#'
#' @param data Data frame, matrix, or named list of data frames (one sheet each).
#' @param path Output `.xlsx` path.
write_tc_xlsx <- function(data, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }

  if (is_tc_workbook_list(data)) {
    sheets <- lapply(data, as.data.frame)
    writexl::write_xlsx(sheets, path)
    return(invisible(path))
  }

  writexl::write_xlsx(as.data.frame(data), path)
}

normalize_tc_chart_type <- function(chart_type) {
  switch(
    chart_type,
    standaard = "line",
    stacked_column = "line",
    area = "line",
    chart_type
  )
}

validate_tc_columns <- function(
    df,
    category_col,
    series_col,
    value_col,
    facet_col = NULL
) {
  required_cols <- c(category_col, series_col, value_col, facet_col)
  required_cols <- required_cols[!is.null(required_cols)]
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
}

is_tc_workbook_list <- function(data) {
  is.list(data) &&
    !is.data.frame(data) &&
    length(data) > 0 &&
    !is.null(names(data)) &&
    all(vapply(data, function(x) is.data.frame(x) || is.matrix(x), logical(1)))
}

tc_facet_levels <- function(facet_values) {
  if (is.factor(facet_values)) {
    present_levels <- levels(facet_values)[levels(facet_values) %in% facet_values]
    return(present_levels)
  }

  unique(facet_values)
}

sanitize_excel_sheet_name <- function(name) {
  name <- as.character(name)
  name <- gsub("[\\\\/:?*\\[\\]]", "_", name)
  name <- substr(name, 1, 31)
  if (!nzchar(name)) {
    return("sheet")
  }
  name
}

sanitize_excel_sheet_names <- function(names) {
  sanitized <- vapply(names, sanitize_excel_sheet_name, character(1))

  if (!any(duplicated(sanitized))) {
    return(unname(sanitized))
  }

  seen <- character(0)
  vapply(seq_along(sanitized), function(i) {
  name <- sanitized[[i]]
  count <- sum(seen == name) + 1L
  seen <<- c(seen, name)
  if (count == 1L) {
    return(name)
  }

  suffix <- paste0("_", count)
  base <- substr(name, 1, max(1L, 31L - nchar(suffix)))
  paste0(base, suffix)
  }, character(1))
}

prepare_tc_long_data <- function(
    df,
    category_col,
    series_col,
    value_col,
    agg_fun = mean,
    category_order = NULL,
    series_order = NULL
) {
  df_clean <- df %>%
    dplyr::select(
      dplyr::all_of(c(category_col, series_col, value_col))
    )

  if (is.null(agg_fun)) {
    key_cols <- df_clean[, c(category_col, series_col), drop = FALSE]
    if (any(duplicated(key_cols))) {
      stop(
        "Duplicate category/series pairs found. ",
        "Aggregate the plot data first or provide agg_fun."
      )
    }

    df_clean <- df_clean %>%
      dplyr::transmute(
        !!rlang::sym(category_col) := !!rlang::sym(category_col),
        !!rlang::sym(series_col) := !!rlang::sym(series_col),
        tc_value = !!rlang::sym(value_col)
      )
  } else {
    df_clean <- df_clean %>%
      dplyr::group_by(
        !!rlang::sym(category_col),
        !!rlang::sym(series_col)
      ) %>%
      dplyr::summarise(
        tc_value = agg_fun(!!rlang::sym(value_col), na.rm = TRUE),
        .groups = "drop"
      )
  }

  if (!is.null(category_order)) {
    df_clean[[category_col]] <- factor(df_clean[[category_col]], levels = category_order)
    df_clean <- df_clean %>% dplyr::arrange(!!rlang::sym(category_col))
    df_clean[[category_col]] <- as.character(df_clean[[category_col]])
  } else if (is.numeric(df_clean[[category_col]])) {
    df_clean <- df_clean %>% dplyr::arrange(!!rlang::sym(category_col))
  }

  if (!is.null(series_order)) {
    df_clean[[series_col]] <- factor(df_clean[[series_col]], levels = series_order)
    df_clean <- df_clean %>% dplyr::arrange(!!rlang::sym(series_col))
    df_clean[[series_col]] <- as.character(df_clean[[series_col]])
  }

  df_clean
}

apply_tc_matrix_layout <- function(tc_matrix, series_col, transpose = FALSE) {
  if (transpose) {
    category_names <- colnames(tc_matrix)[colnames(tc_matrix) != series_col]
    series_names <- tc_matrix[[series_col]]
    value_matrix <- as.matrix(tc_matrix[, category_names, drop = FALSE])
    rownames(value_matrix) <- series_names

    transposed <- t(value_matrix)
    tc_matrix <- tibble::as_tibble(transposed, rownames = "category")
    colnames(tc_matrix)[1] <- ""
    return(tc_matrix)
  }

  tc_matrix <- dplyr::rename(tc_matrix, tc_series = !!rlang::sym(series_col))
  colnames(tc_matrix)[colnames(tc_matrix) == "tc_series"] <- ""
  tc_matrix
}

format_tc_waterfall <- function(
    df,
    category_col,
    series_col,
    value_col,
    agg_fun = mean,
    category_order = NULL,
    waterfall_end_col = NULL,
    waterfall_subtotal_cols = NULL
) {
  df_clean <- prepare_tc_long_data(
    df = df,
    category_col = category_col,
    series_col = series_col,
    value_col = value_col,
    agg_fun = agg_fun,
    category_order = category_order,
    series_order = NULL
  )

  if (dplyr::n_distinct(df_clean[[series_col]]) > 1) {
    warning(
      "Waterfall export contains multiple series. ",
      "Using the first series only; validate against your think-cell template."
    )
    first_series <- df_clean[[series_col]][1]
    df_clean <- df_clean %>%
      dplyr::filter(!!rlang::sym(series_col) == first_series)
  }

  values <- df_clean$tc_value
  names(values) <- df_clean[[category_col]]

  if (!is.null(waterfall_subtotal_cols)) {
    values[waterfall_subtotal_cols] <- paste0("t|", values[waterfall_subtotal_cols])
  }

  if (!is.null(waterfall_end_col)) {
    if (!waterfall_end_col %in% names(values)) {
      stop("waterfall_end_col '", waterfall_end_col, "' not found in category columns.")
    }
    values[[waterfall_end_col]] <- paste0("e|", values[[waterfall_end_col]])
  }

  tc_matrix <- tibble::tibble(tc_series = "Series 1")
  for (category_name in names(values)) {
    tc_matrix[[category_name]] <- values[[category_name]]
  }
  colnames(tc_matrix)[colnames(tc_matrix) == "tc_series"] <- ""

  tc_matrix
}
