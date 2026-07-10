resolve_tc_chart_type <- function(chart_type) {
  if (shiny::is.reactive(chart_type)) {
    return(chart_type())
  }
  chart_type
}

#' UI for raw and think-cell chart data downloads.
#'
#' The think-cell button is only shown when `chart_type` is supported.
#'
#' @param id Module id.
#' @param chart_type think-cell chart type for this chart.
#' @param raw_label Download button label for raw data.
#' @param thinkcell_label Download button label for think-cell data.
chart_data_downloads_ui <- function(
    id,
    chart_type,
    raw_label = "Download data (raw)",
    thinkcell_label = "Download data (think-cell)"
) {
  ns <- shiny::NS(id)

  buttons <- list(
    shiny::downloadButton(ns("raw"), raw_label, class = "btn-default")
  )

  show_thinkcell <- if (shiny::is.reactive(chart_type)) {
    TRUE
  } else {
    is_tc_chart_type_supported(chart_type)
  }

  if (show_thinkcell) {
    buttons <- c(
      buttons,
      list(
        shiny::downloadButton(ns("thinkcell"), thinkcell_label, class = "btn-primary")
      )
    )
  }

  do.call(shiny::tagList, buttons)
}

#' Server logic for raw and think-cell chart data downloads.
#'
#' @param id Module id.
#' @param data Reactive returning the exact data frame used to build the ggplot.
#' @param chart_type think-cell chart type. The think-cell handler is registered
#'   only when this type is supported. May be a reactive for dynamic chart types.
#' @param category_col,series_col,value_col Column names for think-cell export.
#' @param filename_prefix Prefix for downloaded file names.
#' @param agg_fun Aggregation function passed to [format_tc_data()].
#' @param category_order,series_order Optional order vectors for think-cell export.
#' @param waterfall_end_col,waterfall_subtotal_cols Optional waterfall markers.
#' @param facet_col Optional facet column for `facet_wrap()` / `facet_grid()` plots.
chart_data_downloads_server <- function(
    id,
    data,
    chart_type,
    category_col,
    series_col,
    value_col,
    filename_prefix = "chart_data",
    agg_fun = NULL,
    category_order = NULL,
    series_order = NULL,
    waterfall_end_col = NULL,
    waterfall_subtotal_cols = NULL,
    facet_col = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    output$raw <- shiny::downloadHandler(
      filename = function() {
        paste0(filename_prefix, "_raw_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write_tc_xlsx(data(), file)
      }
    )

    register_thinkcell <- if (shiny::is.reactive(chart_type)) {
      TRUE
    } else {
      is_tc_chart_type_supported(chart_type)
    }

    if (register_thinkcell) {
      output$thinkcell <- shiny::downloadHandler(
        filename = function() {
          paste0(filename_prefix, "_thinkcell_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          resolved_chart_type <- resolve_tc_chart_type(chart_type)
          if (!is_tc_chart_type_supported(resolved_chart_type)) {
            stop("Think-cell export is not supported for chart type: ", resolved_chart_type)
          }

          tc_data <- format_tc_data(
            df = data(),
            chart_type = resolved_chart_type,
            category_col = category_col,
            series_col = series_col,
            value_col = value_col,
            agg_fun = agg_fun,
            category_order = category_order,
            series_order = series_order,
            waterfall_end_col = waterfall_end_col,
            waterfall_subtotal_cols = waterfall_subtotal_cols,
            facet_col = facet_col
          )
          write_tc_xlsx(tc_data, file)
        }
      )
    }
  })
}
