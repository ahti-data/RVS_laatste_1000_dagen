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
#' @param favorite_label Label for the "save as favorite" button.
chart_data_downloads_ui <- function(
    id,
    chart_type,
    raw_label = "Download data (raw)",
    thinkcell_label = "Download data (think-cell)",
    slide_label = "Download slide (PowerPoint)",
    favorite_label = "☆ Save as favorite"
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

  # The slide (.pptx) download appears whenever a usable template exists for the
  # chart. For reactive chart types we show it and validate at click time.
  show_slide <- if (shiny::is.reactive(chart_type)) {
    TRUE
  } else {
    tc_template_available(chart_type)
  }

  if (show_slide) {
    buttons <- c(
      buttons,
      list(
        shiny::downloadButton(ns("slide"), slide_label, class = "btn-primary"),
        shiny::div(
          class = "tc-slide-template",
          style = "margin-top:8px;",
          shiny::selectInput(
            ns("slide_template_choice"),
            label = "Slide template",
            choices = tc_template_choices(),
            selected = ""
          ),
          shiny::selectInput(
            ns("slide_order"),
            label = "Category order",
            choices = c(
              "Automatic (numeric / as displayed)" = "auto",
              "As displayed"                        = "as_is",
              "Category ascending"                  = "cat_asc",
              "Category descending"                 = "cat_desc",
              "Value ascending"                     = "val_asc",
              "Value descending"                    = "val_desc"
            ),
            selected = "auto"
          ),
          shiny::uiOutput(ns("slide_template_info")),
          shiny::actionButton(ns("favorite"), favorite_label, class = "btn-default",
                              style = "margin-top:8px;"),
          shiny::uiOutput(ns("favorite_status"))
        )
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
    facet_col = NULL,
    slide_title = NULL,
    figure_title = NULL,
    template_override = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    resolve_opt <- function(x) {
      if (is.null(x)) return("")
      if (shiny::is.reactive(x)) return(tc_or(x(), ""))
      if (is.function(x)) return(tc_or(x(), ""))
      x
    }

    # The template that will be used for the slide: the user's manual choice if
    # set, otherwise the one auto-detected from the displayed figure.
    slide_effective_override <- function() {
      ui_choice <- tc_or(input$slide_template_choice, "")
      if (nzchar(ui_choice)) ui_choice else resolve_opt(template_override)
    }

    slide_chosen_template <- shiny::reactive({
      override <- slide_effective_override()
      if (nzchar(override)) {
        path <- tc_template_for_chart_type("", override = override)
        return(list(name = basename(override), source = "manual",
                    available = !is.na(path)))
      }
      rct <- resolve_tc_chart_type(chart_type)
      df  <- tryCatch(data(), error = function(e) NULL)
      slide_type <- if (!is.null(df) && is.null(facet_col)) {
        tryCatch(tc_detect_slide_type(df, rct, category_col, series_col),
                 error = function(e) rct)
      } else {
        rct
      }
      path <- tc_template_for_chart_type(slide_type)
      list(name = if (is.na(path)) NA_character_ else basename(path),
           source = "auto", type = slide_type, available = !is.na(path))
    })

    output$slide_template_info <- shiny::renderUI({
      info <- tryCatch(slide_chosen_template(), error = function(e) NULL)
      if (is.null(info) || is.na(info$name)) {
        return(shiny::tags$div(
          style = "font-size:12px; color:#991B1B;",
          "No matching think-cell template for the current figure."
        ))
      }
      label <- if (identical(info$source, "manual")) {
        "Chosen template (manual): "
      } else {
        "Chosen template (auto): "
      }
      warn <- if (!isTRUE(info$available)) {
        shiny::tags$div(style = "font-size:11px; color:#B45309;",
                        "(file not found in templates/ \u2014 add it to render the slide)")
      } else {
        NULL
      }
      shiny::tags$div(
        style = "font-size:12px; color:#374151;",
        label, shiny::tags$strong(info$name), warn
      )
    })

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

    # PowerPoint slide (+ table + log) ZIP download. Shown for any chart that a
    # think-cell template can match. Builds the slide from the same data that
    # backs the table download, so the two never disagree.
    register_slide <- if (shiny::is.reactive(chart_type)) {
      TRUE
    } else {
      tc_template_available(chart_type)
    }

    if (register_slide) {
      output$slide <- shiny::downloadHandler(
        filename = function() {
          paste0(filename_prefix, "_slide_", Sys.Date(), ".zip")
        },
        content = function(file) {
          resolved_chart_type <- resolve_tc_chart_type(chart_type)

          # Underlying table only makes sense for think-cell-supported types.
          if (!is_tc_chart_type_supported(resolved_chart_type)) {
            shiny::showNotification(
              paste0("No think-cell export is available for this chart (type: ",
                     resolved_chart_type, ")."),
              type = "warning"
            )
          }

          tc_data <- tryCatch(
            format_tc_data(
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
            ),
            error = function(e) NULL
          )

          if (is.null(tc_data)) {
            # Fall back to the raw data so the ZIP is still useful.
            tc_data <- data()
          }

          if (!tc_template_available(resolved_chart_type)) {
            shiny::showNotification(
              paste0("No suitable think-cell template for the displayed chart ",
                     "(type: ", resolved_chart_type, "). ",
                     "The ZIP contains the data table and an explanation, but no slide."),
              type = "warning", duration = 8
            )
          }

          # Determine the template that matches the *displayed* figure from the
          # actual data (e.g. a grouped/stacked bar with one series -> plain bar),
          # and build the slide matrix in the templates' expected orientation.
          slide_type   <- resolved_chart_type
          slide_matrix <- NULL
          if (is.null(facet_col)) {
            prep <- tryCatch(
              tc_prepare_slide(
                df = data(),
                chart_type = resolved_chart_type,
                category_col = category_col,
                series_col = series_col,
                value_col = value_col,
                agg_fun = agg_fun,
                category_order = category_order,
                series_order = series_order
              ),
              error = function(e) NULL
            )
            if (!is.null(prep)) {
              slide_type   <- prep$chart_type
              slide_matrix <- prep$matrix
            }
          }

          tc_build_slide_zip(
            zip_path          = file,
            tc_data           = tc_data,
            chart_type        = slide_type,
            slide_matrix      = slide_matrix,
            slide_title       = resolve_opt(slide_title),
            figure_title      = resolve_opt(figure_title),
            dashboard_title   = tc_ctx_dashboard_title(),
            tab_label         = tc_ctx_active_tab(),
            subtab_label      = tc_ctx_active_subtab(),
            selections        = tc_ctx_selections(module_id = id),
            filename_prefix   = filename_prefix,
            template_override = slide_effective_override(),
            slide_order       = tc_or(input$slide_order, "auto")
          )
        }
      )

      favorite_status_rv <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$favorite, {
        entry <- favorites_capture(
          data              = data(),
          chart_type        = resolve_tc_chart_type(chart_type),
          category_col      = category_col,
          series_col        = series_col,
          value_col         = value_col,
          agg_fun           = agg_fun,
          category_order    = category_order,
          series_order      = series_order,
          facet_col         = facet_col,
          slide_title       = resolve_opt(slide_title),
          figure_title      = resolve_opt(figure_title),
          template_override = slide_effective_override(),
          slide_order       = tc_or(input$slide_order, "auto"),
          dashboard_title   = tc_ctx_dashboard_title(),
          tab_label         = tc_ctx_active_tab(),
          subtab_label      = tc_ctx_active_subtab(),
          selections        = tc_ctx_selections(module_id = id),
          filename_prefix   = filename_prefix
        )
        favorites_add(entry)
        favorite_status_rv(sprintf("Saved '%s' to favorites.", entry$label))
      })

      output$favorite_status <- shiny::renderUI({
        shiny::req(favorite_status_rv())
        shiny::tags$p(style = "font-size:12px; color:#065F46; margin-top:4px;",
                      favorite_status_rv())
      })
    }
  })
}
