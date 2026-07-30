resolve_tc_chart_type <- function(chart_type) {
  if (shiny::is.reactive(chart_type)) {
    return(chart_type())
  }
  chart_type
}

# ---------------------------------------------------------------------------
# Client-side PNG snapshot on "Save as favorite".
#
# Captures whatever Plotly widget is paired with a chart's download module
# (via `plot_output_id`) at the moment it is starred, using Plotly.js's own
# Plotly.toImage() -- i.e. exactly what's on screen, no server-side ggplot
# object needed. The image only leaves the browser once the server confirms
# the favorite was saved and tells the client which favorite id to tag it
# with (session$sendCustomMessage("tc_favorite_saved", ...)), so the PNG on
# disk (state/favorite_assets/<id>.png) always matches a real favorites.json
# entry. Safe no-op when the paired output isn't a Plotly widget (e.g. a
# renderPlot-based chart) or Plotly can't be found.
#
# Emitted on every chart_data_downloads_ui() call that shows the favorite
# button; the `window.__tcFavoriteCaptureInit` guard makes registering the
# (single, delegated) listeners idempotent no matter how many times the
# script tag itself appears in the page.
TC_FAVORITE_CAPTURE_JS <- "
if (!window.__tcFavoriteCaptureInit) {
  window.__tcFavoriteCaptureInit = true;
  var __tcPendingCapture = {};

  function __tcFindPlotlyDiv(outputId) {
    var el = document.getElementById(outputId);
    if (!el) return null;
    if (el.classList && el.classList.contains('js-plotly-plot')) return el;
    return el.querySelector('.js-plotly-plot');
  }

  $(document).on('click', '.tc-favorite-btn', function() {
    var btnId = this.id;
    var wrap = $(this).closest('[data-plot-output-id]');
    var outputId = wrap.length ? wrap.data('plot-output-id') : null;
    var gd = (outputId && typeof Plotly !== 'undefined') ? __tcFindPlotlyDiv(outputId) : null;
    if (!gd) {
      __tcPendingCapture[btnId] = null;
      return;
    }
    var w = (gd._fullLayout && gd._fullLayout.width) || 900;
    var h = (gd._fullLayout && gd._fullLayout.height) || 550;
    __tcPendingCapture[btnId] = Plotly.toImage(gd, {format: 'png', width: w, height: h})
      .catch(function() { return null; });
  });

  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('tc_favorite_saved', function(msg) {
      var p = __tcPendingCapture[msg.btn_id];
      if (typeof p === 'undefined') return;
      delete __tcPendingCapture[msg.btn_id];
      if (p === null) return;
      p.then(function(dataUrl) {
        if (!dataUrl) return;
        var inputId = msg.btn_id.replace(/favorite$/, 'png_capture');
        Shiny.setInputValue(inputId, {fav_id: msg.fav_id, image: dataUrl}, {priority: 'event'});
      });
    });
  });
}
"

#' UI for raw and think-cell chart data downloads.
#'
#' The think-cell button is only shown when `chart_type` is supported.
#'
#' @param id Module id.
#' @param chart_type think-cell chart type for this chart.
#' @param raw_label Download button label for raw data.
#' @param thinkcell_label Download button label for think-cell data.
#' @param favorite_label Label for the "save as favorite" button.
#' @param plot_output_id Optional Shiny output id (top-level, NOT namespaced --
#'   e.g. `"plot_basispopulatie"`) of the Plotly widget this chart's downloads
#'   pair with. When set, starring a favorite also snapshots that widget as a
#'   PNG (see [TC_FAVORITE_CAPTURE_JS]) for inclusion in the favorites deck
#'   ZIP. Omit for charts with no Plotly widget (e.g. `renderPlot()`-based) --
#'   the favorite is still saved, just without an image.
chart_data_downloads_ui <- function(
    id,
    chart_type,
    raw_label = "Download data (raw)",
    thinkcell_label = "Download data (think-cell)",
    slide_label = "Download slide (PowerPoint)",
    favorite_label = "☆ Save as favorite",
    plot_output_id = NULL
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
          shiny::tags$div(
            `data-plot-output-id` = plot_output_id,
            shiny::actionButton(ns("favorite"), favorite_label,
                                class = "btn-default tc-favorite-btn",
                                style = "margin-top:8px;"),
            shiny::uiOutput(ns("favorite_status"))
          ),
          shiny::tags$script(shiny::HTML(TC_FAVORITE_CAPTURE_JS))
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

    # The "Slide template" dropdown's choices are fixed when the UI is built, so
    # a template uploaded at runtime (Manage-templates tab) wouldn't show up
    # without a page reload. Poll the templates dir and refresh the choices when
    # it changes, preserving the user's current selection.
    slide_ui_present <- if (shiny::is.reactive(chart_type)) TRUE else tc_template_available(chart_type)
    if (slide_ui_present) {
      template_choices_poll <- shiny::reactivePoll(
        5000, session,
        checkFunc = function() {
          d  <- tc_find_templates_dir()      # built-in templates
          cd <- tc_custom_templates_dir()    # runtime uploads (state/template_uploads)
          paste(
            if (!is.null(d)  && !is.na(d)  && dir.exists(d))  as.character(file.info(d)$mtime)  else "",
            if (!is.null(cd) && !is.na(cd) && dir.exists(cd)) as.character(file.info(cd)$mtime) else "",
            sep = "|"
          )
        },
        valueFunc = function() tc_template_choices()
      )
      shiny::observeEvent(template_choices_poll(), {
        choices  <- template_choices_poll()
        current  <- shiny::isolate(input$slide_template_choice)
        selected <- if (!is.null(current) && current %in% choices) current else ""
        shiny::updateSelectInput(session, "slide_template_choice",
                                 choices = choices, selected = selected)
      }, ignoreInit = TRUE)
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

          resolved_slide_title  <- resolve_opt(slide_title)
          resolved_figure_title <- resolve_opt(figure_title)
          resolved_template_ovr <- slide_effective_override()
          resolved_slide_order  <- tc_or(input$slide_order, "auto")
          resolved_dashboard    <- tc_ctx_dashboard_title()
          resolved_tab          <- tc_ctx_active_tab()
          resolved_subtab       <- tc_ctx_active_subtab()
          resolved_selections   <- tc_ctx_selections(module_id = id)

          # Auto-log this export to the shared Export history tab (see
          # utils/export_history.R), using exactly these already-resolved
          # values -- not a second, independent re-derivation -- so the
          # history snapshot always matches what's actually downloaded below.
          # Skipped for faceted charts (tc_data is a per-facet list there),
          # same scope limitation favorites_capture() has today.
          chart_id <- NULL
          if (is.null(facet_col) && !is_tc_workbook_list(tc_data)) {
            history_entry <- tc_history_capture(
              tc_data           = tc_data,
              chart_type        = slide_type,
              slide_matrix      = slide_matrix,
              slide_title       = resolved_slide_title,
              figure_title      = resolved_figure_title,
              template_override = resolved_template_ovr,
              slide_order       = resolved_slide_order,
              dashboard_title   = resolved_dashboard,
              tab_label         = resolved_tab,
              subtab_label      = resolved_subtab,
              selections        = resolved_selections,
              filename_prefix   = filename_prefix
            )
            history_entry$id <- export_history_new_id()
            chart_id <- export_history_add(history_entry)
          }

          tc_build_slide_zip(
            zip_path          = file,
            tc_data           = tc_data,
            chart_type        = slide_type,
            slide_matrix      = slide_matrix,
            slide_title       = resolved_slide_title,
            figure_title      = resolved_figure_title,
            dashboard_title   = resolved_dashboard,
            tab_label         = resolved_tab,
            subtab_label      = resolved_subtab,
            selections        = resolved_selections,
            filename_prefix   = filename_prefix,
            template_override = resolved_template_ovr,
            slide_order       = resolved_slide_order,
            chart_id          = chart_id
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
        fav_id <- favorites_add(entry)
        favorite_status_rv(sprintf("Saved '%s' to favorites.", entry$label))
        # Tell the client which favorite this button's click just created, so
        # it can tag the (already-requested, possibly still-in-flight) PNG
        # snapshot with the right id -- see TC_FAVORITE_CAPTURE_JS.
        session$sendCustomMessage(
          "tc_favorite_saved",
          list(btn_id = session$ns("favorite"), fav_id = fav_id)
        )
      })

      # Written by TC_FAVORITE_CAPTURE_JS once the client-side Plotly snapshot
      # resolves. Silently ignored if no plot_output_id was wired, if the
      # paired output isn't a Plotly widget, or if decoding fails -- a missing
      # snapshot just means that favorite's PNG is absent from the deck ZIP,
      # never a broken favorite.
      shiny::observeEvent(input$png_capture, {
        payload <- input$png_capture
        fav_id  <- payload$fav_id
        image   <- payload$image
        if (is.null(fav_id) || is.null(image) || !nzchar(fav_id)) return()
        b64   <- sub("^data:image/[^;]+;base64,", "", image)
        bytes <- tryCatch(jsonlite::base64_dec(b64), error = function(e) NULL)
        if (is.null(bytes)) return()
        dir.create(favorites_assets_dir(), recursive = TRUE, showWarnings = FALSE)
        writeBin(bytes, favorite_asset_path(fav_id))
      }, ignoreInit = TRUE)

      output$favorite_status <- shiny::renderUI({
        shiny::req(favorite_status_rv())
        shiny::tags$p(style = "font-size:12px; color:#065F46; margin-top:4px;",
                      favorite_status_rv())
      })
    }
  })
}
