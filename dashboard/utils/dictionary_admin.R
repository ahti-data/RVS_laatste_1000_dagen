#' Dictionary tab UI, decoupled from git deploys -- lets anyone add or fix a
#' raw-name -> pretty-label mapping (see `utils/dictionary.R`) without a
#' developer editing code and redeploying. Follows the same
#' `*_ui(id)`/`*_server(id)` module shape as `utils/template_admin.R`, and
#' the same list/modal conventions as `favorites_panel_server()` in
#' `utils/favorites.R`.

#' A safe, stable HTML-id fragment for one `(raw_key, scope)` pair, so each
#' row's "Edit" button gets a distinct, reusable input id across re-renders.
dict_entry_ui_id <- function(raw_key, scope) {
  raw <- jsonlite::base64_enc(charToRaw(paste0(tc_or(raw_key, ""), "", tc_or(scope, ""))))
  paste0("row_", gsub("[^A-Za-z0-9]", "", raw))
}

#' UI for the Dictionary admin panel.
#' @param id Module id.
dictionary_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Dictionary"),
    shiny::p(
      class = "text-muted",
      "Maps a raw name from the underlying data source (e.g. \"zvwktotaal\") to the ",
      "pretty label shown on chart bars and in every download instead (e.g. \"Totale ",
      "ZVW kosten\"). Prefilled automatically where possible -- add or fix an entry below."
    ),
    shiny::fluidRow(
      shiny::column(width = 4, shiny::actionButton(ns("add"), "Add entry", class = "btn-primary")),
      shiny::column(
        width = 8,
        shiny::textInput(ns("search"), NULL, placeholder = "Search raw name, scope, or label...", width = "100%")
      )
    ),
    shiny::uiOutput(ns("status")),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list"))
  )
}

#' Server logic for the Dictionary admin panel.
#' @param id Module id.
#' @param poll_interval_ms How often to check the dictionary file for
#'   changes made by another session, so edits show up everywhere without
#'   any direct wiring between modules (same pattern as
#'   `favorites_panel_server()`).
dictionary_admin_server <- function(id, poll_interval_ms = 2000) {
  shiny::moduleServer(id, function(input, output, session) {
    status_rv <- shiny::reactiveValues(message = NULL, ok = NA)
    editing <- shiny::reactiveVal(NULL)

    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        path <- dictionary_path()
        if (file.exists(path)) as.character(file.info(path)$mtime) else ""
      },
      valueFunc = dictionary_list
    )

    filtered_entries <- shiny::reactive({
      entries <- entries_reactive()
      q <- trimws(tolower(tc_or(input$search, "")))
      if (!nzchar(q)) return(entries)
      Filter(function(e) {
        hay <- tolower(paste(tc_or(e$raw_key, ""), tc_or(e$scope, ""), tc_or(e$pretty_label, "")))
        grepl(q, hay, fixed = TRUE)
      }, entries)
    })

    open_editor <- function(entry = NULL) {
      is_new <- is.null(entry)
      editing(entry)
      shiny::showModal(shiny::modalDialog(
        title = if (is_new) "Add dictionary entry" else "Edit dictionary entry",
        if (is_new) {
          shiny::tagList(
            shiny::textInput(session$ns("edit_raw_key"), "Raw name (as it appears in the data source)"),
            shiny::textInput(
              session$ns("edit_scope"), "Scope (optional)",
              placeholder = "e.g. a column name -- only needed if the same raw name means different things in different places"
            )
          )
        } else {
          shiny::tagList(
            shiny::tags$p(shiny::tags$strong("Raw name: "), tc_or(entry$raw_key, "")),
            shiny::tags$p(shiny::tags$strong("Scope: "), if (nzchar(tc_or(entry$scope, ""))) entry$scope else shiny::tags$em("(none)"))
          )
        },
        shiny::textInput(session$ns("edit_pretty_label"), "Pretty label", value = tc_or(entry$pretty_label, "")),
        footer = shiny::tagList(
          if (!is_new) shiny::actionButton(session$ns("delete"), "Delete", class = "btn-danger"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("save"), "Save", class = "btn-primary")
        )
      ))
    }

    shiny::observeEvent(input$add, open_editor(NULL), ignoreInit = TRUE)

    shiny::observe({
      entries <- filtered_entries()
      lapply(entries, function(e) {
        btn_id <- paste0("edit_", dict_entry_ui_id(e$raw_key, e$scope))
        shiny::observeEvent(input[[btn_id]], {
          open_editor(e)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    shiny::observeEvent(input$save, {
      current <- editing()
      is_new <- is.null(current)
      raw_key <- if (is_new) trimws(tc_or(input$edit_raw_key, "")) else current$raw_key
      scope <- if (is_new) trimws(tc_or(input$edit_scope, "")) else tc_or(current$scope, "")
      pretty_label <- trimws(tc_or(input$edit_pretty_label, ""))

      if (!nzchar(raw_key) || !nzchar(pretty_label)) {
        status_rv$message <- "Both the raw name and the pretty label are required."
        status_rv$ok <- FALSE
        return(invisible(NULL))
      }

      dictionary_set_entry(raw_key, scope, pretty_label)
      status_rv$message <- sprintf("Saved '%s' -> '%s'.", raw_key, pretty_label)
      status_rv$ok <- TRUE
      shiny::removeModal()
    })

    shiny::observeEvent(input$delete, {
      current <- editing()
      shiny::req(current)
      dictionary_remove_entry(current$raw_key, tc_or(current$scope, ""))
      status_rv$message <- sprintf("Removed '%s'.", current$raw_key)
      status_rv$ok <- TRUE
      shiny::removeModal()
    })

    output$status <- shiny::renderUI({
      shiny::req(status_rv$message)
      cls <- if (isTRUE(status_rv$ok)) "text-success" else "text-danger"
      shiny::tags$p(class = cls, status_rv$message)
    })

    output$list <- shiny::renderUI({
      entries <- filtered_entries()
      if (length(entries) == 0) {
        return(shiny::tags$p(class = "text-muted", "No dictionary entries yet."))
      }
      rows <- lapply(entries, function(e) {
        btn_id <- paste0("edit_", dict_entry_ui_id(e$raw_key, e$scope))
        shiny::tags$div(
          style = paste(
            "display:flex; justify-content:space-between; align-items:center;",
            "gap:12px; padding:8px 0; border-bottom:1px solid #eee;"
          ),
          shiny::tags$div(
            shiny::tags$code(tc_or(e$raw_key, "")),
            if (nzchar(tc_or(e$scope, ""))) shiny::tags$span(
              style = "font-size:11px; color:#9CA3AF; margin-left:6px;",
              paste0("scope: ", e$scope)
            ),
            shiny::tags$div(style = "margin-top:2px;", tc_or(e$pretty_label, ""))
          ),
          shiny::actionButton(session$ns(btn_id), "Edit", class = "btn-default btn-sm")
        )
      })
      do.call(shiny::tagList, rows)
    })

    invisible(NULL)
  })
}
