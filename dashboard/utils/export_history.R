#' Export history: an automatic, durable log of every "Download slide" click.
#'
#' Distinct from `utils/favorites.R` (a manually curated, shared shortlist):
#' every slide download is logged here automatically, with a frozen snapshot
#' of exactly what was exported, so a PM can find and redownload a chart from
#' a long time ago -- byte-for-byte the same ZIP -- without needing to have
#' kept the original around. Each entry gets a short chart id that also gets
#' embedded in the exported `.ppttc`/log (see `tc_build_ppttc_slide_block()`
#' and `tc_build_log()` in `utils/slide_download.R`), so a chart spotted in a
#' real PowerPoint deck can be traced back here.
#'
#' Phase 1 only: exact-snapshot redownload. Regenerating a chart against
#' *today's* data (with drift detection if the shape has changed too much)
#' is a deliberately deferred follow-up -- it needs each chart's data
#' computation refactored into a plain function callable outside its own
#' reactive, which is a larger, separate change.

#' One JSON file per entry (`state/export_history/<id>.json`) rather than one
#' growing array, so appending never rewrites the whole log -- same reasoning
#' as `state/template_uploads/`. Override with `SHINY_EXPORT_HISTORY_DIR`.
export_history_dir <- function() {
  Sys.getenv("SHINY_EXPORT_HISTORY_DIR", file.path("state", "export_history"))
}

export_history_new_id <- function() {
  paste0(
    "exp_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
    paste(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = "")
  )
}

#' Save a new history entry.
#'
#' @param entry List, typically the output of [tc_history_capture()]. If
#'   `entry$id` is already set (so the same id can be embedded in the export
#'   itself -- see `utils/chart_downloads.R`), it's kept as-is; otherwise one
#'   is generated.
#' @return The entry's id (invisibly).
export_history_add <- function(entry) {
  if (is.null(entry$id) || !nzchar(entry$id)) {
    entry$id <- export_history_new_id()
  }
  entry$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dir <- export_history_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    entry, file.path(dir, paste0(entry$id, ".json")),
    auto_unbox = TRUE, null = "null", na = "null"
  )
  invisible(entry$id)
}

#' Read a single history entry by id.
#' @return The entry (a list), or `NULL` if missing/corrupt.
export_history_get <- function(id) {
  path <- file.path(export_history_dir(), paste0(id, ".json"))
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

#' List every history entry, most recently created first.
#'
#' Reads every `*.json` file in [export_history_dir()]; unreadable/corrupt
#' files are skipped rather than failing the whole list (the same tolerance
#' [favorites_list()] applies to its single file).
#' @return List of entries (possibly empty).
export_history_list <- function() {
  dir <- export_history_dir()
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0) return(list())
  entries <- lapply(files, function(f) {
    tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  })
  entries <- Filter(Negate(is.null), entries)
  created <- vapply(entries, function(e) tc_or(e$created_at, ""), character(1))
  entries[order(created, decreasing = TRUE)]
}

#' Remove a history entry by id.
export_history_remove <- function(id) {
  path <- file.path(export_history_dir(), paste0(id, ".json"))
  if (file.exists(path)) unlink(path)
  invisible(TRUE)
}

#' Package an already-resolved slide export into a storable history entry.
#'
#' Takes the *same resolved values* the "Download slide" handler in
#' `utils/chart_downloads.R` already computed for its own [tc_build_slide_zip()]
#' call -- not a second, independent re-derivation -- so a history entry
#' always matches exactly what was actually downloaded. Faceted charts
#' (`tc_data` is a per-facet named list, not one matrix) are the caller's
#' responsibility to skip; this function assumes a single matrix.
#'
#' @param tc_data The resolved think-cell matrix (from [format_tc_data()]) --
#'   what the zip's own `<prefix>_table.xlsx` is built from.
#' @param chart_type The *resolved* slide chart type (e.g. from
#'   [tc_prepare_slide()]), not necessarily the chart's declared type.
#' @param slide_matrix Optional pre-oriented matrix for the slide/`.ppttc`
#'   itself, when it differs from `tc_data` (see [tc_build_slide_zip()]).
#' @param slide_title,figure_title Resolved slide text.
#' @param template_override The resolved template choice (manual or
#'   auto-detected name/path) actually used, so a later redownload reproduces
#'   the same template rather than re-running auto-detection against
#'   whatever exists at that time.
#' @param slide_order Resolved category order mode.
#' @param dashboard_title,tab_label,subtab_label,selections Export log metadata.
#' @param source_output,source_sheet Optional data-source identifiers (see
#'   `tc_build_datasheet_log()` in `utils/slide_download.R`), stored on the
#'   entry so a redownload stamps the same values into the datasheet corner
#'   cell as the original export did.
#' @param filename_prefix Prefix used for this chart's downloads.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @return A list ready for [export_history_add()].
tc_history_capture <- function(
    tc_data, chart_type, slide_matrix = NULL,
    slide_title = "", figure_title = "", template_override = "", slide_order = "auto",
    dashboard_title = "", tab_label = "", subtab_label = "",
    selections = NULL, source_output = NULL, source_sheet = NULL,
    filename_prefix = "chart", templates_dir = NULL
) {
  override <- if (nzchar(tc_or(template_override, ""))) template_override else NULL
  template_path <- tc_template_for_chart_type(chart_type, templates_dir = templates_dir, override = override)

  resolved_label <- Find(function(x) !is.null(x) && nzchar(x), list(subtab_label, filename_prefix))

  list(
    label             = tc_or(resolved_label, "chart"),
    filename_prefix   = filename_prefix,
    dashboard_title   = dashboard_title,
    tab_label         = tab_label,
    subtab_label      = subtab_label,
    chart_type        = chart_type,
    template_name     = if (!is.na(template_path)) basename(template_path) else NA_character_,
    template_override = tc_or(template_override, ""),
    selections        = selections,
    source_output     = source_output,
    source_sheet      = source_sheet,
    slide_order       = slide_order,
    slide_title       = slide_title,
    figure_title      = figure_title,
    tc_data_table     = favorites_table_to_storage(tc_data),
    slide_matrix_table = if (!is.null(slide_matrix)) favorites_table_to_storage(slide_matrix) else NULL
  )
}

#' Rebuild the exact original ZIP for a history entry.
#' @param entry A history entry (as returned by [export_history_get()]).
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param templates_dir,ppttc_exe Optional overrides (mainly for tests).
export_history_redownload <- function(entry, zip_path, templates_dir = NULL, ppttc_exe = NULL) {
  tc_build_slide_zip(
    zip_path          = zip_path,
    tc_data           = favorites_table_as_df(entry$tc_data_table),
    chart_type        = entry$chart_type,
    slide_matrix      = if (!is.null(entry$slide_matrix_table)) {
      favorites_table_as_df(entry$slide_matrix_table)
    } else {
      NULL
    },
    slide_title       = tc_or(entry$slide_title, ""),
    figure_title      = tc_or(entry$figure_title, ""),
    dashboard_title   = tc_or(entry$dashboard_title, ""),
    tab_label         = tc_or(entry$tab_label, ""),
    subtab_label      = tc_or(entry$subtab_label, ""),
    selections        = entry$selections,
    source_output     = tc_or(entry$source_output, ""),
    source_sheet      = tc_or(entry$source_sheet, ""),
    filename_prefix   = tc_or(entry$filename_prefix, "chart"),
    templates_dir     = templates_dir,
    template_override = tc_or(entry$template_override, ""),
    ppttc_exe         = ppttc_exe,
    slide_order       = tc_or(entry$slide_order, "auto"),
    chart_id          = entry$id
  )
}

#' Compact breadcrumb + chart-type/template line for one history row.
tc_history_entry_subtitle <- function(e) {
  breadcrumb <- paste(
    Filter(nzchar, c(tc_or(e$dashboard_title, ""), tc_or(e$tab_label, ""), tc_or(e$subtab_label, ""))),
    collapse = " / "
  )
  breadcrumb
}

#' UI for the shared "Export history" tab.
#' @param id Module id.
export_history_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Export history"),
    shiny::p(
      class = "text-muted",
      "Every “Download slide” click is logged here automatically, so a chart from a ",
      "while back can be redownloaded exactly as it was — no need to keep the original ZIP ",
      "around. Find a chart's ID on its rendered slide or in a bundle's log.txt, then look it ",
      "up below."
    ),
    shiny::textInput(
      ns("search"), NULL,
      placeholder = "Search by chart id, dashboard, tab, sub-tab, or chart type..."
    ),
    shiny::uiOutput(ns("list"))
  )
}

#' Server logic for the shared "Export history" tab.
#'
#' Uses `reactivePoll()` over the history directory (max modification time +
#' file count) so the list picks up new entries logged from any chart's
#' module server, the same pattern `favorites_panel_server()` uses.
#' @param id Module id.
#' @param poll_interval_ms How often to check the history directory for changes.
#' @param display_limit Cap on entries shown when there's no search filter
#'   (most-recent-first); a search always shows every match, unfiltered.
export_history_panel_server <- function(id, poll_interval_ms = 2000, display_limit = 50) {
  shiny::moduleServer(id, function(input, output, session) {
    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        dir <- export_history_dir()
        if (!dir.exists(dir)) return("0|")
        files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
        if (length(files) == 0) return("0|")
        paste(length(files), max(file.info(files)$mtime), sep = "|")
      },
      valueFunc = export_history_list
    )

    filtered_entries <- shiny::reactive({
      entries <- entries_reactive()
      query <- trimws(tc_or(input$search, ""))
      if (nzchar(query)) {
        keep <- vapply(entries, function(e) {
          haystack <- paste(
            tc_or(e$id, ""), tc_or(e$label, ""), tc_or(e$dashboard_title, ""),
            tc_or(e$tab_label, ""), tc_or(e$subtab_label, ""), tc_or(e$chart_type, ""),
            sep = " | "
          )
          grepl(query, haystack, ignore.case = TRUE, fixed = TRUE)
        }, logical(1))
        entries[keep]
      } else if (length(entries) > display_limit) {
        entries[seq_len(display_limit)]
      } else {
        entries
      }
    })

    output$list <- shiny::renderUI({
      entries <- filtered_entries()
      total   <- length(entries_reactive())
      if (total == 0) {
        return(shiny::tags$p(class = "text-muted", "No exports logged yet."))
      }
      if (length(entries) == 0) {
        return(shiny::tags$p(class = "text-muted", "No exports match that search."))
      }

      cap_note <- if (!nzchar(trimws(tc_or(input$search, ""))) && total > display_limit) {
        shiny::tags$p(
          class = "text-muted", style = "font-size:12px;",
          sprintf("Showing the %d most recent of %d exports. Search to reach further back.",
                  display_limit, total)
        )
      } else {
        NULL
      }

      rows <- lapply(entries, function(e) {
        shiny::tags$div(
          style = paste(
            "display:flex; justify-content:space-between; align-items:flex-start;",
            "gap:12px; padding:8px 0; border-bottom:1px solid #eee;"
          ),
          shiny::tags$div(
            shiny::tags$strong(tc_or(e$label, "(untitled)")),
            shiny::tags$code(style = "font-size:11px; margin-left:8px; color:#6B7280;", tc_or(e$id, "")),
            shiny::tags$div(
              style = "font-size:12px; color:#6B7280;",
              tc_history_entry_subtitle(e)
            ),
            shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              paste(
                Filter(nzchar, c(tc_or(e$chart_type, ""), tc_or(e$template_name, ""), tc_or(e$created_at, ""))),
                collapse = " · "
              )
            )
          ),
          shiny::downloadButton(
            session$ns(paste0("redownload_", e$id)), "Redownload",
            class = "btn-default btn-sm"
          )
        )
      })
      do.call(shiny::tagList, c(list(cap_note), rows))
    })

    # (Re)register one download handler per currently-listed entry, mirroring
    # favorites_panel_server()'s equivalent pattern for its remove buttons.
    # History entries are immutable once written, so re-registering on every
    # poll tick is harmless -- just re-reads the same on-disk snapshot.
    shiny::observe({
      entries <- entries_reactive()
      lapply(entries, function(e) {
        local({
          entry <- e
          output[[paste0("redownload_", entry$id)]] <- shiny::downloadHandler(
            filename = function() {
              paste0(tc_or(entry$filename_prefix, "chart"), "_slide_", Sys.Date(), ".zip")
            },
            content = function(file) {
              export_history_redownload(entry, file)
            }
          )
        })
      })
    })
  })
}
