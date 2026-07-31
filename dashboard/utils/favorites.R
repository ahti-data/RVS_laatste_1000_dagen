#' Shared favorites: star a chart's current export, revisit it later, and
#' download every starred chart as one combined deck.
#'
#' Deliberately per-dashboard, not per-user (kept simple for now — see
#' CLAUDE.md). Persisted as a flat JSON file at a path outside every folder
#' the deploy workflow syncs, so favorites survive a redeploy the same way
#' `state/template_uploads/` does (see `utils/template_admin.R`).
#'
#' A favorite is a *snapshot* of an export taken at star-time (table + a
#' think-cell slide block + the option selections that produced it), not a
#' live recipe that re-queries fresh data later. Re-running favorites against
#' updated data is a real follow-up, not attempted here.

FAVORITES_RELATIVE_PATH <- file.path("state", "favorites.json")

#' Where the shared favorites list is stored. Override with
#' `SHINY_FAVORITES_PATH` if a dashboard needs a different location.
favorites_path <- function() {
  Sys.getenv("SHINY_FAVORITES_PATH", FAVORITES_RELATIVE_PATH)
}

#' Directory holding client-captured PNG snapshots of starred charts (see
#' `TC_FAVORITE_CAPTURE_JS` in `utils/chart_downloads.R`). Kept beside
#' `favorites.json` so it follows the same `SHINY_FAVORITES_PATH` override and
#' the same never-committed, never-deploy-synced `state/` treatment.
favorites_assets_dir <- function() {
  file.path(dirname(favorites_path()), "favorite_assets")
}

#' Path a favorite's PNG snapshot would live at, whether or not it exists yet.
#' @param id Favorite id.
favorite_asset_path <- function(id) {
  file.path(favorites_assets_dir(), paste0(id, ".png"))
}

#' Read the shared favorites list.
#' @return List of favorite entries (possibly empty).
favorites_list <- function() {
  path <- favorites_path()
  if (!file.exists(path)) return(list())
  entries <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(entries)) return(list())
  entries
}

favorites_write <- function(entries) {
  path <- favorites_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(entries, path, auto_unbox = TRUE, null = "null", na = "null")
  invisible(path)
}

favorites_new_id <- function() {
  paste0(
    "fav_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
    paste(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = "")
  )
}

#' Save a new favorite.
#' @param entry List, typically the output of [favorites_capture()].
#' @return The new favorite's id (invisibly).
favorites_add <- function(entry) {
  entries <- favorites_list()
  entry$id         <- favorites_new_id()
  entry$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entries[[length(entries) + 1]] <- entry
  favorites_write(entries)
  invisible(entry$id)
}

#' Remove a favorite by id (and its PNG snapshot, if any, to avoid orphaned
#' asset files accumulating in `favorites_assets_dir()`).
favorites_remove <- function(id) {
  entries <- favorites_list()
  kept <- Filter(function(e) !identical(e$id, id), entries)
  favorites_write(kept)
  asset <- favorite_asset_path(id)
  if (file.exists(asset)) unlink(asset)
  invisible(TRUE)
}

#' Coerce a favorite's stored table (a data.frame when freshly captured, or a
#' `list(columns, rows)` shape after a JSON round-trip) back into a plain
#' data.frame.
#'
#' think-cell matrices always have an empty first column header (`""`) by
#' convention (see `format_tc_data()`), and `jsonlite` silently renames an
#' empty data.frame column name to its positional index when serializing a
#' data.frame directly (verified: `toJSON(data.frame(\`\` = 1))` comes back
#' keyed `"1"`, not `""`). Favorites therefore store the column names and row
#' values as two plain arrays instead of relying on JSON object keys to carry
#' column identity, so the header round-trips exactly.
favorites_table_as_df <- function(x) {
  if (is.data.frame(x)) return(x)
  cols <- x$columns
  rows <- x$rows
  if (length(rows) == 0) {
    df <- as.data.frame(matrix(nrow = 0, ncol = length(cols)))
  } else {
    col_values <- lapply(seq_along(cols), function(j) {
      vals <- lapply(rows, function(r) r[[j]])
      vals[vapply(vals, is.null, logical(1))] <- NA
      unlist(vals)
    })
    df <- as.data.frame(col_values, stringsAsFactors = FALSE, check.names = FALSE)
  }
  names(df) <- cols
  df
}

#' Inverse of [favorites_table_as_df()]: shape a data.frame into the
#' `list(columns, rows)` form that survives a JSON round-trip untouched.
favorites_table_to_storage <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  list(
    columns = names(df),
    rows = unname(lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, , drop = FALSE]))))
  )
}

#' Capture one favorite entry from the same inputs the "Download slide"
#' button already uses, so starring a chart saves exactly what that button
#' would produce right now. Mirrors the resolution steps in
#' `chart_data_downloads_server()`'s slide download handler
#' (`utils/chart_downloads.R`) rather than the other way around, so both stay
#' obviously in sync.
#'
#' @param data Resolved data frame (already called, not a reactive). Stored
#'   verbatim as `raw_table` (matches the "Download data (raw)" button) as well
#'   as reshaped into the think-cell matrix (`tc_table`).
#' @param chart_type Resolved (non-reactive) think-cell chart type.
#' @param category_col,series_col,value_col Column names for think-cell export.
#' @param agg_fun,category_order,series_order Passed through to [tc_prepare_slide()].
#' @param facet_col Optional facet column; faceted charts skip slide-matrix prep.
#' @param slide_title,figure_title Optional slide text.
#' @param template_override Optional explicit template filename/path.
#' @param slide_order Category order mode (see [tc_order_slide_matrix()]).
#' @param dashboard_title,tab_label,subtab_label,selections Export log metadata.
#' @param source_output,source_sheet Optional data-source identifiers (see
#'   [tc_build_datasheet_log()] in `utils/slide_download.R`), stored on the
#'   entry so a later deck download can stamp them into this favorite's own
#'   datasheet corner cell, same as the single-chart download.
#' @param filename_prefix Prefix used for this chart's downloads.
#' @param label Optional short display label; defaults to the sub-tab or prefix.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @return A list ready for [favorites_add()].
favorites_capture <- function(
    data, chart_type, category_col, series_col, value_col,
    agg_fun = NULL, category_order = NULL, series_order = NULL, facet_col = NULL,
    slide_title = "", figure_title = "", template_override = "", slide_order = "auto",
    dashboard_title = "", tab_label = "", subtab_label = "",
    selections = NULL, source_output = NULL, source_sheet = NULL,
    filename_prefix = "chart", label = NULL, templates_dir = NULL
) {
  slide_type   <- chart_type
  slide_matrix <- NULL
  if (is.null(facet_col)) {
    prep <- tryCatch(
      tc_prepare_slide(
        df = data, chart_type = chart_type, category_col = category_col,
        series_col = series_col, value_col = value_col, agg_fun = agg_fun,
        category_order = category_order, series_order = series_order
      ),
      error = function(e) NULL
    )
    if (!is.null(prep)) {
      slide_type   <- prep$chart_type
      slide_matrix <- prep$matrix
    }
  }
  if (is.null(slide_matrix)) {
    slide_matrix <- tc_slide_orientation(data, chart_type)
  }
  slide_matrix <- tc_order_slide_matrix(slide_matrix, slide_order)

  override <- if (nzchar(tc_or(template_override, ""))) template_override else NULL
  template_path <- tc_template_for_chart_type(slide_type, templates_dir = templates_dir, override = override)

  # Matches tc_build_slide_zip()'s own fallback exactly: only substitute the
  # sub-tab label when slide_title is genuinely empty (tc_or() doesn't treat
  # "" as missing, so this can't be a simple tc_or() chain).
  effective_slide_title <- if (!nzchar(trimws(tc_or(slide_title, "")))) {
    tc_or(subtab_label, "")
  } else {
    slide_title
  }

  slide_block <- if (!is.na(template_path)) {
    tryCatch(
      tc_build_ppttc_slide_block(
        slide_matrix, tc_short_path(template_path),
        effective_slide_title, figure_title
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  # tc_or() only falls back on NULL, not on "" — and tc_ctx_active_subtab()
  # legitimately returns "" whenever the app hasn't registered a nav/subtab
  # context (see tc_register_app_context()), so pick the first non-empty
  # candidate explicitly rather than chaining tc_or().
  resolved_label <- Find(function(x) !is.null(x) && nzchar(x), list(label, subtab_label, filename_prefix))

  list(
    label           = tc_or(resolved_label, "favorite"),
    filename_prefix = filename_prefix,
    dashboard_title = dashboard_title,
    tab_label       = tab_label,
    subtab_label    = subtab_label,
    chart_type      = slide_type,
    template_name   = if (!is.na(template_path)) basename(template_path) else NA_character_,
    selections      = selections,
    source_output   = source_output,
    source_sheet    = source_sheet,
    slide_order     = slide_order,
    # slide_block embeds an *absolute* template path, only valid for rendering
    # on this same machine (see favorites_build_deck_zip()). slide_title/
    # figure_title are kept separately so a portable block -- referencing the
    # template by bare file name -- can be rebuilt for the shipped fallback.
    slide_title     = effective_slide_title,
    figure_title    = tc_or(figure_title, ""),
    slide_block     = slide_block,
    tc_table        = favorites_table_to_storage(slide_matrix),
    # The exact data behind the plot, before think-cell reshaping -- the same
    # data the chart's own "Download data (raw)" button writes. Captured
    # alongside tc_table so the combined favorites deck can ship both a
    # think-cell workbook and an "as originally plotted" workbook.
    raw_table       = favorites_table_to_storage(data)
  )
}

#' Build one combined ZIP from every saved favorite: two workbooks (one sheet
#' per favorite each, same sheet names) -- `favorites_thinkcell_tables.xlsx`
#' (the think-cell-shaped matrix) and `favorites_raw_tables.xlsx` (the original
#' plot data, matching each chart's "Download data (raw)" button) -- a
#' `chart_<label>.png` snapshot for every favorite that has one (see
#' `TC_FAVORITE_CAPTURE_JS` in `utils/chart_downloads.R`), plus one combined
#' log, and either a single rendered multi-slide deck (one `ppttc.exe` call
#' over every favorite's slide block concatenated into one `.ppttc` array) or
#' the same graceful template+`.ppttc`+README fallback [tc_build_slide_zip()]
#' uses when no renderer is available.
#'
#' Every renderable favorite is also auto-logged to Export History
#' (`utils/export_history.R`) -- one fresh entry + chart id per call, the
#' same as the single-chart "Download slide" button (favorite content never
#' changes after starring, so there's no staleness risk in always minting a
#' fresh id rather than trying to reuse one from an earlier download).
#'
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param entries Favorites to include; defaults to every saved favorite.
#' @param ppttc_exe Optional override for the think-cell executable.
#' @param templates_dir Optional templates directory override (mainly for tests).
favorites_build_deck_zip <- function(zip_path, entries = NULL, ppttc_exe = NULL, templates_dir = NULL) {
  entries <- tc_or(entries, favorites_list())

  work <- tempfile("favorites_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  if (length(entries) == 0) {
    writeLines("No favorites saved yet.", file.path(work, "README.txt"))
  } else {
    labels <- sanitize_excel_sheet_names(
      vapply(entries, function(e) tc_or(e$label, "favorite"), character(1))
    )

    sheets <- stats::setNames(
      lapply(entries, function(e) favorites_table_as_df(e$tc_table)),
      labels
    )
    write_tc_xlsx(sheets, file.path(work, "favorites_thinkcell_tables.xlsx"))

    # raw_table is only present on favorites captured after this field was
    # added; older favorites simply don't contribute a sheet here rather than
    # failing the whole export.
    has_raw <- vapply(entries, function(e) !is.null(e$raw_table), logical(1))
    if (any(has_raw)) {
      raw_sheets <- stats::setNames(
        lapply(entries[has_raw], function(e) favorites_table_as_df(e$raw_table)),
        labels[has_raw]
      )
      write_tc_xlsx(raw_sheets, file.path(work, "favorites_raw_tables.xlsx"))
    }

    # Client-captured PNG snapshots (see TC_FAVORITE_CAPTURE_JS), one per
    # favorite that has one -- named after the same sheet label so it's
    # obvious which workbook sheet a given image goes with. Kept flat
    # (alongside the other zip contents, not in a subfolder) because the zip
    # helper below lists `work` non-recursively.
    for (i in seq_along(entries)) {
      fav_id <- entries[[i]]$id
      if (is.null(fav_id) || !nzchar(fav_id)) next
      asset <- favorite_asset_path(fav_id)
      if (file.exists(asset)) {
        file.copy(asset, file.path(work, paste0("chart_", labels[[i]], ".png")), overwrite = TRUE)
      }
    }

    renderable <- Filter(function(e) !is.null(e$slide_block) && nzchar(tc_or(e$slide_block, "")), entries)
    rendered <- FALSE

    # Auto-log every renderable favorite to Export History (state/export_
    # history/) -- one fresh entry + chart id per "Download all favorites"
    # click, same as the single-chart "Download slide" button, reusing the
    # same per-chart capture/store functions (utils/export_history.R). A
    # favorite already has everything tc_history_capture() needs, so this is
    # a direct mapping, not a re-derivation. Matched by position to `entries`
    # (NA for favorites with no usable template) so it lines up with the log
    # loop below too.
    chart_ids <- rep(NA_character_, length(entries))
    for (i in seq_along(entries)) {
      e <- entries[[i]]
      if (is.null(e$slide_block) || !nzchar(tc_or(e$slide_block, ""))) next
      history_entry <- tc_history_capture(
        tc_data           = favorites_table_as_df(e$tc_table),
        chart_type        = e$chart_type,
        slide_matrix      = favorites_table_as_df(e$tc_table),
        slide_title       = tc_or(e$slide_title, ""),
        figure_title      = tc_or(e$figure_title, ""),
        template_override = tc_or(e$template_name, ""),
        slide_order       = tc_or(e$slide_order, "auto"),
        dashboard_title   = tc_or(e$dashboard_title, ""),
        tab_label         = tc_or(e$tab_label, ""),
        subtab_label      = tc_or(e$subtab_label, ""),
        selections        = e$selections,
        filename_prefix   = tc_or(e$filename_prefix, "chart"),
        templates_dir     = templates_dir
      )
      history_entry$id <- export_history_new_id()
      chart_ids[i] <- export_history_add(history_entry)
    }
    renderable_ids <- chart_ids[vapply(entries, function(e) {
      !is.null(e$slide_block) && nzchar(tc_or(e$slide_block, ""))
    }, logical(1))]

    if (length(renderable) > 0) {
      # Rebuilt fresh per favorite (not the frozen e$slide_block) so each
      # slide's ppttc data carries its own just-minted chart id -- resolving
      # the template path fresh here also means an immediate render always
      # uses whatever the template currently is, not a possibly-stale
      # star-time path.
      render_blocks <- vapply(seq_along(renderable), function(i) {
        e <- renderable[[i]]
        tpl_path <- tc_template_for_chart_type(
          e$chart_type, templates_dir = templates_dir, override = tc_or(e$template_name, "")
        )
        tpl_ref <- if (!is.na(tpl_path)) tc_short_path(tpl_path) else tc_or(e$template_name, "")
        tc_build_ppttc_slide_block(
          favorites_table_as_df(e$tc_table), tpl_ref,
          tc_or(e$slide_title, ""), tc_or(e$figure_title, ""),
          chart_id = renderable_ids[[i]],
          datasheet_log = tc_build_datasheet_log(
            dashboard_title = tc_or(e$dashboard_title, ""),
            tab_label       = tc_or(e$tab_label, ""),
            subtab_label    = tc_or(e$subtab_label, ""),
            chart_type      = e$chart_type,
            selections      = e$selections,
            chart_id        = renderable_ids[[i]],
            source_output   = tc_or(e$source_output, ""),
            source_sheet    = tc_or(e$source_sheet, "")
          )
        )
      }, character(1))
      ppttc_json <- sprintf("[%s]", paste(render_blocks, collapse = ","))
      exe <- tc_or(ppttc_exe, tc_find_ppttc_exe())

      if (!is.null(exe) && !is.na(exe) && nzchar(exe)) {
        out_pptx <- file.path(work, "favorites_deck.pptx")
        res <- tc_render_pptx_ppttc(ppttc_json, out_pptx, exe)
        rendered <- isTRUE(res$ok)
      }

      if (!rendered) {
        # The captured slide_block embeds an *absolute* template path -- only
        # valid on the machine that built it (see the matching note in
        # tc_build_slide_zip()). The shipped .ppttc must instead reference each
        # template by the bare file name it's copied under below, so rebuild a
        # portable block per favorite rather than reusing slide_block as-is.
        portable_blocks <- vapply(seq_along(renderable), function(i) {
          e <- renderable[[i]]
          tc_build_ppttc_slide_block(
            favorites_table_as_df(e$tc_table), tc_or(e$template_name, ""),
            tc_or(e$slide_title, ""), tc_or(e$figure_title, ""),
            chart_id = renderable_ids[[i]],
            datasheet_log = tc_build_datasheet_log(
              dashboard_title = tc_or(e$dashboard_title, ""),
              tab_label       = tc_or(e$tab_label, ""),
              subtab_label    = tc_or(e$subtab_label, ""),
              chart_type      = e$chart_type,
              selections      = e$selections,
              chart_id        = renderable_ids[[i]],
              source_output   = tc_or(e$source_output, ""),
              source_sheet    = tc_or(e$source_sheet, "")
            )
          )
        }, character(1))
        portable_json <- sprintf("[%s]", paste(portable_blocks, collapse = ","))
        writeLines(portable_json, file.path(work, "favorites_deck.ppttc"), useBytes = TRUE)
        templates_used <- unique(Filter(nzchar, stats::na.omit(
          vapply(renderable, function(e) tc_or(e$template_name, NA_character_), character(1))
        )))
        for (tpl in templates_used) {
          tpl_path <- tc_template_for_chart_type("", templates_dir = templates_dir, override = tpl)
          if (!is.na(tpl_path)) file.copy(tpl_path, file.path(work, basename(tpl_path)), overwrite = TRUE)
        }
        writeLines(paste0(
          "think-cell was not available to render the combined deck automatically.\n",
          "To finish it on a PC with PowerPoint + think-cell:\n\n",
          "  ppttc favorites_deck.ppttc -o favorites_deck.pptx\n\n",
          "(the template files referenced inside favorites_deck.ppttc are included alongside it)\n"
        ), file.path(work, "README_render_deck.txt"), useBytes = TRUE)
      }
    } else {
      writeLines(paste(
        "None of the saved favorites currently have a matching think-cell",
        "template, so no deck could be built. The table and log are still included."
      ), file.path(work, "NO_TEMPLATE.txt"))
    }

    log_txt <- paste(vapply(seq_along(entries), function(i) {
      e <- entries[[i]]
      tc_build_log(
        dashboard_title = e$dashboard_title,
        tab_label       = e$tab_label,
        subtab_label    = e$subtab_label,
        selections      = e$selections,
        chart_type      = e$chart_type,
        template_file   = tc_or(e$template_name, NA_character_),
        rendered        = rendered,
        order_mode      = e$slide_order,
        chart_id        = if (is.na(chart_ids[i])) NULL else chart_ids[i],
        note            = if (is.na(tc_or(e$template_name, NA_character_))) {
          "No matching template was available when this was starred."
        } else {
          NULL
        }
      )
    }, character(1)), collapse = "\n\n========================================\n\n")
    writeLines(log_txt, file.path(work, "log.txt"))
  }

  files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = files, flags = "-q -X")

  invisible(zip_path_abs)
}

#' Compact, single-line rendering of a favorite's option selections for the
#' Favorites list (the multi-line [tc_format_selections()] is for the log).
#'
#' Drops empty values, joins each option as `name: value`, and truncates the
#' whole string so a chart with many options doesn't blow up the row.
#' @param selections Named list of option selections (as stored on a favorite).
#' @param max_chars Soft cap on the returned string length.
#' @return A single string, or "" when there is nothing to show.
favorites_selections_inline <- function(selections, max_chars = 160) {
  if (is.null(selections) || length(selections) == 0) return("")
  nm <- names(selections)
  if (is.null(nm)) nm <- paste0("option_", seq_along(selections))
  parts <- vapply(seq_along(selections), function(i) {
    v <- selections[[i]]
    if (is.null(v) || length(v) == 0) return("")
    v <- paste(as.character(v), collapse = ", ")
    if (!nzchar(trimws(v))) return("")
    sprintf("%s: %s", nm[[i]], v)
  }, character(1))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return("")
  out <- paste(parts, collapse = " · ")
  if (nchar(out) > max_chars) out <- paste0(substr(out, 1, max_chars - 1), "…")
  out
}

#' UI for the shared "Favorites" tab: the saved list plus a combined download.
#' @param id Module id.
favorites_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Favorites"),
    shiny::p(class = "text-muted",
             "Shared across everyone using this dashboard — starring a chart saves ",
             "a snapshot of its current export here."),
    shiny::downloadButton(ns("download_all"), "Download all favorites", class = "btn-primary"),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list"))
  )
}

#' Server logic for the shared "Favorites" tab.
#'
#' Uses `reactivePoll()` on the favorites file's modification time so the list
#' picks up stars added from any chart's module server without any direct
#' wiring between modules.
#' @param id Module id.
#' @param poll_interval_ms How often to check the favorites file for changes.
favorites_panel_server <- function(id, poll_interval_ms = 2000) {
  shiny::moduleServer(id, function(input, output, session) {
    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        path <- favorites_path()
        if (file.exists(path)) as.character(file.info(path)$mtime) else ""
      },
      valueFunc = favorites_list
    )

    output$list <- shiny::renderUI({
      entries <- entries_reactive()
      if (length(entries) == 0) {
        return(shiny::tags$p(class = "text-muted",
                             "No favorites saved yet. Star a chart to add one."))
      }
      rows <- lapply(entries, function(e) {
        breadcrumb <- paste(
          Filter(nzchar, c(e$dashboard_title, e$tab_label, e$subtab_label)),
          collapse = " / "
        )
        options_line <- favorites_selections_inline(e$selections)
        details <- shiny::tagList(
          if (nzchar(breadcrumb)) shiny::tags$div(
            style = "font-size:12px; color:#6B7280;", breadcrumb
          ),
          if (nzchar(tc_or(e$chart_type, ""))) shiny::tags$div(
            style = "font-size:11px; color:#9CA3AF;",
            paste0("Chart: ", e$chart_type)
          ),
          if (nzchar(options_line)) shiny::tags$div(
            style = "font-size:11px; color:#6B7280; margin-top:2px;",
            shiny::tags$span(style = "color:#9CA3AF;", "Options: "),
            options_line
          )
        )
        shiny::tags$div(
          style = paste(
            "display:flex; justify-content:space-between; align-items:flex-start;",
            "gap:12px; padding:8px 0; border-bottom:1px solid #eee;"
          ),
          shiny::tags$div(
            shiny::tags$strong(tc_or(e$label, "(untitled)")),
            details
          ),
          shiny::actionButton(session$ns(paste0("remove_", e$id)), "Remove",
                              class = "btn-default btn-sm")
        )
      })
      do.call(shiny::tagList, rows)
    })

    # One-shot removal observers, (re)created whenever the list changes.
    shiny::observe({
      entries <- entries_reactive()
      lapply(entries, function(e) {
        btn_id <- paste0("remove_", e$id)
        shiny::observeEvent(input[[btn_id]], {
          favorites_remove(e$id)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    output$download_all <- shiny::downloadHandler(
      filename = function() paste0("favorites_deck_", Sys.Date(), ".zip"),
      content = function(file) {
        favorites_build_deck_zip(file)
      }
    )
  })
}
