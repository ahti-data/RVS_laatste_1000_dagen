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

#' A fresh id for one "Download all favorites" click -- shared by every
#' history entry logged from that click (see [favorites_build_deck_zip()]),
#' so the whole batch can be found and regenerated together later from
#' Export History. Distinct from [favorites_new_id()] (a *favorite*'s own,
#' permanent id, assigned once at star time) -- this one identifies a
#' *download event*, minted fresh every click, same as a solo download's id.
favorites_download_new_id <- function() {
  paste0(
    "favdl_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
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

#' Remove several favorites (and their PNG snapshots) at once. Irreversible
#' -- the UI (`favorites_panel_server()`) gates this behind a confirmation
#' modal before calling it.
#' @param ids Character vector of favorite ids to remove; `NULL` removes
#'   every saved favorite (used by [favorites_remove_all()]).
favorites_remove_ids <- function(ids = NULL) {
  entries <- favorites_list()
  target_ids <- if (is.null(ids)) vapply(entries, function(e) tc_or(e$id, ""), character(1)) else ids
  for (id in target_ids) {
    asset <- favorite_asset_path(id)
    if (file.exists(asset)) unlink(asset)
  }
  kept <- Filter(function(e) !(tc_or(e$id, "") %in% target_ids), entries)
  favorites_write(kept)
  invisible(TRUE)
}

#' Remove every saved favorite (and every PNG snapshot), emptying the shared
#' list. Thin wrapper around [favorites_remove_ids()] with `ids = NULL`.
favorites_remove_all <- function() {
  favorites_remove_ids(NULL)
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
#' @param module_id The chart's `chart_data_downloads_server(id = ...)`,
#'   stored on the entry so a later "regenerate" (see `utils/export_history.R`)
#'   can look this chart back up in the session's live chart registry and
#'   rebuild it against today's data instead of replaying this snapshot.
#' @param filename_prefix Prefix used for this chart's downloads.
#' @param label Optional short display label; defaults to the chart's own
#'   title (`figure_title`/`slide_title`), then the sub-tab, then the prefix.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @return A list ready for [favorites_add()].
favorites_capture <- function(
    data, chart_type, category_col, series_col, value_col,
    agg_fun = NULL, category_order = NULL, series_order = NULL, facet_col = NULL,
    slide_title = "", figure_title = "", template_override = "", slide_order = "auto",
    dashboard_title = "", tab_label = "", subtab_label = "",
    selections = NULL, source_output = NULL, source_sheet = NULL, module_id = NULL,
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
  # candidate explicitly rather than chaining tc_or(). Prefers the chart's
  # own title (figure_title, then slide_title) over the sub-tab name, so a
  # favorite reads the same way the chart itself does; sub-tab/prefix are
  # only a fallback for charts that don't have a title wired yet.
  resolved_label <- Find(
    function(x) !is.null(x) && nzchar(x),
    list(label, figure_title, slide_title, subtab_label, filename_prefix)
  )

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
    module_id       = module_id,
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

#' Build the HTML page bundling every spec's captured chart image, in the
#' same order as `specs`, so a bulk download's PNGs read as one document
#' instead of N loose files scattered in the ZIP. Self-contained (images
#' embedded as base64 data URIs) -- opens directly in any browser, no
#' server or extra files needed.
#' @param specs Same shape as [tc_build_deck_from_specs()]; only `label` and
#'   `asset_path` are used here.
#' @return The HTML as a single string, or `NA_character_` if no spec has a
#'   usable `asset_path`.
tc_build_charts_overview_html <- function(specs) {
  sections <- vapply(specs, function(s) {
    asset <- s$asset_path
    if (is.null(asset) || !nzchar(asset) || !file.exists(asset)) return("")
    bytes <- tryCatch(readBin(asset, "raw", n = file.info(asset)$size), error = function(e) NULL)
    if (is.null(bytes) || length(bytes) == 0) return("")
    uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
    sprintf(
      '<section style="margin-bottom:32px;"><h2 style="font:600 16px sans-serif; color:#111;">%s</h2><img src="%s" style="max-width:100%%; border:1px solid #ddd; border-radius:4px;"></section>',
      htmltools::htmlEscape(tc_or(s$label, "chart")), uri
    )
  }, character(1))
  sections <- sections[nzchar(sections)]
  if (length(sections) == 0) return(NA_character_)
  paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Charts</title></head><body style=\"font-family:sans-serif; max-width:900px; margin:24px auto; padding:0 16px;\">",
    paste(sections, collapse = "\n"),
    "</body></html>"
  )
}

#' Build one combined ZIP from a list of chart "specs" -- two workbooks (one
#' sheet per spec each, same sheet names) -- `favorites_thinkcell_tables.xlsx`
#' (the think-cell-shaped matrix) and `favorites_raw_tables.xlsx` (only for
#' specs that have one) -- a single `charts_overview.html` bundling every
#' spec's captured chart image in order (see [tc_build_charts_overview_html()];
#' skipped when no spec has one), and either a single rendered multi-slide
#' deck (one `ppttc.exe` call over every spec's slide block concatenated into
#' one `.ppttc` array) or the same graceful template+`.ppttc`+README fallback
#' [tc_build_slide_zip()] uses when no renderer is available.
#'
#' Generic over *where* the specs came from -- [favorites_build_deck_zip()]
#' builds them from saved favorites; a "regenerate this bulk download" flow
#' (`utils/export_history.R`) builds them from freshly re-derived live data
#' instead -- so both stay byte-for-byte consistent by construction.
#'
#' @param specs List of `list(label, tc_table, raw_table = NULL, chart_type,
#'   template_path = NA, slide_title = "", figure_title = "", download_id =
#'   NULL, favorite_download_id = NULL, datasheet_log = NULL, asset_path =
#'   NULL)`. `template_path` (already resolved, or `NA`) decides whether a
#'   spec is renderable; `datasheet_log` is the fully-built corner-cell log
#'   string (see [tc_build_datasheet_log()]) -- the caller builds it, since
#'   it already has every field that goes into it.
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param ppttc_exe Optional override for the think-cell executable.
#' @return `zip_path`, invisibly.
tc_build_deck_from_specs <- function(specs, zip_path, ppttc_exe = NULL) {
  work <- tempfile("tc_deck_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  if (length(specs) == 0) {
    writeLines("No charts to include.", file.path(work, "README.txt"))
  } else {
    labels <- sanitize_excel_sheet_names(
      vapply(specs, function(s) tc_or(s$label, "chart"), character(1))
    )

    as_df <- function(x) as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

    sheets <- stats::setNames(lapply(specs, function(s) as_df(s$tc_table)), labels)
    write_tc_xlsx(sheets, file.path(work, "favorites_thinkcell_tables.xlsx"))

    # raw_table is optional per spec; specs without one simply don't
    # contribute a sheet here rather than failing the whole export.
    has_raw <- vapply(specs, function(s) !is.null(s$raw_table), logical(1))
    if (any(has_raw)) {
      raw_sheets <- stats::setNames(lapply(specs[has_raw], function(s) as_df(s$raw_table)), labels[has_raw])
      write_tc_xlsx(raw_sheets, file.path(work, "favorites_raw_tables.xlsx"))
    }

    # One page with every captured chart image in spec order, instead of N
    # loose chart_<label>.png files -- see tc_build_charts_overview_html().
    overview_html <- tc_build_charts_overview_html(specs)
    if (!is.na(overview_html)) {
      writeLines(overview_html, file.path(work, "charts_overview.html"), useBytes = TRUE)
    }

    renderable_idx <- which(vapply(specs, function(s) !is.na(tc_or(s$template_path, NA_character_)), logical(1)))
    rendered <- FALSE

    if (length(renderable_idx) > 0) {
      render_blocks <- vapply(renderable_idx, function(i) {
        s <- specs[[i]]
        tc_build_ppttc_slide_block(
          as_df(s$tc_table), tc_short_path(s$template_path),
          tc_or(s$slide_title, ""), tc_or(s$figure_title, ""),
          chart_id = s$download_id, datasheet_log = s$datasheet_log,
          favorite_download_id = s$favorite_download_id
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
        # Templates must be referenced by the bare file name copied alongside
        # them (see the matching note in tc_build_slide_zip()) -- a server
        # path resolved here is meaningless on whatever PC opens the bundle.
        portable_blocks <- vapply(renderable_idx, function(i) {
          s <- specs[[i]]
          tc_build_ppttc_slide_block(
            as_df(s$tc_table), basename(s$template_path),
            tc_or(s$slide_title, ""), tc_or(s$figure_title, ""),
            chart_id = s$download_id, datasheet_log = s$datasheet_log,
            favorite_download_id = s$favorite_download_id
          )
        }, character(1))
        portable_json <- sprintf("[%s]", paste(portable_blocks, collapse = ","))
        writeLines(portable_json, file.path(work, "favorites_deck.ppttc"), useBytes = TRUE)
        templates_used <- unique(vapply(specs[renderable_idx], function(s) s$template_path, character(1)))
        for (tpl_path in templates_used) {
          file.copy(tpl_path, file.path(work, basename(tpl_path)), overwrite = TRUE)
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
        "None of these charts currently have a matching think-cell",
        "template, so no deck could be built. The tables are still included."
      ), file.path(work, "NO_TEMPLATE.txt"))
    }
  }

  files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = files, flags = "-q -X")

  invisible(zip_path_abs)
}

#' Build one combined ZIP from every saved favorite -- a thin wrapper around
#' [tc_build_deck_from_specs()] that also auto-logs every renderable favorite
#' to Export History (`utils/export_history.R`), all sharing one fresh
#' `favorite_download_id` (see [favorites_download_new_id()]) so this whole
#' click can be found and regenerated together later, and one fresh
#' `download_id` each -- the same as the single-chart "Download slide"
#' button (favorite content never changes after starring, so there's no
#' staleness risk in always minting fresh ids rather than reusing one).
#'
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param entries Favorites to include; defaults to every saved favorite.
#' @param ppttc_exe Optional override for the think-cell executable.
#' @param templates_dir Optional templates directory override (mainly for tests).
favorites_build_deck_zip <- function(zip_path, entries = NULL, ppttc_exe = NULL, templates_dir = NULL) {
  entries <- tc_or(entries, favorites_list())

  if (length(entries) == 0) {
    return(tc_build_deck_from_specs(list(), zip_path, ppttc_exe))
  }

  favorite_download_id <- favorites_download_new_id()
  # One shared timestamp for every entry logged from this click, rather than
  # each one's independently-generated (near-identical but not exact) time --
  # see export_history_add()'s created_at handling.
  batch_created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  labels <- sanitize_excel_sheet_names(
    vapply(entries, function(e) tc_or(e$label, "favorite"), character(1))
  )

  specs <- lapply(seq_along(entries), function(i) {
    e <- entries[[i]]
    tpl_path <- tc_template_for_chart_type(
      e$chart_type, templates_dir = templates_dir, override = tc_or(e$template_name, "")
    )

    download_id <- NA_character_
    if (!is.na(tpl_path)) {
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
        source_output     = tc_or(e$source_output, ""),
        source_sheet      = tc_or(e$source_sheet, ""),
        favorite_download_id = favorite_download_id,
        module_id         = tc_or(e$module_id, ""),
        filename_prefix   = tc_or(e$filename_prefix, "chart"),
        templates_dir     = templates_dir
      )
      history_entry$id         <- export_history_new_id()
      history_entry$created_at <- batch_created_at
      download_id <- export_history_add(history_entry)
    }

    datasheet_log <- tc_build_datasheet_log(
      dashboard_title = tc_or(e$dashboard_title, ""),
      tab_label       = tc_or(e$tab_label, ""),
      subtab_label    = tc_or(e$subtab_label, ""),
      chart_type      = e$chart_type,
      selections      = e$selections,
      chart_id        = if (is.na(download_id)) NULL else download_id,
      favorite_download_id = favorite_download_id,
      source_output   = tc_or(e$source_output, ""),
      source_sheet    = tc_or(e$source_sheet, "")
    )

    list(
      label = labels[[i]],
      tc_table = favorites_table_as_df(e$tc_table),
      raw_table = if (!is.null(e$raw_table)) favorites_table_as_df(e$raw_table) else NULL,
      chart_type = e$chart_type,
      template_path = tpl_path,
      slide_title = tc_or(e$slide_title, ""),
      figure_title = tc_or(e$figure_title, ""),
      download_id = if (is.na(download_id)) NULL else download_id,
      favorite_download_id = favorite_download_id,
      datasheet_log = datasheet_log,
      asset_path = if (!is.null(e$id) && nzchar(tc_or(e$id, ""))) favorite_asset_path(e$id) else NULL
    )
  })

  tc_build_deck_from_specs(specs, zip_path, ppttc_exe)
}

#' Compact, single-line rendering of a favorite's option selections for the
#' Favorites list.
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

#' UI for a "Favorites" tab: the saved list plus a combined download.
#' @param id Module id.
#' @param intro Optional override for the intro paragraph (a single string).
#'   Defaults to a description of the *shared, all-dashboard* list; pass a
#'   different one when mounting a per-tab filtered instance (see
#'   [favorites_panel_server()]'s `tab_label_filter`).
favorites_panel_ui <- function(id, intro = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Favorites"),
    shiny::p(class = "text-muted", tc_or(
      intro,
      paste0(
        "Shared across everyone using this dashboard — starring a chart saves ",
        "a snapshot of its current export here."
      )
    )),
    shiny::downloadButton(ns("download_all"), "Download all favorites", class = "btn-primary"),
    shiny::actionButton(ns("remove_all"), "Remove all", class = "btn-default"),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list"))
  )
}

#' Server logic for a "Favorites" tab.
#'
#' Uses `reactivePoll()` on the favorites file's modification time so the list
#' picks up stars added from any chart's module server without any direct
#' wiring between modules.
#' @param id Module id.
#' @param poll_interval_ms How often to check the favorites file for changes.
#' @param tab_label_filter Optional tab label (matching a favorite's own
#'   `tab_label`, e.g. `"Iteratie 1"`) -- when supplied, this instance shows,
#'   downloads, and removes only favorites starred from that tab, instead of
#'   the whole shared list. Mount one filtered instance per top-level tab
#'   (each with its own module `id`) alongside one unfiltered instance (the
#'   main "Favorites" tab) that always shows the cumulative, all-tabs list --
#'   they all read/write the same underlying `favorites.json`, so nothing
#'   needs to be kept in sync manually.
favorites_panel_server <- function(id, poll_interval_ms = 2000, tab_label_filter = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        path <- favorites_path()
        if (file.exists(path)) as.character(file.info(path)$mtime) else ""
      },
      valueFunc = function() {
        entries <- favorites_list()
        if (is.null(tab_label_filter)) return(entries)
        Filter(function(e) identical(tc_or(e$tab_label, ""), tab_label_filter), entries)
      }
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
          if (nzchar(tc_or(e$chart_type, "")) || nzchar(tc_or(e$created_at, ""))) shiny::tags$div(
            style = "font-size:11px; color:#9CA3AF;",
            paste(Filter(nzchar, c(
              if (nzchar(tc_or(e$chart_type, ""))) paste0("Chart: ", e$chart_type),
              if (nzchar(tc_or(e$created_at, ""))) paste0("Saved: ", e$created_at)
            )), collapse = " · ")
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
        favorites_build_deck_zip(file, entries = entries_reactive())
      }
    )

    shiny::observeEvent(input$remove_all, {
      n <- length(entries_reactive())
      scope <- if (is.null(tab_label_filter)) {
        "every saved favorite (and its snapshot image) for everyone using this dashboard"
      } else {
        sprintf("all %d favorite(s) starred from this tab (and their snapshot images)", n)
      }
      shiny::showModal(shiny::modalDialog(
        title = "Remove all favorites?",
        sprintf("This deletes %s. This can't be undone.", scope),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("remove_all_confirm"), "Remove all", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$remove_all_confirm, {
      favorites_remove_ids(vapply(entries_reactive(), function(e) e$id, character(1)))
      shiny::removeModal()
    })
  })
}
