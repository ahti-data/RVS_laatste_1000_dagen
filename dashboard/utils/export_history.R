#' Export history: an automatic, durable log of every "Download slide" click.
#'
#' Distinct from `utils/favorites.R` (a manually curated, shared shortlist):
#' every slide download is logged here automatically, with a frozen snapshot
#' of exactly what was exported, so a PM can find and redownload a chart from
#' a long time ago -- byte-for-byte the same ZIP -- without needing to have
#' kept the original around. Each entry gets a short download id that also
#' gets embedded in the exported `.ppttc`/datasheet (see
#' `tc_build_ppttc_slide_block()` in `utils/slide_download.R`), so a chart
#' spotted in a real PowerPoint deck can be traced back here.
#'
#' Two ways to get a chart back, both available per-row and (via the
#' checkboxes + bottom banner) across an arbitrary multi-row selection:
#'   * "Redownload" -- always an exact-snapshot replay, instant, no lookup
#'     needed. Selecting several rows combines them into one deck instead of
#'     one zip per click.
#'   * "Regenerate" -- rebuilds against *today's* live dashboard data when a
#'     chart's module is still registered in this session (see
#'     `tc_chart_registry_get()` in `utils/slide_download.R`), falling back
#'     to the last-known snapshot otherwise. Every regenerate mints
#'     brand-new ids and logs a fresh entry -- it never overwrites or reuses
#'     the one it started from. Selecting several rows mints one shared
#'     `favorite_download_id` across the whole regenerated batch, same as a
#'     bulk "Download all favorites" click.

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
#'   is generated. Same for `entry$created_at` -- kept as-is when already
#'   set, so a batch of entries from one bulk download/regenerate can share
#'   one exact timestamp instead of each independently stamping "now".
#' @return The entry's id (invisibly).
export_history_add <- function(entry) {
  if (is.null(entry$id) || !nzchar(entry$id)) {
    entry$id <- export_history_new_id()
  }
  if (is.null(entry$created_at) || !nzchar(entry$created_at)) {
    entry$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }
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
#' @param favorite_download_id Optional id shared by every chart from the
#'   same "Download all favorites" click or bulk regenerate -- `NULL` for a
#'   solo download.
#' @param module_id The chart's `chart_data_downloads_server(id = ...)`, so a
#'   later regenerate can look this chart back up in the session's live
#'   chart registry (see `tc_chart_registry_get()` in
#'   `utils/slide_download.R`) instead of only ever replaying this snapshot.
#' @param filename_prefix Prefix used for this chart's downloads.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @return A list ready for [export_history_add()].
tc_history_capture <- function(
    tc_data, chart_type, slide_matrix = NULL,
    slide_title = "", figure_title = "", template_override = "", slide_order = "auto",
    dashboard_title = "", tab_label = "", subtab_label = "",
    selections = NULL, source_output = NULL, source_sheet = NULL,
    favorite_download_id = NULL, module_id = NULL,
    filename_prefix = "chart", templates_dir = NULL
) {
  override <- if (nzchar(tc_or(template_override, ""))) template_override else NULL
  template_path <- tc_template_for_chart_type(chart_type, templates_dir = templates_dir, override = override)

  resolved_label <- Find(
    function(x) !is.null(x) && nzchar(x),
    list(figure_title, slide_title, subtab_label, filename_prefix)
  )

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
    favorite_download_id = favorite_download_id,
    module_id         = module_id,
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
    chart_id          = entry$id,
    favorite_download_id = entry$favorite_download_id
  )
}

# ---------------------------------------------------------------------------
# Regenerate: rebuild a chart (or a whole bulk-download group) against
# *today's* live dashboard data, via the session's chart registry
# (utils/slide_download.R), falling back to the last-known snapshot when a
# chart's module isn't registered in the current session. Every regenerate
# mints brand-new ids and logs a fresh entry -- it never touches the id that
# was pasted in.
# ---------------------------------------------------------------------------

#' Regenerate one history entry as a standalone ZIP (used for a solo
#' regenerate) -- live, via the session's chart registry, when possible;
#' otherwise an exact-snapshot rebuild, same as [export_history_redownload()]
#' but under a brand-new id (a regenerate never reuses the pasted id).
#' @return `list(live = TRUE/FALSE)`, invisibly.
export_history_regenerate_entry <- function(entry, zip_path, session,
                                            templates_dir = NULL, ppttc_exe = NULL) {
  reg <- tc_chart_registry_get(session, tc_or(entry$module_id, ""))
  if (!is.null(reg)) {
    ok <- tryCatch({ reg$build_zip(zip_path); TRUE }, error = function(e) FALSE)
    if (ok) return(invisible(list(live = TRUE)))
  }

  new_entry <- entry
  new_entry$id <- export_history_new_id()
  new_entry$favorite_download_id <- NULL
  new_entry$created_at <- NULL
  export_history_add(new_entry)
  export_history_redownload(new_entry, zip_path, templates_dir = templates_dir, ppttc_exe = ppttc_exe)
  invisible(list(live = FALSE))
}

#' Prepare one history entry as a [tc_build_deck_from_specs()] spec (used for
#' a bulk regenerate, where every member folds into one combined deck) --
#' live, via the session's chart registry, when possible; otherwise from the
#' entry's own frozen snapshot. Either way, mints a fresh `download_id`,
#' tags it with `favorite_download_id`/`created_at` (the bulk regenerate's
#' shared batch values), and logs a brand-new history entry.
#' @return `list(live = TRUE/FALSE, spec = list(...))`.
export_history_prepare_regenerate_spec <- function(entry, session, favorite_download_id = NULL,
                                                    created_at = NULL, templates_dir = NULL) {
  reg <- tc_chart_registry_get(session, tc_or(entry$module_id, ""))
  live_spec <- NULL
  if (!is.null(reg)) {
    live_spec <- tryCatch(reg$get_spec(), error = function(e) NULL)
    if (!is.null(live_spec) && isTRUE(live_spec$is_faceted)) live_spec <- NULL
  }

  if (!is.null(live_spec)) {
    history_entry <- tc_history_capture(
      tc_data           = live_spec$tc_data,
      chart_type        = live_spec$chart_type,
      slide_matrix      = live_spec$slide_matrix,
      slide_title       = live_spec$slide_title,
      figure_title      = live_spec$figure_title,
      template_override = live_spec$template_override,
      slide_order       = live_spec$slide_order,
      dashboard_title   = live_spec$dashboard_title,
      tab_label         = live_spec$tab_label,
      subtab_label      = live_spec$subtab_label,
      selections        = live_spec$selections,
      source_output     = live_spec$source_output,
      source_sheet      = live_spec$source_sheet,
      favorite_download_id = favorite_download_id,
      module_id         = tc_or(entry$module_id, ""),
      filename_prefix   = live_spec$filename_prefix,
      templates_dir     = templates_dir
    )
    history_entry$id <- export_history_new_id()
    if (!is.null(created_at)) history_entry$created_at <- created_at
    download_id <- export_history_add(history_entry)

    tpl_path <- tc_template_for_chart_type(
      live_spec$chart_type, templates_dir = templates_dir,
      override = tc_or(live_spec$template_override, "")
    )
    datasheet_log <- tc_build_datasheet_log(
      dashboard_title = live_spec$dashboard_title, tab_label = live_spec$tab_label,
      subtab_label = live_spec$subtab_label, chart_type = live_spec$chart_type,
      selections = live_spec$selections, chart_id = download_id,
      favorite_download_id = favorite_download_id,
      source_output = live_spec$source_output, source_sheet = live_spec$source_sheet
    )
    label <- tc_or(
      Find(function(x) !is.null(x) && nzchar(x),
           list(live_spec$figure_title, live_spec$slide_title, live_spec$subtab_label, live_spec$filename_prefix)),
      "chart"
    )
    slide_matrix <- tc_or(live_spec$slide_matrix, live_spec$tc_data)

    return(list(live = TRUE, spec = list(
      label = label,
      tc_table = as.data.frame(slide_matrix, stringsAsFactors = FALSE, check.names = FALSE),
      raw_table = NULL,
      chart_type = live_spec$chart_type,
      template_path = tpl_path,
      slide_title = live_spec$slide_title,
      figure_title = live_spec$figure_title,
      download_id = download_id,
      favorite_download_id = favorite_download_id,
      datasheet_log = datasheet_log,
      asset_path = NULL
    )))
  }

  # Fallback: this chart's module isn't registered in the current session
  # (e.g. the app restarted since it was last downloaded) -- rebuild from
  # its own frozen snapshot instead, still as a brand-new history entry.
  new_entry <- entry
  new_entry$id <- export_history_new_id()
  new_entry$favorite_download_id <- favorite_download_id
  new_entry$created_at <- created_at
  export_history_add(new_entry)

  tpl_path <- tc_template_for_chart_type(
    new_entry$chart_type, templates_dir = templates_dir,
    override = tc_or(new_entry$template_override, "")
  )
  datasheet_log <- tc_build_datasheet_log(
    dashboard_title = tc_or(new_entry$dashboard_title, ""), tab_label = tc_or(new_entry$tab_label, ""),
    subtab_label = tc_or(new_entry$subtab_label, ""), chart_type = new_entry$chart_type,
    selections = new_entry$selections, chart_id = new_entry$id,
    favorite_download_id = favorite_download_id,
    source_output = tc_or(new_entry$source_output, ""), source_sheet = tc_or(new_entry$source_sheet, "")
  )
  slide_matrix <- if (!is.null(new_entry$slide_matrix_table)) {
    favorites_table_as_df(new_entry$slide_matrix_table)
  } else {
    favorites_table_as_df(new_entry$tc_data_table)
  }

  list(live = FALSE, spec = list(
    label = tc_or(new_entry$label, "chart"),
    tc_table = slide_matrix,
    raw_table = NULL,
    chart_type = new_entry$chart_type,
    template_path = tpl_path,
    slide_title = tc_or(new_entry$slide_title, ""),
    figure_title = tc_or(new_entry$figure_title, ""),
    download_id = new_entry$id,
    favorite_download_id = favorite_download_id,
    datasheet_log = datasheet_log,
    asset_path = NULL
  ))
}

#' Build one [tc_build_deck_from_specs()]-shaped spec straight from a stored
#' history entry's frozen snapshot -- no session/registry involved, and no
#' new id minted (this is an exact redownload, not a regenerate: the spec
#' keeps the entry's own `id`/`favorite_download_id` as-is). This is the same
#' "pick slide_matrix_table over tc_data_table, rebuild the corner-cell log"
#' logic [export_history_prepare_regenerate_spec()]'s fallback branch already
#' does, extracted so both share it instead of a third near-duplicate.
#' @param entry A history entry (as returned by [export_history_get()]).
#' @param templates_dir Optional templates directory override (mainly for tests).
export_history_snapshot_spec <- function(entry, templates_dir = NULL) {
  tpl_path <- tc_template_for_chart_type(
    entry$chart_type, templates_dir = templates_dir,
    override = tc_or(entry$template_override, "")
  )
  datasheet_log <- tc_build_datasheet_log(
    dashboard_title = tc_or(entry$dashboard_title, ""), tab_label = tc_or(entry$tab_label, ""),
    subtab_label = tc_or(entry$subtab_label, ""), chart_type = entry$chart_type,
    selections = entry$selections, chart_id = entry$id,
    favorite_download_id = entry$favorite_download_id,
    source_output = tc_or(entry$source_output, ""), source_sheet = tc_or(entry$source_sheet, "")
  )
  slide_matrix <- if (!is.null(entry$slide_matrix_table)) {
    favorites_table_as_df(entry$slide_matrix_table)
  } else {
    favorites_table_as_df(entry$tc_data_table)
  }

  list(
    label             = tc_or(entry$label, "chart"),
    tc_table          = slide_matrix,
    raw_table         = NULL,
    chart_type        = entry$chart_type,
    template_path     = tpl_path,
    slide_title       = tc_or(entry$slide_title, ""),
    figure_title      = tc_or(entry$figure_title, ""),
    download_id       = entry$id,
    favorite_download_id = entry$favorite_download_id,
    datasheet_log     = datasheet_log,
    asset_path        = NULL
  )
}

#' Exact-snapshot redownload of an arbitrary list of history entries (the
#' Export History tab's checkbox-driven "Redownload selected"). One entry
#' replays [export_history_redownload()] unchanged; two or more combine into
#' a single deck -- same shape "Download all favorites" produces -- via
#' [export_history_snapshot_spec()] + [tc_build_deck_from_specs()]. No new
#' history entries are logged; a redownload isn't a new export.
#' @param entries List of history entries.
#' @return `zip_path`, invisibly.
export_history_download_many <- function(entries, zip_path, ppttc_exe = NULL, templates_dir = NULL) {
  if (length(entries) == 1) {
    return(export_history_redownload(entries[[1]], zip_path, templates_dir = templates_dir, ppttc_exe = ppttc_exe))
  }
  specs <- lapply(entries, export_history_snapshot_spec, templates_dir = templates_dir)
  tc_build_deck_from_specs(specs, zip_path, ppttc_exe)
}

#' Regenerate an arbitrary list of history entries against today's live
#' dashboard data where possible (the Export History tab's per-row
#' "Regenerate" button and checkbox-driven "Regenerate selected"). One entry
#' delegates to [export_history_regenerate_entry()] unchanged, writing a
#' standalone ZIP; two or more mint one shared `favorite_download_id` + one
#' `batch_created_at` (same as a bulk "Download all favorites" click), call
#' [export_history_prepare_regenerate_spec()] per entry, then combine into
#' one deck via [tc_build_deck_from_specs()]. Every regenerate mints
#' brand-new ids -- it never touches the entries passed in.
#' @param entries List of history entries.
#' @return `list(live_count, total)`, invisibly.
export_history_regenerate_many <- function(entries, zip_path, session, templates_dir = NULL, ppttc_exe = NULL) {
  if (length(entries) == 1) {
    res <- export_history_regenerate_entry(
      entries[[1]], zip_path, session, templates_dir = templates_dir, ppttc_exe = ppttc_exe
    )
    return(invisible(list(live_count = if (isTRUE(res$live)) 1 else 0, total = 1)))
  }

  new_favorite_download_id <- favorites_download_new_id()
  batch_created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  prepared <- lapply(entries, function(e) {
    export_history_prepare_regenerate_spec(
      e, session, favorite_download_id = new_favorite_download_id,
      created_at = batch_created_at, templates_dir = templates_dir
    )
  })
  specs <- lapply(prepared, function(p) p$spec)
  live_count <- sum(vapply(prepared, function(p) isTRUE(p$live), logical(1)))
  tc_build_deck_from_specs(specs, zip_path, ppttc_exe)
  invisible(list(live_count = live_count, total = length(specs)))
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
      "around. Find a chart's id on its rendered slide or in its datasheet's corner cell, ",
      "or search for it below. Each row also has its own instant Redownload/Regenerate ",
      "buttons; check one or more rows to act on them together."
    ),
    shiny::textInput(
      ns("search"), NULL,
      placeholder = "Search by download id, dashboard, tab, sub-tab, or chart type..."
    ),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list")),
    shiny::uiOutput(ns("selection_banner"))
  )
}

#' Group history entries sharing one `favorite_download_id` into a single
#' row (kind `"group"`); every other entry stays its own row (kind
#' `"solo"`). Sorted by recency, a group's timestamp being its most-recent
#' member's, so bulk and solo rows interleave correctly in time order.
#' @param entries List of history entries (e.g. from `filtered_entries()`).
#' @return List of `list(kind = "solo", entry, created_at)` or
#'   `list(kind = "group", favorite_download_id, members, created_at)`,
#'   most-recent first.
export_history_group_rows <- function(entries) {
  has_group <- vapply(entries, function(e) nzchar(tc_or(e$favorite_download_id, "")), logical(1))

  solo_rows <- lapply(entries[!has_group], function(e) {
    list(kind = "solo", entry = e, created_at = tc_or(e$created_at, ""))
  })

  bulk_ids <- unique(vapply(entries[has_group], function(e) e$favorite_download_id, character(1)))
  group_rows <- lapply(bulk_ids, function(gid) {
    members <- Filter(function(e) identical(tc_or(e$favorite_download_id, ""), gid), entries)
    created_ats <- vapply(members, function(e) tc_or(e$created_at, ""), character(1))
    list(
      kind = "group",
      favorite_download_id = gid,
      members = members,
      # "%Y-%m-%d %H:%M:%S" strings sort correctly lexicographically, so
      # plain max() gives the most recent one -- which.max() would silently
      # coerce to numeric (NA, with a warning) and error on the resulting
      # empty index.
      created_at = max(created_ats)
    )
  })

  rows <- c(solo_rows, group_rows)
  rows[order(vapply(rows, function(r) r$created_at, character(1)), decreasing = TRUE)]
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
            tc_or(e$id, ""), tc_or(e$favorite_download_id, ""), tc_or(e$label, ""),
            tc_or(e$dashboard_title, ""), tc_or(e$tab_label, ""), tc_or(e$subtab_label, ""),
            tc_or(e$chart_type, ""),
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

    # Groups entries sharing one favorite_download_id into a single
    # expand/collapse row; solo entries (no favorite_download_id) render as
    # before. See export_history_group_rows() for the (pure, unit-tested)
    # grouping/ordering itself.
    display_rows <- shiny::reactive(export_history_group_rows(filtered_entries()))

    # Selection state for the checkbox-driven bottom banner. A row's checkbox
    # id is its entry `id` for a solo row or its `favorite_download_id` for a
    # group row -- a group is selected/acted on as one unit, consistent with
    # how it already regenerates as one unit. Stored in `selected`
    # (reactiveValues) rather than read directly from `input[[...]]`, because
    # a freshly-rendered checkboxInput (renderUI can re-run on any poll tick,
    # e.g. someone else logs a new export while rows are checked) resets to
    # its `value=` argument -- each row is rendered with
    # `value = isTRUE(selected[[row_id]])` so it survives that.
    selected <- shiny::reactiveValues()
    registered_checkboxes <- new.env()

    entry_row_ui <- function(e, indent = FALSE) {
      row_id <- e$id
      shiny::tags$div(
        style = paste(
          "display:flex; justify-content:space-between; align-items:flex-start;",
          "gap:12px; padding:8px 0; border-bottom:1px solid #eee;",
          if (indent) "margin-left:24px;" else ""
        ),
        shiny::tags$div(
          style = "display:flex; gap:8px; align-items:flex-start;",
          if (!indent) shiny::checkboxInput(session$ns(paste0("sel_", row_id)), NULL, value = isTRUE(selected[[row_id]])),
          shiny::tags$div(
            shiny::tags$strong(tc_or(e$label, "(untitled)")),
            shiny::tags$code(style = "font-size:11px; margin-left:8px; color:#6B7280;", tc_or(e$id, "")),
            shiny::tags$div(
              style = "font-size:12px; color:#6B7280;",
              tc_history_entry_subtitle(e)
            ),
            shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              paste(Filter(nzchar, c(tc_or(e$chart_type, ""), tc_or(e$template_name, ""))), collapse = " · ")
            ),
            if (nzchar(tc_or(e$created_at, ""))) shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              paste0("Downloaded: ", e$created_at)
            )
          )
        ),
        shiny::tags$div(
          style = "display:flex; gap:6px; flex-shrink:0;",
          shiny::downloadButton(
            session$ns(paste0("redownload_", e$id)), "Redownload",
            class = "btn-default btn-sm"
          ),
          shiny::downloadButton(
            session$ns(paste0("regenerate_", e$id)), "Regenerate",
            class = "btn-default btn-sm"
          )
        )
      )
    }

    group_row_ui <- function(g) {
      is_open <- isTRUE(expanded[[g$favorite_download_id]])
      row_id <- g$favorite_download_id
      shiny::tags$div(
        style = "padding:8px 0; border-bottom:1px solid #eee;",
        shiny::tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; gap:12px;",
          shiny::tags$div(
            style = "display:flex; gap:8px; align-items:center;",
            shiny::checkboxInput(session$ns(paste0("sel_", row_id)), NULL, value = isTRUE(selected[[row_id]])),
            shiny::actionLink(
              session$ns(paste0("toggle_", g$favorite_download_id)),
              label = sprintf("%s \U0001F4E6 Bulk download — %d charts",
                              if (is_open) "▾" else "▸", length(g$members))
            ),
            shiny::tags$code(style = "font-size:11px; margin-left:8px; color:#6B7280;", g$favorite_download_id),
            if (nzchar(g$created_at)) shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              paste0("Downloaded: ", g$created_at)
            )
          ),
          shiny::downloadButton(
            session$ns(paste0("regenerate_group_", g$favorite_download_id)), "Regenerate",
            class = "btn-default btn-sm"
          )
        ),
        if (is_open) shiny::tagList(lapply(g$members, function(m) entry_row_ui(m, indent = TRUE)))
      )
    }

    output$list <- shiny::renderUI({
      rows  <- display_rows()
      total <- length(entries_reactive())
      if (total == 0) {
        return(shiny::tags$p(class = "text-muted", "No exports logged yet."))
      }
      if (length(rows) == 0) {
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

      row_uis <- lapply(rows, function(r) {
        if (identical(r$kind, "group")) group_row_ui(r) else entry_row_ui(r$entry)
      })
      do.call(shiny::tagList, c(list(cap_note), row_uis))
    })

    # Map every currently-displayed, individually-selectable row id (a solo
    # entry's own id, or a group's favorite_download_id) to its flattened
    # entry list -- a checked group expands to all its members.
    row_id_index <- shiny::reactive({
      idx <- list()
      for (r in display_rows()) {
        if (identical(r$kind, "group")) {
          idx[[r$favorite_download_id]] <- r$members
        } else {
          idx[[r$entry$id]] <- list(r$entry)
        }
      }
      idx
    })

    selected_entries <- shiny::reactive({
      idx <- row_id_index()
      ids <- Filter(function(rid) isTRUE(selected[[rid]]), names(idx))
      unlist(idx[ids], recursive = FALSE)
    })

    # (Re)register one redownload + one regenerate download handler per
    # currently-listed entry, mirroring favorites_panel_server()'s equivalent
    # pattern for its remove buttons. History entries are immutable once
    # written, so re-registering on every poll tick is harmless -- just
    # re-reads the same on-disk snapshot.
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
          output[[paste0("regenerate_", entry$id)]] <- shiny::downloadHandler(
            filename = function() {
              paste0(tc_or(entry$filename_prefix, "chart"), "_slide_regenerated_", Sys.Date(), ".zip")
            },
            content = function(file) {
              export_history_regenerate_many(list(entry), file, session)
            }
          )
        })
      })

      bulk_ids <- unique(Filter(nzchar, vapply(entries, function(e) tc_or(e$favorite_download_id, ""), character(1))))
      lapply(bulk_ids, function(gid) {
        local({
          group_id <- gid
          output[[paste0("regenerate_group_", group_id)]] <- shiny::downloadHandler(
            filename = function() paste0("favorites_deck_regenerated_", Sys.Date(), ".zip"),
            content = function(file) {
              members <- Filter(function(e) identical(tc_or(e$favorite_download_id, ""), group_id), entries)
              export_history_regenerate_many(members, file, session)
            }
          )
        })
      })
    })

    # Expand/collapse state for bulk groups, and a lazily-registered toggle
    # observer per group id -- registered exactly once per id ever seen
    # (favorite_download_id is stable/permanent), unlike the redownload
    # handlers above: an observeEvent (unlike a downloadHandler assignment)
    # isn't idempotent, so re-registering on every poll tick would stack
    # duplicate handlers and make toggling flip-flop incorrectly over time.
    expanded <- shiny::reactiveValues()
    registered_toggles <- new.env()
    shiny::observe({
      entries <- entries_reactive()
      bulk_ids <- unique(Filter(nzchar, vapply(entries, function(e) tc_or(e$favorite_download_id, ""), character(1))))
      new_ids <- Filter(function(gid) !exists(gid, envir = registered_toggles, inherits = FALSE), bulk_ids)
      lapply(new_ids, function(gid) {
        assign(gid, TRUE, envir = registered_toggles)
        btn_id <- paste0("toggle_", gid)
        shiny::observeEvent(input[[btn_id]], {
          expanded[[gid]] <- !isTRUE(expanded[[gid]])
        }, ignoreInit = TRUE)
      })
    })

    # Same lazy-registration pattern as the toggles above, but for each row's
    # checkbox -- an observeEvent isn't idempotent, so this must only ever
    # register once per row id.
    shiny::observe({
      row_ids <- names(row_id_index())
      new_ids <- Filter(function(rid) !exists(rid, envir = registered_checkboxes, inherits = FALSE), row_ids)
      lapply(new_ids, function(rid) {
        assign(rid, TRUE, envir = registered_checkboxes)
        input_id <- paste0("sel_", rid)
        shiny::observeEvent(input[[input_id]], {
          selected[[rid]] <- isTRUE(input[[input_id]])
        }, ignoreInit = TRUE, ignoreNULL = FALSE)
      })
    })

    shiny::observeEvent(input$clear_selection, {
      for (rid in ls(registered_checkboxes)) {
        selected[[rid]] <- FALSE
        shiny::updateCheckboxInput(session, paste0("sel_", rid), value = FALSE)
      }
    })

    output$selection_banner <- shiny::renderUI({
      entries <- selected_entries()
      if (length(entries) == 0) return(NULL)
      shiny::tags$div(
        style = paste(
          "position:fixed; left:0; right:0; bottom:0; z-index:1000;",
          "background:#111827; color:#fff; padding:10px 20px;",
          "display:flex; align-items:center; justify-content:center; gap:16px; flex-wrap:wrap;",
          "box-shadow:0 -2px 8px rgba(0,0,0,0.15);"
        ),
        sprintf("%d chart(s) selected", length(entries)),
        shiny::actionLink(session$ns("clear_selection"), "Clear selection", style = "color:#93C5FD;"),
        shiny::downloadButton(
          session$ns("download_selected"), "Redownload selected",
          class = "btn-default btn-sm"
        ),
        shiny::downloadButton(
          session$ns("regenerate_selected"), "Regenerate selected against today's data",
          class = "btn-primary btn-sm"
        )
      )
    })

    output$download_selected <- shiny::downloadHandler(
      filename = function() paste0("export_history_selected_", Sys.Date(), ".zip"),
      content = function(file) {
        entries <- selected_entries()
        shiny::req(length(entries) > 0)
        export_history_download_many(entries, file)
      }
    )

    output$regenerate_selected <- shiny::downloadHandler(
      filename = function() paste0("export_history_selected_regenerated_", Sys.Date(), ".zip"),
      content = function(file) {
        entries <- selected_entries()
        shiny::req(length(entries) > 0)
        export_history_regenerate_many(entries, file, session)
      }
    )
  })
}
