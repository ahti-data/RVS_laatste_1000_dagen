# =============================================================================
# think-cell slide (PowerPoint) ZIP download helpers
# =============================================================================
# Turns the exact data that already backs the dashboard's think-cell TABLE
# download into a downloadable ZIP containing:
#
#   1. log.txt              - dashboard / tab / sub-tab name + all option
#                             selections that define the displayed figure.
#   2. <prefix>_table.xlsx  - the same think-cell matrix as the table download
#                             (so the underlying data always matches the slide).
#   3. slide.pptx           - a think-cell slide rendered from the template that
#                             matches the displayed chart type.
#
# If think-cell (ppttc.exe) is not installed on the host, the ZIP instead
# ships the (valid, non-corrupt) template plus the ready-to-render `.ppttc`
# data file and a short README, so the user can finish the slide in one click.
#
# Design goals (see CLAUDE.md):
#   * small, pure, individually unit-testable functions
#   * a single source of truth for "which template matches which chart"
#   * reuses the rendering approach proven in R/thinkcell_shiny_app.R
# =============================================================================

# Internal null-coalescing helper. Kept local (tc_or) so this file does not
# depend on, nor collide with, the app-level `%||%`.
tc_or <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------------------------------------------------------------------
# App context: registered once by the main server so the download module can
# label the log with the dashboard name, the active tab / sub-tab, and a
# snapshot of every option the user has selected -- without editing each of
# the (many) per-chart download wirings.
# ---------------------------------------------------------------------------
.TC_CTX <- new.env(parent = emptyenv())

# Inputs that are UI plumbing rather than figure-defining options; excluded
# from the "selected options" log section.
TC_INTERNAL_INPUT_PATTERN <- paste0(
  "(_cell_edit$|_cell_clicked$|_rows_|_columns_|_state$|_search$|_search_",
  "|plotly_|_hover$|hoverData|clickData|relayout|brush|_open$|active_tab$",
  "|^\\.|_dl-|-thinkcell$|-raw$|-slide$)"
)

#' Register the running app's context for slide-export logging.
#'
#' @param input The top-level Shiny `input`.
#' @param dashboard_title Human-readable dashboard name.
#' @param nav_id Input id of the top-level navbar (e.g. "main_nav").
#' @param subtab_by_tab Named character vector mapping each top-level tab value
#'   to the input id of its tabset (e.g. c("Iteratie 1" = "iter1_tabs")).
#' @param dl_option_prefixes Named character vector mapping each download module
#'   id to a regex matching the option inputs that define that chart's figure
#'   (e.g. c("iter1_totaal_dl" = "^tot_")). When a module id is found here, only
#'   the matching inputs are logged; otherwise all non-plumbing inputs are.
#' @param internal_pattern Optional regex overriding [TC_INTERNAL_INPUT_PATTERN].
tc_register_app_context <- function(input,
                                    dashboard_title = "",
                                    nav_id = NULL,
                                    subtab_by_tab = NULL,
                                    dl_option_prefixes = NULL,
                                    internal_pattern = NULL) {
  .TC_CTX$input              <- input
  .TC_CTX$dashboard_title    <- dashboard_title
  .TC_CTX$nav_id             <- nav_id
  .TC_CTX$subtab_by_tab      <- subtab_by_tab
  .TC_CTX$dl_option_prefixes <- dl_option_prefixes
  .TC_CTX$internal_pattern   <- tc_or(internal_pattern, TC_INTERNAL_INPUT_PATTERN)
  invisible(TRUE)
}

tc_ctx_dashboard_title <- function() tc_or(.TC_CTX$dashboard_title, "")

tc_ctx_active_tab <- function() {
  inp <- .TC_CTX$input; nav <- .TC_CTX$nav_id
  if (is.null(inp) || is.null(nav)) return("")
  val <- tryCatch(shiny::isolate(inp[[nav]]), error = function(e) NULL)
  tc_or(val, "")
}

tc_ctx_active_subtab <- function() {
  inp <- .TC_CTX$input; map <- .TC_CTX$subtab_by_tab
  if (is.null(inp) || is.null(map)) return("")
  tab <- tc_ctx_active_tab()
  if (!nzchar(tab) || !tab %in% names(map)) return("")
  sub_id <- map[[tab]]
  val <- tryCatch(shiny::isolate(inp[[sub_id]]), error = function(e) NULL)
  tc_or(val, "")
}

#' Snapshot of the user's current figure-defining option selections.
#'
#' When `module_id` maps to a registered option prefix, only that chart's own
#' inputs are returned; otherwise all non-plumbing inputs are (fallback).
#' @param module_id Optional download module id used to scope the options.
#' @return Named list (sorted).
tc_ctx_selections <- function(module_id = NULL) {
  inp <- .TC_CTX$input
  if (is.null(inp)) return(list())
  all <- tryCatch(shiny::isolate(shiny::reactiveValuesToList(inp)),
                  error = function(e) list())
  if (length(all) == 0) return(list())

  prefix <- NULL
  if (!is.null(module_id) && !is.null(.TC_CTX$dl_option_prefixes) &&
      module_id %in% names(.TC_CTX$dl_option_prefixes)) {
    prefix <- .TC_CTX$dl_option_prefixes[[module_id]]
  }

  if (!is.null(prefix) && nzchar(prefix)) {
    # Scoped: only inputs belonging to this chart's sidebar.
    keep <- names(all)[grepl(prefix, names(all), perl = TRUE)]
  } else {
    # Fallback: everything except UI plumbing and the nav/subtab selectors.
    nav_ids <- c(.TC_CTX$nav_id, unname(.TC_CTX$subtab_by_tab))
    keep <- names(all)[!grepl(.TC_CTX$internal_pattern, names(all))]
    keep <- setdiff(keep, nav_ids)
  }

  # keep only scalar/short atomic option values (drop data frames, long blobs)
  keep <- keep[vapply(keep, function(k) {
    v <- all[[k]]
    is.null(v) || (is.atomic(v) && length(v) <= 50)
  }, logical(1))]
  all[sort(keep)]
}

# ---------------------------------------------------------------------------
# Chart type  ->  template mapping (the "internal function that determines the
# type of plot displayed and picks the right template").
#
# Only chart types with a genuinely matching template are listed. Anything not
# here (stacked_bar, waterfall, scatter, boxplot, ...) has NO suitable template
# and the caller must tell the user so.
# ---------------------------------------------------------------------------
TC_TEMPLATE_BY_CHART_TYPE <- c(
  line            = "template_line.pptx",
  bar             = "template_v_bar.pptx",         # single series, all bars one colour
  v_bar           = "template_v_bar.pptx",
  h_bar           = "template_h_bar.pptx",
  grouped_bar     = "template_v_bar_group.pptx",   # bars coloured per group
  v_bar_group     = "template_v_bar_group.pptx",
  stacked_bar     = "template_v_bar_stacked.pptx", # absolute stacked columns
  v_bar_stacked   = "template_v_bar_stacked.pptx",
  stacked_bar_100 = "template_v_bar_stacked_100.pptx", # 100% stacked columns
  v_bar_stacked_100 = "template_v_bar_stacked_100.pptx",
  pie             = "template_pie.pptx"
)

#' Does a chart type have a matching think-cell template?
#'
#' Type-level check (mapping membership only). Use [tc_template_available()]
#' when you also need the template file to actually exist on disk.
#' @param chart_type Character chart type (legacy aliases are normalised).
#' @return Logical scalar.
tc_chart_type_has_template <- function(chart_type) {
  ct <- normalize_tc_chart_type(chart_type)
  ct %in% names(TC_TEMPLATE_BY_CHART_TYPE)
}

#' Is a usable template file available for this chart type?
#'
#' Stricter than [tc_chart_type_has_template()]: the mapped `.pptx` must also
#' exist on disk. Used to gate the download button so it only appears when a
#' slide can really be produced.
#' @param chart_type Character chart type.
#' @param templates_dir Optional templates directory override.
#' @return Logical scalar.
tc_template_available <- function(chart_type, templates_dir = NULL) {
  !is.na(tc_template_for_chart_type(chart_type, templates_dir))
}

#' Locate the templates directory relative to the app root / working dir.
#' @param start Optional extra root to search first.
#' @return Normalised directory path, or NA_character_ if not found.
tc_find_templates_dir <- function(start = NULL) {
  roots <- c(start,
             if (exists("APP_ROOT")) get("APP_ROOT") else NULL,
             getwd())
  roots <- unique(Filter(function(r) !is.null(r) && !is.na(r) && nzchar(r), roots))
  candidates <- character(0)
  for (r in roots) {
    candidates <- c(
      candidates,
      file.path(r, "templates"),
      file.path(dirname(r), "templates"),
      file.path(r, "..", "templates")
    )
  }
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) return(NA_character_)
  normalizePath(candidates[[1]], winslash = "/", mustWork = FALSE)
}

#' Root under which the app keeps runtime state it must be able to *write*
#' (favorites, uploaded templates). Anchored to `APP_ROOT` when the app defines
#' it, else the working directory — the same base `favorites.json` is written
#' to. Deliberately NOT the deploy-owned `templates/` dir, which on a server is
#' typically read-only for the Shiny process.
tc_runtime_state_root <- function() {
  root <- if (exists("APP_ROOT")) get("APP_ROOT") else NULL
  if (is.null(root) || (length(root) == 1 && is.na(root)) || !nzchar(root)) root <- getwd()
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

#' Runtime-writable directory for templates uploaded through the app (rather
#' than committed to git).
#'
#' In production (no explicit `templates_dir`) this lives beside the app's other
#' runtime state, at `state/template_uploads/` — a location the Shiny process
#' can always write (the same reason `state/favorites.json` works), even when
#' the deploy-owned `templates/` dir is read-only for the app user. Like
#' `state/`, it is never synced by the deploy, so uploads survive a redeploy.
#'
#' Tests (and bespoke deployments) may pass `templates_dir`, in which case the
#' historical `templates/custom/` layout under that dir is used instead; the
#' `SHINY_TEMPLATE_UPLOADS_DIR` environment variable overrides everything.
#' @param templates_dir Optional base templates directory override.
#' @return Path (may not yet exist).
tc_custom_templates_dir <- function(templates_dir = NULL) {
  if (!is.null(templates_dir) && !is.na(templates_dir) && nzchar(templates_dir)) {
    return(file.path(templates_dir, "custom"))
  }
  override <- Sys.getenv("SHINY_TEMPLATE_UPLOADS_DIR", "")
  if (nzchar(override)) return(override)
  file.path(tc_runtime_state_root(), "state", "template_uploads")
}

#' Resolve a template (filename or full path) to an existing file path.
#'
#' A bare filename is looked up in the runtime uploads dir
#' ([tc_custom_templates_dir()]) first, then the built-in `templates/`, so an
#' uploaded override takes precedence over a built-in template of the same name.
#' @return Normalised path, or NA_character_ if it cannot be found.
tc_resolve_template_path <- function(template, templates_dir = NULL) {
  if (is.null(template) || is.na(template) || !nzchar(template)) return(NA_character_)
  if (file.exists(template)) {
    return(normalizePath(template, winslash = "/", mustWork = FALSE))
  }
  custom_dir <- tc_custom_templates_dir(templates_dir)
  if (!is.null(custom_dir) && !is.na(custom_dir) && nzchar(custom_dir)) {
    custom_candidate <- file.path(custom_dir, template)
    if (file.exists(custom_candidate)) {
      return(normalizePath(custom_candidate, winslash = "/", mustWork = FALSE))
    }
  }
  dir <- tc_or(templates_dir, tc_find_templates_dir())
  if (is.null(dir) || is.na(dir)) return(NA_character_)
  candidate <- file.path(dir, template)
  if (file.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

#' Template that visually matches the displayed chart.
#' @param chart_type Character chart type (legacy aliases normalised).
#' @param templates_dir Optional templates directory override.
#' @param override Optional explicit template filename/path to use instead.
#' @return Existing template path, or NA_character_ when none is suitable.
tc_template_for_chart_type <- function(chart_type, templates_dir = NULL, override = NULL) {
  if (!is.null(override) && !is.na(override) && nzchar(override)) {
    return(tc_resolve_template_path(override, templates_dir))
  }
  ct <- normalize_tc_chart_type(chart_type)
  if (!ct %in% names(TC_TEMPLATE_BY_CHART_TYPE)) return(NA_character_)
  fname <- unname(TC_TEMPLATE_BY_CHART_TYPE[[ct]])
  tc_resolve_template_path(fname, templates_dir)
}

# ---------------------------------------------------------------------------
# .ppttc JSON builder (same wire format as R/thinkcell_shiny_app.R, hardened
# for NA / non-numeric cells so it never emits invalid JSON).
# ---------------------------------------------------------------------------
tc_json_escape <- function(s) {
  s <- as.character(s)
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"', '\\\\"', s)
  s <- gsub("\r", "\\\\r", s)
  s <- gsub("\n", "\\\\n", s)
  s <- gsub("\t", "\\\\t", s)
  s
}

tc_cell_label <- function(x) sprintf('{"string":"%s"}', tc_json_escape(x))

tc_cell_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("null")
  num <- suppressWarnings(as.numeric(x))
  if (is.na(num)) return(tc_cell_label(x))  # non-numeric -> string cell
  sprintf('{"number":%s}', formatC(num, format = "f", drop0trailing = TRUE))
}

tc_json_row <- function(cells) sprintf("[%s]", paste(cells, collapse = ","))

#' Transpose a think-cell matrix (swap rows <-> columns).
#'
#' Input/output keep the think-cell convention: first column holds row labels
#' and the first header cell is empty (""). Used to put slide data into the
#' orientation the templates expect (categories across the header, series down
#' the first column) regardless of how [format_tc_data()] laid it out.
tc_transpose_matrix <- function(m) {
  m <- as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(m) < 2 || nrow(m) < 1) return(m)
  row_labels    <- as.character(m[[1]])   # become the new header
  series_labels <- names(m)[-1]           # become the new first column
  vals <- t(as.matrix(m[, -1, drop = FALSE]))
  out <- data.frame(series_labels, vals, check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- c("", row_labels)
  rownames(out) <- NULL
  out
}

#' Put a think-cell matrix into the orientation the slide templates expect.
#'
#' The reference templates (and R/thinkcell_shiny_app.R) read categories from
#' the header row and series from the first column. [format_tc_data()] already
#' does this for line charts but transposes bar/stacked/grouped charts, so we
#' transpose those back for the slide. (The exported table keeps its own layout.)
tc_slide_orientation <- function(m, chart_type) {
  if (normalize_tc_chart_type(chart_type) %in% tc_chart_types_transposed()) {
    return(tc_transpose_matrix(m))
  }
  m
}

#' Chart types that render as vertical/horizontal bars (may be single- or
#' multi-series). Used to decide when a chart collapses to a plain bar.
TC_BAR_FAMILY <- c(
  "bar", "v_bar", "h_bar",
  "grouped_bar", "v_bar_group",
  "stacked_bar", "v_bar_stacked",
  "stacked_bar_100", "v_bar_stacked_100"
)

#' List the template `.pptx` files available to the app.
#'
#' Merges the built-in `templates/` set with any uploaded overrides in the
#' runtime uploads dir ([tc_custom_templates_dir()]), deduplicated by file name;
#' `tc_resolve_template_path()` prefers the uploaded copy when both exist.
#' @return Sorted character vector of file names (may be empty).
tc_list_templates <- function(templates_dir = NULL) {
  dir <- tc_or(templates_dir, tc_find_templates_dir())
  base_files <- if (!is.null(dir) && !is.na(dir) && dir.exists(dir)) {
    list.files(dir, pattern = "\\.pptx$", ignore.case = TRUE)
  } else {
    character(0)
  }
  custom_dir <- tc_custom_templates_dir(templates_dir)
  custom_files <- if (!is.null(custom_dir) && !is.na(custom_dir) && dir.exists(custom_dir)) {
    list.files(custom_dir, pattern = "\\.pptx$", ignore.case = TRUE)
  } else {
    character(0)
  }
  sort(unique(c(base_files, custom_files)))
}

#' Named choices for a template-override selectInput.
#' First entry is the automatic (detected) option with value "".
tc_template_choices <- function(templates_dir = NULL) {
  files  <- tc_list_templates(templates_dir)
  labels <- c("Automatisch (gedetecteerd)", files)
  values <- c("", files)
  stats::setNames(values, labels)
}

#' Detect the template chart type for the *displayed* figure from its data.
#'
#' Same rule used by [tc_prepare_slide()], exposed separately so the UI can show
#' the chosen template reactively without building the whole matrix.
tc_detect_slide_type <- function(df, chart_type, category_col, series_col) {
  ct <- normalize_tc_chart_type(chart_type)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c(category_col, series_col) %in% names(df))) return(ct)
  n_cat <- dplyr::n_distinct(df[[category_col]])
  n_ser <- dplyr::n_distinct(df[[series_col]])
  n_series_eff <- if (n_cat <= 1 && n_ser > 1) n_cat else n_ser
  if (ct %in% TC_BAR_FAMILY && n_series_eff <= 1) "bar" else ct
}

#' Parse a vector to numbers, or return NULL if any element isn't numeric.
#' Handles both "." and "," decimal separators.
tc_numeric_or_na <- function(x) {
  x <- as.character(x)
  n <- suppressWarnings(as.numeric(x))
  if (!anyNA(n)) return(n)
  n2 <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  if (!anyNA(n2)) return(n2)
  NULL
}

#' Order the category (column) axis of a slide matrix.
#'
#' The slide matrix keeps categories in the header row and series in the first
#' column, so ordering only ever reorders the category columns - there is no
#' ambiguity about which axis is affected.
#'
#' Modes:
#'   * "auto"     - numeric-ascending when *every* category label is a number
#'                  (matches how the dashboard renders numeric axes); otherwise
#'                  left as displayed. Safe default.
#'   * "as_is"    - keep the order exactly as provided.
#'   * "cat_asc"  / "cat_desc" - sort by category label (numeric-aware).
#'   * "val_asc"  / "val_desc" - sort by the category's total across all series.
tc_order_slide_matrix <- function(m, mode = "auto") {
  m <- as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(mode) || !nzchar(mode) || identical(mode, "as_is")) return(m)
  if (ncol(m) < 3) return(m)  # 0 or 1 category: nothing to reorder

  cats <- names(m)[-1]
  cat_num <- tc_numeric_or_na(cats)

  col_totals <- function() {
    vapply(cats, function(cn) {
      sum(suppressWarnings(as.numeric(as.character(m[[cn]]))), na.rm = TRUE)
    }, numeric(1))
  }

  ord <- switch(
    mode,
    auto     = if (!is.null(cat_num)) order(cat_num) else seq_along(cats),
    cat_asc  = if (!is.null(cat_num)) order(cat_num) else order(cats),
    cat_desc = rev(if (!is.null(cat_num)) order(cat_num) else order(cats)),
    val_asc  = order(col_totals()),
    val_desc = rev(order(col_totals())),
    seq_along(cats)
  )

  m[, c(1, 1 + ord), drop = FALSE]
}

#' Windows-safe path for embedding in the .ppttc `template` field.
#'
#' think-cell's `ppttc` can fail to load templates whose path contains spaces or
#' parentheses (e.g. "Downloads (2)"). On Windows we hand it the short 8.3 path,
#' which has neither; elsewhere we just normalise. Returns a forward-slashed path.
tc_short_path <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(path)
  p <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    short <- tryCatch(utils::shortPathName(p), error = function(e) p)
    if (length(short) == 1 && nzchar(short)) p <- short
  }
  gsub("\\\\", "/", p)
}

#' Determine the template chart type and slide matrix that match the *displayed*
#' figure, based on the data behind it.
#'
#' The dashboard sometimes declares a grouped/stacked bar even when the current
#' selection collapses the figure to a single series (one category, or one
#' series). think-cell should then show a plain vertical bar. This inspects the
#' data and:
#'   * puts the dimension that actually varies on the x-axis (categories),
#'   * downgrades grouped/stacked bars to a simple `bar` when only one series
#'     remains,
#'   * returns the matrix already in the templates' expected orientation
#'     (categories across the header, series down the first column).
#'
#' @return list(chart_type = <template chart type>, matrix = <data frame>).
tc_prepare_slide <- function(df, chart_type, category_col, series_col, value_col,
                             agg_fun = NULL, category_order = NULL, series_order = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  n_cat <- dplyr::n_distinct(df[[category_col]])
  n_ser <- dplyr::n_distinct(df[[series_col]])

  cat_col <- category_col
  ser_col <- series_col
  ord_cat <- category_order
  ord_ser <- series_order
  # One category but several series -> put the series on the x-axis so the
  # figure reads as a simple bar chart rather than a cluster at one position.
  if (n_cat <= 1 && n_ser > 1) {
    cat_col <- series_col
    ser_col <- category_col
    ord_cat <- series_order
    ord_ser <- category_order
  }

  # "line" layout == reference orientation (series rows, categories columns).
  m <- format_tc_data(
    df, chart_type = "line",
    category_col = cat_col, series_col = ser_col, value_col = value_col,
    agg_fun = agg_fun, category_order = ord_cat, series_order = ord_ser
  )

  slide_type <- tc_detect_slide_type(df, chart_type, category_col, series_col)

  list(chart_type = slide_type, matrix = m)
}

#' Build a single think-cell slide `{template,data}` JSON object (no array
#' wrapper). Exposed separately from [tc_build_ppttc_json()] so multiple
#' slides can be concatenated into one `.ppttc` array for a multi-slide deck
#' (see `favorites_build_deck_zip()` in `utils/favorites.R`), since one
#' `ppttc.exe` call over an array of these blocks renders one deck.
#'
#' @param df think-cell matrix: column 1 = row labels, remaining columns are
#'   categories. This is exactly what [format_tc_data()] returns.
#' @param template Template path written into the JSON (forward-slashed).
#' @param slide_title Optional slide title (bound to SlideTitle). Skipped if "".
#' @param figure_title Optional figure caption (bound to FigureTitle). Skipped if "".
#' @return A single JSON object string (not wrapped in `[...]`).
tc_build_ppttc_slide_block <- function(df, template, slide_title = "", figure_title = "") {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df) < 2) {
    stop("think-cell matrix needs at least a label column and one value column.", call. = FALSE)
  }
  cats   <- names(df)[-1]
  header <- tc_json_row(c("null", vapply(cats, tc_cell_label, character(1))))
  series <- vapply(seq_len(nrow(df)), function(i) {
    label  <- tc_cell_label(df[[1]][i])
    values <- vapply(df[i, -1, drop = FALSE], tc_cell_value, character(1))
    tc_json_row(c(label, values))
  }, character(1))

  chart_table <- sprintf("[%s]", paste(c(header, series), collapse = ","))

  blocks <- character(0)
  if (nzchar(trimws(tc_or(slide_title, "")))) {
    blocks <- c(blocks, sprintf('{"name":"SlideTitle","table":[[%s]]}',
                                tc_cell_label(slide_title)))
  }
  blocks <- c(blocks, sprintf('{"name":"Chart1","table":%s}', chart_table))
  if (nzchar(trimws(tc_or(figure_title, "")))) {
    blocks <- c(blocks, sprintf('{"name":"FigureTitle","table":[[%s]]}',
                                tc_cell_label(figure_title)))
  }

  data_block <- paste(blocks, collapse = ",")
  sprintf('{"template":"%s","data":[%s]}',
          gsub("\\\\", "/", template), data_block)
}

#' Build the think-cell `.ppttc` JSON for one slide.
#' @inheritParams tc_build_ppttc_slide_block
#' @return A single JSON string (a one-element array).
tc_build_ppttc_json <- function(df, template, slide_title = "", figure_title = "") {
  sprintf("[%s]", tc_build_ppttc_slide_block(df, template, slide_title, figure_title))
}

# ---------------------------------------------------------------------------
# think-cell executable discovery + rendering (mirrors the reference app).
# ---------------------------------------------------------------------------
#' Find the think-cell `ppttc` executable, or NA_character_ if unavailable.
tc_find_ppttc_exe <- function() {
  candidates <- c(
    getOption("tc.ppttc_exe", default = NA_character_),
    Sys.getenv("TC_PPTTC_EXE", unset = NA_character_),
    "C:/Program Files (x86)/think-cell/ppttc.exe",
    "C:/Program Files/think-cell/ppttc.exe"
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) return(normalizePath(hit[[1]], winslash = "/", mustWork = FALSE))
  on_path <- unname(Sys.which(c("ppttc.exe", "ppttc")))
  on_path <- on_path[nzchar(on_path)]
  if (length(on_path) > 0) return(on_path[[1]])
  NA_character_
}

#' Render a .pptx from ppttc JSON via the think-cell executable.
#' @return list(ok, status, log).
tc_render_pptx_ppttc <- function(json, out_pptx, exe) {
  ppttc_path <- tempfile(fileext = ".ppttc")
  writeLines(json, ppttc_path, useBytes = TRUE)
  on.exit(unlink(ppttc_path), add = TRUE)
  out <- suppressWarnings(system2(
    exe,
    args   = c(shQuote(ppttc_path), "-o", shQuote(out_pptx)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(
    ok     = (status == 0L && file.exists(out_pptx)),
    status = status,
    log    = paste(out, collapse = "\n")
  )
}

# ---------------------------------------------------------------------------
# Log file
# ---------------------------------------------------------------------------
tc_format_selections <- function(selections) {
  if (is.null(selections) || length(selections) == 0) return("  (none captured)")
  nm <- names(selections)
  if (is.null(nm)) nm <- paste0("option_", seq_along(selections))
  lines <- vapply(seq_along(selections), function(i) {
    v <- selections[[i]]
    if (is.null(v) || length(v) == 0) {
      v <- ""
    } else {
      v <- paste(as.character(v), collapse = ", ")
    }
    sprintf("  - %s: %s", nm[[i]], v)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' Assemble the human-readable export log.
tc_build_log <- function(dashboard_title, tab_label, subtab_label, selections,
                         chart_type, template_file, rendered, note = NULL,
                         order_mode = NULL) {
  order_label <- switch(
    tc_or(order_mode, "auto"),
    auto     = "auto (numeric ascending when applicable, else as displayed)",
    as_is    = "as displayed",
    cat_asc  = "category ascending",
    cat_desc = "category descending",
    val_asc  = "value ascending",
    val_desc = "value descending",
    tc_or(order_mode, "auto")
  )
  header <- paste(
    "think-cell slide export log",
    "===========================",
    paste0("Generated:      ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Dashboard:      ", tc_or(dashboard_title, "")),
    paste0("Tab:            ", tc_or(tab_label, "")),
    paste0("Sub-tab:        ", tc_or(subtab_label, "")),
    paste0("Chart type:     ", tc_or(chart_type, "")),
    paste0("Template:       ", tc_or(template_file, "(none available)")),
    paste0("Category order: ", order_label),
    paste0("Slide rendered: ",
           if (isTRUE(rendered)) "yes (think-cell)"
           else "no (template + .ppttc data included instead)"),
    sep = "\n"
  )
  if (!is.null(note) && nzchar(note)) {
    header <- paste(header, paste0("Note:           ", note), sep = "\n")
  }
  paste0(
    header, "\n\n",
    "Selected options\n",
    "----------------\n",
    tc_format_selections(selections), "\n"
  )
}

# ---------------------------------------------------------------------------
# Orchestrator: build the download ZIP.
# ---------------------------------------------------------------------------
#' Build the think-cell slide download ZIP at `zip_path`.
#'
#' @param zip_path Output .zip path (the `file` handed in by downloadHandler).
#' @param tc_data think-cell matrix from [format_tc_data()] (a data frame, or a
#'   named list of data frames for faceted charts).
#' @param chart_type Resolved chart type of the displayed figure.
#' @param slide_title,figure_title Optional titles bound in the template.
#' @param dashboard_title,tab_label,subtab_label Log metadata.
#' @param selections Named list of the user's option selections (for the log).
#' @param filename_prefix Prefix for the table file name.
#' @param templates_dir,template_override,ppttc_exe Optional overrides.
#' @param write_table_fun Function(data, path) writing the underlying table.
#' @return list(zip_path, rendered, template, note) invisibly.
tc_build_slide_zip <- function(zip_path,
                               tc_data,
                               chart_type,
                               slide_title      = "",
                               figure_title     = "",
                               dashboard_title  = "",
                               tab_label        = "",
                               subtab_label     = "",
                               selections       = NULL,
                               filename_prefix  = "chart",
                               templates_dir    = NULL,
                               template_override = NULL,
                               ppttc_exe        = NULL,
                               slide_matrix     = NULL,
                               slide_order      = "auto",
                               write_table_fun  = write_tc_xlsx) {

  resolved_type <- normalize_tc_chart_type(chart_type)
  template_path <- tc_template_for_chart_type(resolved_type, templates_dir, template_override)
  has_template  <- !is.na(template_path)

  work <- tempfile("tc_slide_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # ---- (2) underlying table (identical to the think-cell table download) ----
  table_path <- file.path(work, paste0(filename_prefix, "_table.xlsx"))
  write_table_fun(tc_data, table_path)

  # think-cell renders a single matrix. The caller may pass a pre-oriented
  # `slide_matrix` (already reflecting the displayed plot type); otherwise derive
  # it from the table data and orient it for the templates.
  is_faceted <- is_tc_workbook_list(tc_data)
  if (!is.null(slide_matrix)) {
    slide_matrix <- as.data.frame(slide_matrix, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    slide_matrix <- if (is_faceted) tc_data[[1]] else tc_data
    slide_matrix <- tc_slide_orientation(slide_matrix, chart_type)
  }
  # Order the category axis (matches the displayed figure by default).
  slide_matrix <- tc_order_slide_matrix(slide_matrix, slide_order)
  note <- if (is_faceted) {
    sprintf("data has %d facets; slide shows first facet '%s', table contains all facets",
            length(tc_data), names(tc_data)[[1]])
  } else NULL

  rendered <- FALSE

  if (!has_template) {
    # No suitable template: still give the user the data + a clear explanation.
    note <- paste(c(
      sprintf("No think-cell template matches chart type '%s'; slide was not created.", chart_type),
      note
    ), collapse = " | ")
    writeLines(paste0(
      "No suitable think-cell template is available for the chart currently ",
      "displayed in the dashboard (chart type: ", chart_type, ").\n\n",
      "The underlying data table is still included so it can be built manually.\n",
      "Templates exist for: ", paste(sort(unique(names(TC_TEMPLATE_BY_CHART_TYPE))), collapse = ", "), ".\n"
    ), file.path(work, "NO_TEMPLATE.txt"), useBytes = TRUE)
  } else {
    # ---- (3) slide ----------------------------------------------------------
    if (!nzchar(trimws(tc_or(slide_title, "")))) slide_title <- tc_or(subtab_label, "")
    json <- tc_build_ppttc_json(slide_matrix, tc_short_path(template_path), slide_title, figure_title)
    exe  <- tc_or(ppttc_exe, tc_find_ppttc_exe())

    if (!is.null(exe) && !is.na(exe) && nzchar(exe)) {
      out_pptx <- file.path(work, "slide.pptx")
      res <- tc_render_pptx_ppttc(json, out_pptx, exe)
      if (isTRUE(res$ok)) {
        rendered <- TRUE
      } else {
        note <- paste(c(note, paste("ppttc render failed:", res$log)), collapse = " | ")
      }
    } else {
      note <- paste(c(note,
        paste0("think-cell (ppttc) not found on this machine, so the slide was ",
               "not rendered. Set options(tc.ppttc_exe = \"<path to ppttc.exe>\") ",
               "or the TC_PPTTC_EXE environment variable, then download again.")),
        collapse = " | ")
    }

    if (!rendered) {
      # Graceful, never-corrupt fallback: ship the valid template + ppttc data.
      # The shipped .ppttc must reference the template by the bare file name it's
      # copied under here, NOT the absolute path resolved above -- that path is
      # only valid on *this* machine (typically the Linux server, which is why
      # rendering fell back in the first place). A PM opening this bundle on
      # their own PC has no such path; think-cell needs "slide_template.pptx"
      # sitting right next to chart_data.ppttc, not a server path it can't reach.
      file.copy(template_path, file.path(work, "slide_template.pptx"), overwrite = TRUE)
      portable_json <- tc_build_ppttc_json(slide_matrix, "slide_template.pptx", slide_title, figure_title)
      writeLines(portable_json, file.path(work, "chart_data.ppttc"), useBytes = TRUE)
      writeLines(paste0(
        "think-cell was not available to render the slide automatically on this machine.\n",
        "To finish the slide on a PC with PowerPoint + think-cell:\n\n",
        "  Option A (command line):\n",
        "    ppttc chart_data.ppttc -o slide.pptx\n\n",
        "  Option B (in PowerPoint):\n",
        "    1. Open slide_template.pptx.\n",
        "    2. think-cell ribbon > update the chart from chart_data.ppttc.\n\n",
        "The underlying data (", basename(table_path), ") matches this chart exactly.\n"
      ), file.path(work, "README_render_slide.txt"), useBytes = TRUE)
    }
  }

  # ---- (1) log --------------------------------------------------------------
  log_txt <- tc_build_log(
    dashboard_title = dashboard_title,
    tab_label       = tab_label,
    subtab_label    = subtab_label,
    selections      = selections,
    chart_type      = chart_type,
    template_file   = if (has_template) basename(template_path) else NA_character_,
    rendered        = rendered,
    note            = note,
    order_mode      = slide_order
  )
  writeLines(log_txt, file.path(work, "log.txt"), useBytes = TRUE)

  # ---- zip (flat) -----------------------------------------------------------
  files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = files, flags = "-q -X")

  invisible(list(
    zip_path = zip_path_abs,
    rendered = rendered,
    template = if (has_template) template_path else NA_character_,
    note     = note
  ))
}
