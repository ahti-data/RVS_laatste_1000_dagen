#' Dictionary: raw data-source names -> pretty bar/category labels.
#'
#' Every chart's bar/category labels come straight from whatever raw value
#' sits in the underlying data (the `category_col`/`series_col` values fed to
#' `chart_data_downloads_server()` -- see `utils/format_thinkcell_download.R`,
#' which pulls those values verbatim into every export). This module gives a
#' dashboard one editable, persisted place to map a raw value (e.g.
#' `"zvwktotaal"`) to the pretty label that should appear on the chart and in
#' every export instead (e.g. `"Totale ZVW kosten"`) -- see the "Manage
#' templates" tab (`utils/template_admin.R`) for the sibling admin-tab
#' pattern this follows, and `utils/favorites.R` for the shared-JSON-file
#' persistence pattern.
#'
#' Deliberately shared across everyone using the dashboard (like favorites),
#' not per-user -- see CLAUDE.md.
#'
#' Only bar/category *values* are in scope here -- axis titles
#' (`labs(x=, y=)`) are untouched by design.

DICTIONARY_RELATIVE_PATH <- file.path("state", "dictionary.json")

#' Where the shared dictionary is stored. Override with
#' `SHINY_DICTIONARY_PATH` if a dashboard needs a different location.
dictionary_path <- function() {
  Sys.getenv("SHINY_DICTIONARY_PATH", DICTIONARY_RELATIVE_PATH)
}

#' Prefilled default entries for a fresh dashboard, before anyone has edited
#' anything -- written to disk the first time [dictionary_list()] runs with
#' no existing file, so the Dictionary tab always starts non-empty and
#' editable. The template ships an empty seed; a real dashboard overrides
#' this function (defined *after* sourcing this file, e.g. from its own
#' `data/metadata/dictionary_seed.R`) with entries derived from its own raw
#' data-source names.
#' @return List of `list(raw_key, scope, pretty_label)`.
dictionary_seed_entries <- function() {
  list()
}

#' Generic fallback prettifier used when no dictionary entry and no
#' caller-supplied `fallback` apply: underscore/dash -> space, squished,
#' title-cased.
#' @param x Character scalar.
dictionary_default_prettify <- function(x) {
  x <- gsub("[_-]+", " ", as.character(x))
  x <- trimws(gsub("\\s+", " ", x))
  if (!nzchar(x)) return(x)
  # A plain word-by-word capitalizer rather than tools::toTitleCase(), which
  # skips capitalizing a fixed list of "small words" (e.g. "some") even as
  # the first word -- not the behavior wanted for an arbitrary raw data name.
  words <- strsplit(x, " ", fixed = TRUE)[[1]]
  words <- vapply(words, function(w) {
    if (!nzchar(w)) return(w)
    paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
  }, character(1))
  paste(words, collapse = " ")
}

#' Read the shared dictionary, seeding it from [dictionary_seed_entries()] on
#' first use (no file on disk yet) so the file -- and the Dictionary tab --
#' starts prefilled.
#' @return List of `list(raw_key, scope, pretty_label, updated_at)` entries.
dictionary_list <- function() {
  path <- dictionary_path()
  if (!file.exists(path)) {
    seed <- dictionary_seed_entries()
    if (length(seed) > 0) dictionary_write(seed)
    return(seed)
  }
  entries <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(entries)) return(list())
  entries
}

dictionary_write <- function(entries) {
  path <- dictionary_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(entries, path, auto_unbox = TRUE, null = "null", na = "null")
  invisible(path)
}

#' In-process cache of [dictionary_list()], invalidated whenever the
#' underlying file's mtime changes -- avoids re-reading/re-parsing the JSON
#' file once per category value on every chart render (a chart with a large
#' categorical column would otherwise call [dictionary_pretty()] hundreds of
#' times per render).
.DICTIONARY_CACHE <- new.env(parent = emptyenv())

dictionary_entries_cached <- function() {
  path <- dictionary_path()
  mtime <- if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
  cached <- .DICTIONARY_CACHE[[path]]
  if (!is.null(cached) && identical(cached$mtime, mtime)) return(cached$entries)
  entries <- dictionary_list()
  .DICTIONARY_CACHE[[path]] <- list(mtime = mtime, entries = entries)
  entries
}

#' Look up one raw value's dictionary entry.
#' @param raw_key Raw value as it appears in the underlying data.
#' @param scope Disambiguating context -- normally the column name the raw
#'   value comes from (e.g. `"age_cat"`), or a domain tag when several
#'   columns share meaning (e.g. `"zvw_metric"`). Entries are matched on the
#'   exact `(raw_key, scope)` pair -- deliberately no cross-scope fallback,
#'   since the same raw code can mean different things in different columns.
#' @return The stored `pretty_label`, or `NULL` if no entry exists.
dictionary_lookup <- function(raw_key, scope = "") {
  raw_key <- as.character(raw_key)
  scope <- as.character(scope)
  entries <- dictionary_entries_cached()
  for (e in entries) {
    if (identical(as.character(tc_or(e$raw_key, "")), raw_key) &&
        identical(as.character(tc_or(e$scope, "")), scope)) {
      return(e$pretty_label)
    }
  }
  NULL
}

#' Pretty label for one raw value: a stored dictionary entry if one exists,
#' else `fallback(raw_key)` if supplied, else [dictionary_default_prettify()].
#' `NA` passes through unchanged.
#' @param raw_key Raw value.
#' @param scope Disambiguating context; see [dictionary_lookup()].
#' @param fallback Optional one-argument function called with `raw_key` when
#'   no dictionary entry exists -- lets a dashboard keep its own existing
#'   prettifying logic as the default for anything not yet in the dictionary.
dictionary_pretty <- function(raw_key, scope = "", fallback = NULL) {
  if (is.na(raw_key)) return(NA_character_)
  hit <- dictionary_lookup(raw_key, scope)
  if (!is.null(hit)) return(hit)
  if (!is.null(fallback)) return(fallback(raw_key))
  dictionary_default_prettify(raw_key)
}

#' Vectorized [dictionary_pretty()] -- relabel a whole category/series
#' column's *values* (never the column name, never an axis title).
#' @param x Character or factor vector.
#' @param scope Disambiguating context; see [dictionary_lookup()].
#' @param fallback Optional one-argument function; see [dictionary_pretty()].
dictionary_relabel <- function(x, scope = "", fallback = NULL) {
  if (length(x) == 0) return(x)
  vapply(
    as.character(x), dictionary_pretty,
    character(1), scope = scope, fallback = fallback,
    USE.NAMES = FALSE
  )
}

#' Save (add or update) one dictionary entry, keyed on `(raw_key, scope)`.
#' An existing entry for that pair is replaced -- this is how a user edit
#' always wins over a prefilled seed value, since seed rows are written into
#' the same file entries are upserted into. The full read-modify-write cycle
#' runs under [tc_with_file_lock()] (see `utils/slide_download.R`), same
#' reasoning as [favorites_add()] in `utils/favorites.R`.
#' @param raw_key Raw value as it appears in the underlying data.
#' @param scope Disambiguating context; see [dictionary_lookup()].
#' @param pretty_label The label to show instead.
dictionary_set_entry <- function(raw_key, scope, pretty_label) {
  raw_key <- as.character(raw_key)
  scope <- tc_or(as.character(scope), "")
  tc_with_file_lock(dictionary_path(), function() {
    entries <- dictionary_list()
    # A plain for-loop rather than Position() -- ggplot2 attaches its own
    # `Position` ggproto object to the search path ahead of base::Position
    # once library(ggplot2) has run, silently shadowing the base function.
    idx <- NULL
    for (i in seq_along(entries)) {
      e <- entries[[i]]
      if (identical(as.character(tc_or(e$raw_key, "")), raw_key) &&
          identical(as.character(tc_or(e$scope, "")), scope)) {
        idx <- i
        break
      }
    }
    entry <- list(raw_key = raw_key, scope = scope, pretty_label = as.character(pretty_label), updated_at = tc_now())
    if (is.null(idx)) {
      entries[[length(entries) + 1]] <- entry
    } else {
      entries[[idx]] <- entry
    }
    dictionary_write(entries)
  })
  invisible(TRUE)
}

#' Remove one dictionary entry by `(raw_key, scope)`. See
#' [dictionary_set_entry()] for why this locks.
dictionary_remove_entry <- function(raw_key, scope) {
  raw_key <- as.character(raw_key)
  scope <- tc_or(as.character(scope), "")
  tc_with_file_lock(dictionary_path(), function() {
    entries <- dictionary_list()
    kept <- Filter(function(e) {
      !(identical(as.character(tc_or(e$raw_key, "")), raw_key) &&
          identical(as.character(tc_or(e$scope, "")), scope))
    }, entries)
    dictionary_write(kept)
  })
  invisible(TRUE)
}
