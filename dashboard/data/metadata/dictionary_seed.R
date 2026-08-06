#' Dictionary prefill for this dashboard (see `utils/dictionary.R`).
#'
#' Overrides the template's empty `dictionary_seed_entries()` with real rows,
#' mechanically derived from the existing `pretty_*_default()` recode tables
#' and raw-name vectors already in `app.R` (`pretty_sheet_default()`,
#' `pretty_stat_default()`, `pretty_code_metric_default()`,
#' `pretty_vektmszsettingzpk_default()`, `pretty_it3_zpk_metric_default()`,
#' `pretty_split_name_default()`, `pretty_value_default()`,
#' `population_label_default()`, `pretty_metric_name_default()` applied to
#' `domain_order_zvw`/`diag_activity_names`/`intervention_names`) -- rather
#' than hand-retyping the Dutch labels, which risks a transcription typo
#' diverging from the logic that already produces them. Seeded once into
#' `state/dictionary.json` on first use; after that, edits made from the
#' Dictionary tab are the source of truth (see `dictionary_list()`).
#'
#' Deliberately not exhaustive -- names that only exist inside a workbook at
#' runtime (per-sheet `name`/`cost_type` values, "top 20 codes" procedure
#' lists) aren't enumerable here. Anything not seeded simply falls back to
#' the matching `pretty_*_default()` function until someone adds it from the
#' Dictionary tab.
#'
#' Must load *after* `utils/dictionary.R` (for the default `scope`-taking
#' helpers) and after this file's own `pretty_*_default()`/raw-name-vector
#' definitions further down in `app.R` have run -- safe because this
#' function's body only runs when `dictionary_list()` first reads the
#' dictionary (well after the whole of `app.R` has sourced), not when this
#' file itself is sourced.
dictionary_seed_entries <- function() {
  entries <- list()
  add <- function(raw_key, scope, pretty_label) {
    entries[[length(entries) + 1]] <<- list(raw_key = raw_key, scope = scope, pretty_label = pretty_label)
  }
  add_from <- function(raw_keys, scope, prettify) {
    for (k in raw_keys) add(k, scope, prettify(k))
  }

  # Sheet names -> pretty_sheet_default()
  sheet_keys <- c(
    "top_20_codes_operatie_1000", "top_20_codes_operatie_30",
    "top_20_codes_activit_1000", "top_20_codes_activit_30",
    "wlz", "wlz_corrected", "zvw", "zvw_corrected",
    "msz_prestaties", "msz_prestaties_corrected", "msz_prestaties_diag",
    "msz_activit_diag", "msz_addon_oncology_total_cancer",
    "msz_addon_oncology_cancer", "msz_addon_oncology_total", "msz_addon",
    "huisartsdecltab", "msz_prestatie_diagnostiek"
  )
  add_from(sheet_keys, "sheet", pretty_sheet_default)

  # Aggregation-statistic names -> pretty_stat_default()
  add_from(names(stat_labels_iteration2), "stat", pretty_stat_default)

  # Code/declaration metric names -> pretty_code_metric_default()
  code_metric_keys <- c(
    "n_totaal_gebruikers", "n_totaal_declaraties", "gebruikers_per_persoon",
    "declaraties_per_persoon", "sum_totaal_groep", "sum_per_gebruiker"
  )
  add_from(code_metric_keys, "code_metric", pretty_code_metric_default)

  # vektmszsettingzpk codes -> pretty_vektmszsettingzpk_default()
  add_from(c("1", "2", "3", "9"), "vektmszsettingzpk", pretty_vektmszsettingzpk_default)

  # Iteration-3 ZPK metric names -> pretty_it3_zpk_metric_default()
  it3_zpk_metric_keys <- c(
    "n_totaal_gebruikers", "n_totaal_declaraties", "sum_totaal_groep",
    "median_cost_per_declaratie", "gemiddelde_kosten_per_persoon"
  )
  add_from(it3_zpk_metric_keys, "it3_zpk_metric", pretty_it3_zpk_metric_default)

  # Demographic split column names -> pretty_split_name_default()
  add_from(demographic_cols_iteration2, "split_name", pretty_split_name_default)
  add_from(c("provincie", "burgstaat", "used_any_acp_2years"), "split_name", pretty_split_name_default)

  # Column-specific category values -> pretty_value_default()
  add_from(as.character(2:9), "age_cat", function(k) pretty_value_default("age_cat", k))
  add_from(c("400+", "280_400", "120_280", "tot_120", "Overig"), "inkomen_klasse",
           function(k) pretty_value_default("inkomen_klasse", k))

  # Population labels -> population_label_default()
  add_from(c("Overleden", "In leven"), "population", population_label_default)

  # ZVW domain/metric names -> pretty_metric_name_default(x, sheet = "zvw")
  add_from(domain_order_zvw, "zvw_metric", function(k) pretty_metric_name_default(k, sheet = "zvw"))

  # Non-ZVW metric names -> pretty_metric_name_default(x, sheet = NULL)
  add_from(c(diag_activity_names, intervention_names), "metric_generic",
           function(k) pretty_metric_name_default(k, sheet = NULL))

  entries
}
