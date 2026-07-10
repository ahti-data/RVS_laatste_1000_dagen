cat("\n========== RVS TOOL APP STARTUP ==========\n")
cat(paste0("Time: ", Sys.time(), "\n"))
cat(paste0("Working directory: ", getwd(), "\n"))
cat("=============================================\n\n")
flush.console()

# ===== PACKAGE MANAGEMENT: Install & Load =====
cat("[STARTUP] Installing and loading required packages...\n")
flush.console()

packages <- c(
  "shiny",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "purrr",
  "plotly",
  "tibble",
  "openxlsx",
  "writexl",
  "htmlwidgets",
  "DT",
  "stringr",
  "scales",
  "sf"
)

# Identify packages that are not yet installed
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

# Install missing packages if any
if (length(new_packages) > 0) {
  cat(sprintf("[STARTUP] Installing missing packages: %s\n", paste(new_packages, collapse = ", ")))
  flush.console()
  install.packages(
    new_packages,
    lib = Sys.getenv("R_LIBS_USER"),
    repos = "https://cran.r-project.org"
  )
  cat("[STARTUP] Package installation complete.\n")
  flush.console()
}

# Load all required packages
cat("[STARTUP] Loading packages...\n")
flush.console()
lapply(packages, library, character.only = TRUE)
cat("[STARTUP] All packages loaded successfully.\n\n")
flush.console()

# ===== ITERATIE 2 HELPERS (inlined) =====
# (Content moved from former `iteration2.R` to keep everything in one file.)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

detect_app_root <- function() {
  env <- Sys.getenv("RVS_APP_ROOT", unset = Sys.getenv("SHINY_APP_DIR", unset = NA_character_))
  if (!is.na(env) && nzchar(env) && file.exists(file.path(env, "app.R"))) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }

  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (root in unique(c(wd, file.path(wd, "dashboard")))) {
    if (file.exists(file.path(root, "app.R")) && dir.exists(file.path(root, "data"))) {
      return(normalizePath(root, winslash = "/", mustWork = FALSE))
    }
  }

  wd
}

APP_ROOT <- detect_app_root()
cat(sprintf("[startup] APP_ROOT=%s, getwd()=%s\n", APP_ROOT, normalizePath(getwd(), winslash = "/", mustWork = FALSE)))
flush.console()

resolve_existing_path <- function(candidates) {
  expand_path <- function(path) {
    if (is.na(path) || !nzchar(path)) return(character(0))
    if (grepl("^(/|[A-Za-z]:)", path)) return(path)
    unique(c(
      path,
      file.path(APP_ROOT, path),
      file.path(getwd(), path)
    ))
  }

  expanded <- unique(unlist(lapply(candidates, expand_path), use.names = FALSE))
  hit <- expanded[file.exists(expanded)]
  if (length(hit) == 0) return(NA_character_)
  normalizePath(hit[[1]], winslash = "/", mustWork = FALSE)
}

source_util <- function(rel_path) {
  path <- resolve_existing_path(c(rel_path, file.path("dashboard", rel_path)))
  if (is.na(path)) {
    stop("Cannot find utility file: ", rel_path)
  }
  source(path, local = FALSE)
}

source_util("utils/format_thinkcell_download.R")
source_util("utils/chart_downloads.R")
source_util("data/metadata/brand_colors.R")

demographic_cols_iteration2 <- c(
  "doodsoorzaak",
  "age_cat",
  "geslacht",
  "inkomen_klasse",
  "seswoa_cat",
  "migratie_achtergrond",
  "huishoudsamenstelling",
  "stedgem",
  "wlz_start_period",
  "wlz_before_heeft_heup_totaal"
)

pretty_default_iteration2 <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish() |>
    stringr::str_to_title()
}

pretty_sheet <- function(x) {
  dplyr::recode(
    x,
    top_20_codes_operatie_1000 = "Top operatieproducten | 1000 dagen",
    top_20_codes_operatie_30 = "Top operatieproducten | 30 dagen",
    top_20_codes_activit_1000 = "Top zorgactiviteiten | 1000 dagen",
    top_20_codes_activit_30 = "Top zorgactiviteiten | 30 dagen",
    wlz = "WLZ",
    wlz_corrected = "WLZ gecorrigeerd",
    zvw = "ZVW",
    zvw_corrected = "ZVW gecorrigeerd",
    msz_prestaties = "MSZ prestaties",
    msz_prestaties_corrected = "MSZ prestaties gecorrigeerd",
    msz_prestaties_diag = "MSZ prestatie diagnostiek",
    msz_activit_diag = "MSZ activiteit diagnostiek",
    msz_addon_oncology_total_cancer = "MSZ add-on oncologie totaal, overleden aan kanker",
    msz_addon_oncology_cancer = "MSZ add-on oncologiegroepen, overleden aan kanker",
    msz_addon_oncology_total = "MSZ add-on oncologie totaal",
    msz_addon = "MSZ add-ons",
    huisartsdecltab = "Huisarts declaraties",
    msz_prestatie_diagnostiek = "MSZ prestatie diagnostiek",
    .default = pretty_default_iteration2(x)
  )
}

stat_labels_iteration2 <- c(
  sum_totaal_groep = "Totale som",
  n_totaal_gebruikers = "Aantal gebruikers",
  aandeel_gebruikers_berekend = "Aandeel gebruikers",
  gemiddelde_per_gebruiker_berekend = "Gemiddelde per gebruiker",
  gemiddelde_per_persoon_berekend = "Gemiddelde per persoon",
  gemiddelde_per_persoon = "Gemiddelde per persoon (export)"
)

pretty_stat <- function(x) {
  unname(stat_labels_iteration2[x] %||% pretty_default_iteration2(x))
}

pretty_code_metric <- function(x) {
  dplyr::recode(
    x,
    n_totaal_gebruikers = "Aantal gebruikers",
    n_totaal_declaraties = "Aantal declaraties",
    gebruikers_per_persoon = "Aantal gebruikers / aantal personen",
    declaraties_per_persoon = "Aantal declaraties / aantal personen",
    sum_totaal_groep = "Totale kosten",
    sum_per_gebruiker = "Kosten per gebruiker",
    .default = pretty_default_iteration2(x)
  )
}

pretty_vektmszsettingzpk <- function(x) {
  dplyr::recode(
    as.character(x),
    "1" = "Poliklinisch",
    "2" = "Dagbehandeling",
    "3" = "Klinisch",
    "9" = "missing",
    .default = as.character(x)
  )
}

normalize_it3_filter_value <- function(x) {
  val <- stringr::str_trim(as.character(x))
  val[val == "" | val == "NA"] <- NA_character_
  sub("\\.0+$", "", val)
}

it3_zpk_prestatie_colname <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  first_existing(names(df), c("prestatie_type", "Prestatie_type", "prestatietype", "PrestatieType"))
}

filter_it3_zpk_eq <- function(df, col, selected) {
  if (!col %in% names(df)) return(df)
  sel <- normalize_it3_filter_value(selected)
  if (length(sel) != 1 || is.na(sel)) return(df)
  df |>
    dplyr::filter(normalize_it3_filter_value(.data[[col]]) == sel)
}

it3_zpk_group_cols <- function(df) {
  intersect(
    c("died", "cohort", "zpk_category", "t", "vektmszsettingzpk", "bin_size", "n_totaal_population"),
    names(df)
  )
}

it3_zpk_combine_prestatie_types <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  group_cols <- it3_zpk_group_cols(df)
  sum_cols <- intersect(
    c("n_totaal_declaraties", "sum_totaal_groep"),
    names(df)
  )
  if (length(group_cols) == 0 || length(sum_cols) == 0) return(df)

  out <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_cols), ~ sum(numericize(.x), na.rm = TRUE)),
      .groups = "drop"
    )

  if ("prestatie_type" %in% names(df)) {
    out$prestatie_type <- "All"
  }

  out
}

pretty_it3_zpk_metric <- function(x) {
  dplyr::recode(
    x,
    n_totaal_gebruikers = "Aantal gebruikers",
    n_totaal_declaraties = "Aantal declaraties",
    sum_totaal_groep = "Totale kosten",
    median_cost_per_declaratie = "Mediaan kosten per declaratie",
    gemiddelde_kosten_per_persoon = "Gemiddelde kosten per persoon",
    .default = as.character(x)
  )
}

it3_zpk_metric_choices <- c(
  "Aantal gebruikers" = "n_totaal_gebruikers",
  "Aantal declaraties" = "n_totaal_declaraties",
  "Totale kosten" = "sum_totaal_groep",
  "Mediaan kosten per declaratie" = "median_cost_per_declaratie",
  "Gemiddelde kosten per persoon" = "gemiddelde_kosten_per_persoon"
)

it3_zpk_metrics_unavailable_for_all <- c("n_totaal_gebruikers", "median_cost_per_declaratie")

it3_zpk_is_all_prestatie <- function(prestatie_type) {
  prest_sel <- normalize_it3_filter_value(prestatie_type)
  length(prest_sel) == 1 && identical(prest_sel, "All")
}

it3_zpk_metric_unavailable_for_all <- function(metric, prestatie_type) {
  it3_zpk_is_all_prestatie(prestatie_type) && metric %in% it3_zpk_metrics_unavailable_for_all
}

it3_zpk_empty_plot <- function(message) {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE, fixedrange = TRUE),
      yaxis = list(visible = FALSE, fixedrange = TRUE),
      annotations = list(
        list(
          text = message,
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 14, color = "#4B5563")
        )
      )
    ) |>
    plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
}

it3_zpk_format_metric <- function(x, metric) {
  if (identical(metric, "gemiddelde_kosten_per_persoon")) {
    scales::number(x, big.mark = ",", decimal.mark = ".", accuracy = 0.01)
  } else {
    scales::comma(x, big.mark = ",", decimal.mark = ".")
  }
}

it3_zpk_metric_values <- function(df, metric) {
  if (identical(metric, "gemiddelde_kosten_per_persoon")) {
    pop <- numericize(df[["n_totaal_population"]])
    cost <- numericize(df[["sum_totaal_groep"]])
    dplyr::if_else(is.na(pop) | pop == 0, NA_real_, cost / pop)
  } else {
    numericize(df[[metric]])
  }
}

format_code_value <- function(value, metric_name) {
  if (metric_name %in% c("gebruikers_per_persoon", "declaraties_per_persoon")) {
    scales::number(value, big.mark = ",", decimal.mark = ".", accuracy = 0.0001)
  } else {
    scales::comma(value, big.mark = ",", decimal.mark = ".")
  }
}

wrap_hover <- function(x, width = 52) {
  x |>
    stringr::str_wrap(width = width) |>
    stringr::str_replace_all("\n", "<br>")
}

clean_code_text <- function(x) {
  as.character(x) |>
    stringr::str_replace_all('^\"+|\"+$', "") |>
    stringr::str_squish()
}

sheet_allowed_splits <- function(sheet) {
  dplyr::case_when(
    sheet %in% c("zvw", "msz_prestaties") ~ "all_demographic",
    sheet == "wlz" ~ "wlz_start_period",
    sheet == "msz_addon_oncology_total_cancer" ~ "age_income",
    TRUE ~ "none"
  )
}

allowed_split_columns <- function(sheet, columns) {
  rule <- sheet_allowed_splits(sheet)
  allowed <- switch(
    rule,
    all_demographic = c(
      "age_cat",
      "geslacht",
      "inkomen_klasse",
      "seswoa_cat",
      "migratie_achtergrond",
      "huishoudsamenstelling",
      "stedgem",
      "wlz_start_period"
    ),
    wlz_start_period = "wlz_start_period",
    age_income = c("age_cat", "inkomen_klasse"),
    character(0)
  )
  intersect(allowed, columns)
}

pretty_metric_name <- function(x, sheet = NULL) {
  x_clean <- as.character(x)
  if (!is.null(sheet) && sheet %in% c("zvw", "zvw_corrected")) {
    x_clean <- x_clean |>
      stringr::str_replace("^nopzvwk", "") |>
      stringr::str_replace("^zvwk", "")
  }

  x_clean |>
    stringr::str_replace("^gebruikt_", "gebruik ") |>
    stringr::str_replace("^heeft_", "") |>
    stringr::str_replace("^kosten_", "kosten ") |>
    stringr::str_replace("^bedrag", "bedrag ") |>
    stringr::str_replace("^n_", "aantal ") |>
    stringr::str_replace("^zvwk", "ZVW kosten ") |>
    stringr::str_replace("^nopzvwk", "Niet-ZVW kosten ") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish() |>
    stringr::str_to_title() |>
    stringr::str_replace_all("Msz", "MSZ") |>
    stringr::str_replace_all("Zvw", "ZVW") |>
    stringr::str_replace_all("Wlz", "WLZ") |>
    stringr::str_replace_all("I[cC]", "IC") |>
    stringr::str_replace_all("Aaa", "AAA")
}

pretty_split_name <- function(x) {
  dplyr::recode(
    x,
    age_cat = "Leeftijd",
    inkomen_klasse = "Inkomen",
    geslacht = "Geslacht",
    seswoa_cat = "SES-WOA",
    migratie_achtergrond = "Migratieachtergrond",
    huishoudsamenstelling = "Huishoudsamenstelling",
    stedgem = "Stedelijkheid",
    wlz_start_period = "WLZ-startperiode",
    provincie = "Provincie",
    burgstaat = "Burgerlijke staat",
    cohort = "Cohort",
    used_any_acp_2years = "ACP-gebruik (2 jaar)",
    huisarts_consults_cat = "Huisartsconsulten",
    doodsoorzaak = "Doodsoorzaak",
    .default = pretty_default_iteration2(x)
  )
}

pretty_value <- function(column, value) {
  value <- as.character(value)
  if (identical(column, "age_cat")) {
    return(dplyr::recode(
      value,
      `2` = "18-29 jaar",
      `3` = "30-39 jaar",
      `4` = "40-49 jaar",
      `5` = "50-59 jaar",
      `6` = "60-69 jaar",
      `7` = "70-79 jaar",
      `8` = "80-89 jaar",
      `9` = "90+ jaar",
      .default = value
    ))
  }
  if (identical(column, "inkomen_klasse")) {
    return(dplyr::recode(
      value,
      `400+` = "400%+",
      `280_400` = "280-400%",
      `120_280` = "120-280%",
      tot_120 = "Tot 120%",
      Overig = "Overig",
      .default = value
    ))
  }
  value
}

ordered_values <- function(x) {
  vals <- unique(as.character(x))
  vals <- vals[!is.na(vals)]
  preferred <- c("all", "Overleden", "In leven", "2019", "2023")
  c(intersect(preferred, vals), sort(setdiff(vals, preferred)))
}

ordered_split_values <- function(column, values) {
  values <- setdiff(unique(as.character(values)), c(NA_character_, "all"))
  if (identical(column, "age_cat")) {
    return(intersect(as.character(2:9), values))
  }
  if (identical(column, "inkomen_klasse")) {
    return(intersect(c("tot_120", "120_280", "280_400", "400+", "Overig"), values))
  }
  ordered_values(values)
}

axis_label <- function(x, width = 18) {
  stringr::str_wrap(as.character(x), width = width)
}

heatmap_split_values <- function(column, values) {
  values <- setdiff(unique(as.character(values)), c(NA_character_, "all"))
  if (identical(column, "age_cat")) {
    return(intersect(as.character(2:9), values))
  }
  if (identical(column, "inkomen_klasse")) {
    return(intersect(c("tot_120", "120_280", "280_400", "400+", "Overig"), values))
  }
  ordered_values(values)
}

view_choices_for <- function(df) {
  choices <- c()
  if ("bin_size" %in% names(df)) {
    bin_values <- as.character(df$bin_size)
    t_values <- if ("t" %in% names(df)) as.character(df$t) else rep(NA_character_, nrow(df))
    if (any(bin_values == "30" & t_values != "-1", na.rm = TRUE)) choices <- c(choices, "Maandelijks" = "maandelijks")
    if (any(bin_values == "30" & t_values == "-1", na.rm = TRUE)) choices <- c(choices, "Laatste 30 dagen" = "laatste_30")
    if ("1000" %in% bin_values) choices <- c(choices, "Laatste 1000 dagen" = "laatste_1000")
  }
  if (length(choices) == 0) choices <- c("Totaal" = "totaal")
  choices
}

stat_choices_for <- function(df, name) {
  types <- unique(as.character(df$type[df$name == name]))
  choices <- c()
  if ("sum_totaal_groep" %in% types) choices <- c(choices, "sum_totaal_groep")
  if ("n_totaal_gebruikers" %in% types) choices <- c(choices, "n_totaal_gebruikers")
  if ("n_totaal_gebruikers" %in% types) choices <- c(choices, "aandeel_gebruikers_berekend")
  if (all(c("sum_totaal_groep", "n_totaal_gebruikers") %in% types)) {
    choices <- c(choices, "gemiddelde_per_gebruiker_berekend")
  }
  if ("sum_totaal_groep" %in% types) choices <- c(choices, "gemiddelde_per_persoon_berekend")
  if ("gemiddelde_per_persoon" %in% types) choices <- c(choices, "gemiddelde_per_persoon")
  unique(choices)
}

top_metric_choices_for <- function(df) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  choices <- setdiff(numeric_cols, c("cohort", "n_instellingen"))
  if (all(c("cohort", "died", "n_totaal_gebruikers") %in% names(df))) {
    choices <- c(choices, "gebruikers_per_persoon")
  }
  if (all(c("cohort", "died", "n_totaal_declaraties") %in% names(df))) {
    choices <- c(choices, "declaraties_per_persoon")
  }
  unique(choices)
}

top_total_people <- function(cohort, died) {
  cohort_chr <- as.character(cohort)
  died_chr <- as.character(died)
  dplyr::case_when(
    cohort_chr == "2019" & died_chr == "In leven" ~ 1500080,
    cohort_chr == "2019" & died_chr == "Overleden" ~ 150030,
    cohort_chr == "2023" & died_chr == "In leven" ~ 1674150,
    cohort_chr == "2023" & died_chr == "Overleden" ~ 167420,
    TRUE ~ NA_real_
  )
}

population_label <- function(x) {
  dplyr::recode(as.character(x), `In leven` = "Controle", .default = as.character(x))
}

population_palette <- c("Overleden" = "#1F77B4", "Controle" = "#9ECAE1")

lighten_color <- function(color, amount = 0.45) {
  rgb <- grDevices::col2rgb(color) / 255
  lighter <- rgb + (1 - rgb) * amount
  grDevices::rgb(lighter[1, ], lighter[2, ], lighter[3, ])
}

top_period_palette <- c(
  "Laatste 1000 dagen" = "#5D8F73",
  "Laatste 30 dagen" = "#C47C4E",
  "Laatste 1000 dagen | Overleden" = "#5D8F73",
  "Laatste 1000 dagen | Controle" = "#B7D4C2",
  "Laatste 30 dagen | Overleden" = "#C47C4E",
  "Laatste 30 dagen | Controle" = "#E5B894",
  "Overleden" = "#4A96CF",
  "Controle" = "#A7CCE8",
  "Ratio 1000 / 30" = "#4A96CF"
)

period_label <- function(x) {
  dplyr::recode(
    as.character(x),
    laatste_1000_dagen = "Laatste 1000 dagen",
    laatste_30_dagen = "Laatste 30 dagen",
    .default = pretty_default_iteration2(x)
  )
}

domain_order_zvw <- c(
  "zvwktotaal",
  "zvwkziekenhuis",
  "zvwkfarmacie",
  "zvwkwykverpleging",
  "zvwkhuisarts",
  "zvwkhulpmiddel",
  "zvwkggzzpmtotaal",
  "nopzvwkhuisartsconsult",
  "nopzvwkhuisartsinschrijf",
  "nopzvwkhuisartsoverig"
)

build_palette <- function(n) {
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 55, c = 85)[seq_len(n)]
}

sanitize_filename <- function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

build_export_name <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  sanitize_filename(paste(parts, collapse = "_"))
}

combine_series <- function(..., sep = " | ") {
  parts <- data.frame(..., stringsAsFactors = FALSE)
  apply(parts, 1, function(row) {
  paste(row[!is.na(row) & nzchar(as.character(row))], collapse = sep)
  })
}

save_plot_png <- function(file, plot_obj) {
  ggplot2::ggsave(
    file,
    plot = plot_obj,
    scale = 0.7,
    width = 14,
    height = 10,
    dpi = 300,
    bg = "transparent"
  )
}

numericize <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

metric_value_from_wide <- function(df, stat) {
  for (col in c("sum_totaal_groep", "n_totaal_gebruikers", "gemiddelde_per_persoon")) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  dplyr::case_when(
    stat == "sum_totaal_groep" ~ df[["sum_totaal_groep"]],
    stat == "n_totaal_gebruikers" ~ df[["n_totaal_gebruikers"]],
    stat == "aandeel_gebruikers_berekend" ~ df[["n_totaal_gebruikers"]] / df[["n_totaal_num"]],
    stat == "gemiddelde_per_gebruiker_berekend" ~ df[["sum_totaal_groep"]] / df[["n_totaal_gebruikers"]],
    stat == "gemiddelde_per_persoon_berekend" ~ df[["sum_totaal_groep"]] / df[["n_totaal_num"]],
    stat == "gemiddelde_per_persoon" ~ df[["gemiddelde_per_persoon"]],
    TRUE ~ NA_real_
  )
}

aggregate_metric_data <- function(sheet, names_keep = NULL, stat = "sum_totaal_groep",
                                  bin_size_filter = NULL, t_value = NULL, cohort_filter = NULL,
                                  died_filter = NULL, split_col = NULL, split_values = NULL,
                                  keep_total_splits = TRUE) {
  df <- get_sheet(sheet)
  if (!is.null(names_keep)) df <- df |> dplyr::filter(name %in% names_keep)
  if (!is.null(bin_size_filter) && "bin_size" %in% names(df)) df <- df |> dplyr::filter(as.character(bin_size) == as.character(bin_size_filter))
  if (!is.null(t_value) && "t" %in% names(df)) df <- df |> dplyr::filter(as.character(t) == as.character(t_value))
  if (!is.null(cohort_filter) && "cohort" %in% names(df)) df <- df |> dplyr::filter(as.character(cohort) %in% as.character(cohort_filter))
  if (!is.null(died_filter) && "died" %in% names(df)) df <- df |> dplyr::filter(as.character(died) %in% as.character(died_filter))

  dims <- intersect(demographic_cols_iteration2, names(df))
  for (col in dims) {
    if (!is.null(split_col) && identical(col, split_col)) {
      vals <- split_values %||% ordered_split_values(col, df[[col]])
      df <- df |> dplyr::filter(as.character(.data[[col]]) %in% vals)
    } else if (isTRUE(keep_total_splits) && "all" %in% as.character(df[[col]])) {
      df <- df |> dplyr::filter(as.character(.data[[col]]) == "all")
    }
  }

  if (nrow(df) == 0) return(df)
  id_cols <- setdiff(names(df), c("variable", "type", "value"))
  wide <- df |>
    dplyr::mutate(value_num_raw = numericize(value), n_totaal_num = numericize(n_totaal)) |>
    dplyr::select(dplyr::all_of(id_cols), n_totaal_num, type, value_num_raw) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = value_num_raw,
      values_fn = list(value_num_raw = ~ dplyr::first(.x))
    )
  wide$value_num <- metric_value_from_wide(wide, stat)
  wide$maat <- pretty_stat(stat)
  wide
}

metric_choices_basic <- c(
  sum_totaal_groep = "Totale som",
  n_totaal_gebruikers = "Aantal gebruikers",
  aandeel_gebruikers_berekend = "Aandeel gebruikers",
  gemiddelde_per_gebruiker_berekend = "Gemiddelde per gebruiker",
  gemiddelde_per_persoon_berekend = "Gemiddelde per persoon"
)

diag_activity_names <- c(
  "n_ct_scan", "n_echo", "n_mri", "n_overig", "n_pet_spect",
  "n_punctie_biopsie", "n_radiologie", "n_scopie", "n_zpk_8"
)

intervention_names <- c(
  "n_add_on_ic", "n_aaa_kijkoperatie", "n_aaa_operatie", "n_aaa_totaal",
  "n_heup_operatie", "n_heup_prothese", "n_heup_totaal",
  "kosten_add_on_ic", "kosten_aaa_kijkoperatie", "kosten_aaa_operatie",
  "kosten_aaa_totaal", "kosten_heup_operatie", "kosten_heup_prothese",
  "kosten_heup_totaal"
)

is_cost_outcome <- function(name) {
  stringr::str_detect(
    as.character(name),
    "^(zvwk|nopzvwk|kosten_|bedrag|vektmszvergoedbedrag)"
  )
}

is_cost_stat <- function(stat) {
  stat %in% c(
    "sum_totaal_groep",
    "gemiddelde_per_gebruiker_berekend",
    "gemiddelde_per_persoon_berekend",
    "gemiddelde_per_persoon"
  )
}

first_existing <- function(cols, candidates) {
  hit <- intersect(candidates, cols)
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

first_preferred <- function(preferred, choices) {
  hit <- intersect(preferred, choices)
  if (length(hit) > 0) hit[[1]] else choices[[1]]
}

sheet_names_iteration2 <- character(0)
cache_env_iteration2 <- new.env(parent = emptyenv())
data_path_iteration2 <- NA_character_

safe_read_sheet_iteration2 <- function(sheet) {
  tryCatch(
    openxlsx::read.xlsx(data_path_iteration2, sheet = sheet),
    error = function(e) {
      warning(sprintf("Failed to read sheet %s: %s", sheet, e$message))
      data.frame()
    }
  )
}

get_sheet <- function(sheet) {
  key <- paste0("sheet__", sheet)
  if (!exists(key, envir = cache_env_iteration2, inherits = FALSE)) {
    assign(key, safe_read_sheet_iteration2(sheet), envir = cache_env_iteration2)
  }
  get(key, envir = cache_env_iteration2, inherits = FALSE)
}

choice_names <- function(values, labeler = identity) {
  vals <- unique(as.character(values))
  vals <- vals[!is.na(vals)]
  stats::setNames(vals, vapply(vals, labeler, character(1)))
}

corrected_sheet_for <- function(sheet) {
  candidate <- paste0(sheet, "_corrected")
  if (candidate %in% sheet_names_iteration2) candidate else NA_character_
}

build_agg_version_annotations <- function(df, view) {
  if (!"versie" %in% names(df) || dplyr::n_distinct(df$versie, na.rm = TRUE) <= 1) {
    return(list())
  }

  populations <- if ("died" %in% names(df)) population_label(df$died) else "Totaal"
  populations <- populations[!is.na(populations)]
  pop_order <- c("Overleden", "Controle", sort(setdiff(unique(populations), c("Overleden", "Controle"))))
  pop_order <- pop_order[pop_order %in% unique(populations)]
  if (length(pop_order) == 0) {
    return(list())
  }

  is_monthly <- identical(view, "maandelijks") && "t" %in% names(df) && any(!is.na(df$t))
  sample_html <- function(label, color, variant) {
    if (is_monthly) {
      line_html <- if (identical(variant, "corrected")) "&#9473; &#9473; &#9473;" else "&#9473;&#9473;&#9473;"
      paste0("<span style='color:", color, ";'>", line_html, "</span>&nbsp;", label)
    } else {
      paste0("<span style='color:", color, ";'>&#9632;</span>&nbsp;", label)
    }
  }

  y_positions <- seq(-0.08, by = -0.075, length.out = length(pop_order))
  lapply(seq_along(pop_order), function(i) {
    population <- pop_order[[i]]
    base_color <- population_palette[[population]] %||% "#4b5563"
    corrected_color <- if (is_monthly) base_color else lighten_color(base_color)
    row_text <- paste(
      sample_html("Niet gecorrigeerd", base_color, "observed"),
      sample_html("Inflatiecorrectie", corrected_color, "corrected"),
      sep = "&nbsp;&nbsp;&nbsp;&nbsp;"
    )

    list(
      x = 0,
      y = y_positions[[i]],
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "top",
      align = "left",
      showarrow = FALSE,
      text = paste0(
        "<span style='color:", base_color, "; font-weight:600;'>", population, ":</span>",
        "&nbsp;&nbsp;",
        row_text
      ),
      font = list(size = 12, color = "#4b5563")
    )
  })
}


# Load Iteratie 2 workbook metadata at startup (UI needs aggregate_sheets before server runs).
data_path_iteration2 <- resolve_existing_path(c(
  "data/data_iteration_2/output.xlsx",
  "dashboard/data/data_iteration_2/output.xlsx",
  "output.xlsx",
  "data/output.xlsx"
))
if (is.na(data_path_iteration2)) {
  stop("Iteratie 2: kan output.xlsx niet vinden in data/data_iteration_2/output.xlsx.")
}

sheet_names_iteration2 <- openxlsx::getSheetNames(data_path_iteration2)

sheet_preview_iteration2 <- dplyr::bind_rows(lapply(sheet_names_iteration2, function(sheet) {
  df <- safe_read_sheet_iteration2(sheet)
  cols <- names(df)
  data.frame(
    sheet = sheet,
    label = pretty_sheet(sheet),
    n_rows = nrow(df),
    n_cols = ncol(df),
    columns = paste(cols, collapse = ", "),
    is_aggregate = all(c("name", "type", "value") %in% cols),
    is_top_code = startsWith(sheet, "top_20_codes_"),
    stringsAsFactors = FALSE
  )
}))

aggregate_sheets <- sheet_preview_iteration2 |>
  dplyr::filter(
    is_aggregate,
    sheet != "wlz_msz_heup",
    !stringr::str_ends(sheet, "_corrected")
  ) |>
  dplyr::arrange(label)

top_code_sheets <- sheet_preview_iteration2 |>
  dplyr::filter(is_top_code) |>
  dplyr::arrange(label)

iteration2_header <- function() {
  tags$div(
    style = paste(
      "padding: 12px 18px 6px 18px;",
      "color: #4b5563;",
      "font-size: 14px;",
      "border-bottom: 1px solid #e5e7eb;"
    ),
    "Interactieve verkenning van zorggebruik, zorgkosten en meest voorkomende MSZ-codes in de laatste 1000 dagen."
  )
}

iteration2_panels <- function() {
  list(
    tabPanel(
      "Uitkomsten",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("agg_sheet", "Dataset", choices = choice_names(aggregate_sheets$sheet, pretty_sheet)),
          uiOutput("agg_name_ui"),
          uiOutput("agg_stat_ui"),
          uiOutput("agg_corrected_ui"),
          uiOutput("agg_view_ui"),
          uiOutput("agg_cohort_ui"),
          uiOutput("agg_died_ui"),
          uiOutput("agg_split_ui"),
          uiOutput("agg_split_values_ui")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("plot_agg", height = "620px"),
          br(),
          div(
            style = "display: flex; gap: 12px; align-items: center; flex-wrap: wrap;",
            chart_data_downloads_ui(
              "iter2_agg_dl",
              chart_type = "line",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            ),
            downloadButton("dl_agg_plot", "Grafiek downloaden")
          )
        )
      )
    ),
    tabPanel(
      "Heatmap",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("hm_split_ui"),
          uiOutput("hm_stat_ui"),
          uiOutput("hm_cohort_ui"),
          uiOutput("hm_rows_ui")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("plot_heatmap", height = "720px"),
          br(),
          div(
            style = "display: flex; gap: 12px; align-items: center; flex-wrap: wrap;",
            chart_data_downloads_ui(
              "iter2_hm_dl",
              chart_type = "scatter",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            ),
            downloadButton("dl_heatmap_plot", "Grafiek downloaden")
          )
        )
      )
    ),
    tabPanel(
      "Top 20 codes",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons("top_sheet", "Dataset", choices = choice_names(top_code_sheets$sheet, pretty_sheet)),
          uiOutput("top_metric_ui"),
          radioButtons(
            "top_mode",
            "Tijdvenster",
            choices = c(
              "Laatste 1000 versus laatste 30" = "perioden",
              "Alleen laatste 1000 dagen" = "alleen_1000",
              "Alleen laatste 30 dagen" = "alleen_30",
              "Ratio 1000 / 30" = "ratio"
            ),
            selected = "perioden"
          ),
          uiOutput("top_cohort_ui"),
          uiOutput("top_category_filter"),
          uiOutput("top_population_ui")
        ),
        mainPanel(
          width = 9,
          plotlyOutput("plot_top", height = "720px"),
          br(),
          div(
            style = "display: flex; gap: 12px; align-items: center; flex-wrap: wrap;",
            chart_data_downloads_ui(
              "iter2_top_dl",
              chart_type = "bar",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            ),
            downloadButton("dl_top_plot", "Grafiek downloaden")
          )
        )
      )
    )
  )
}

#
# ===== ITERATIE 3 HELPERS / DATA (inlined) =====
#

pick_first_existing <- function(paths) {
  resolve_existing_path(paths)
}

resolve_iteration3_subfile <- function(subdir, filename) {
  resolve_existing_path(c(
    file.path("data", "data_iteration_3", subdir, filename),
    file.path("dashboard", "data", "data_iteration_3", subdir, filename),
    file.path("data", "data_iteration_3", filename),
    file.path("dashboard", "data", "data_iteration_3", filename)
  ))
}

resolve_iteration3_file <- function(pattern, prefer_basename = NULL) {
  dirs <- resolve_existing_path(c(
    file.path("data", "data_iteration_3"),
    file.path("dashboard", "data", "data_iteration_3")
  ))
  if (is.na(dirs)) return(NA_character_)

  candidates <- Sys.glob(file.path(dirs, pattern))
  candidates <- candidates[!grepl("^~\\$", basename(candidates))]
  if (length(candidates) == 0) return(NA_character_)

  if (!is.null(prefer_basename) && nzchar(prefer_basename)) {
    preferred <- candidates[basename(candidates) == prefer_basename]
    if (length(preferred) > 0) return(preferred[[1]])
  }

  # Dated backup copies may linger on the server; prefer the newest match.
  info <- file.info(candidates)
  candidates <- candidates[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
  candidates[[1]]
}

read_iteration3_xlsx <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(tibble::tibble())
  df <- readxl::read_excel(path, .name_repair = "unique")
  df <- dplyr::as_tibble(df)

  num_cols <- intersect(
    c(
      "t",
      "n_totaal_gebruikers",
      "n_totaal_activiteiten",
      "sum_totaal_groep",
      "median_cost_per_declaratie",
      "n_totaal_population",
      "ranking",
      "n_totaal_gebruikers_30d",
      "n_totaal_activiteiten_30d",
      "n_totaal_gebruikers_Overleden",
      "n_totaal_activiteiten_Overleden",
      "n_totaal_gebruikers_In_leven",
      "n_totaal_activiteiten_In_leven",
      "n_totaal_gebruikers_1000d",
      "n_totaal_activiteiten_1000d",
      "n_totaal_declaraties",
      "n_totaal_declaraties_30d",
      "n_totaal_declaraties_Overleden",
      "n_totaal_declaraties_In_leven",
      "n_totaal_declaraties_1000d",
      "age_acp_user",
      "n_users_acp_consults_2years",
      "n_population",
      "mean_costs_all",
      "mean_costs_users",
      "median_costs_all",
      "median_costs_users"
    ),
    names(df)
  )
  for (col in num_cols) df[[col]] <- numericize(df[[col]])

  for (col in intersect(
    c(
      "prestatie_type", "died", "cohort", "bin_size", "zpk_category",
      "geslacht", "split_by", "group", "cost_type", "ranked_by", "cost_bin"
    ),
    names(df)
  )) {
    df[[col]] <- as.character(df[[col]])
  }

  df
}

it3_zvwk_ref_line_choices <- c(
  "Gemiddelde kosten (alle)" = "mean_costs_all",
  "Gemiddelde kosten (gebruikers)" = "mean_costs_users",
  "Mediaan kosten (alle)" = "median_costs_all",
  "Mediaan kosten (gebruikers)" = "median_costs_users"
)

parse_zvwk_cost_bin_bounds <- function(label) {
  label <- as.character(label)
  if (!nzchar(label)) return(c(lower = NA_real_, upper = NA_real_))

  if (grepl(" - ", label, fixed = TRUE)) {
    parts <- strsplit(label, " - ", fixed = TRUE)[[1]]
    lower <- suppressWarnings(as.numeric(parts[[1]]))
    upper_raw <- parts[[2]]
    upper <- if (toupper(upper_raw) %in% c("INF", "INFINITY")) {
      Inf
    } else {
      suppressWarnings(as.numeric(upper_raw))
    }
    return(c(lower = lower, upper = upper))
  }

  val <- suppressWarnings(as.numeric(label))
  if (!is.na(val)) return(c(lower = val, upper = val))
  c(lower = NA_real_, upper = NA_real_)
}

it3_zvwk_bin_for_value <- function(bin_labels, value) {
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value) || length(bin_labels) == 0) return(NA_character_)

  for (lbl in bin_labels) {
    bounds <- parse_zvwk_cost_bin_bounds(lbl)
    lower <- bounds[["lower"]]
    upper <- bounds[["upper"]]
    if (is.na(lower)) next

    if (is.infinite(upper)) {
      if (value >= lower) return(lbl)
    } else if (identical(lbl, "0") || (upper == lower && !is.na(upper))) {
      if (value == lower) return(lbl)
    } else if (!is.na(upper) && value >= lower && value < upper) {
      return(lbl)
    }
  }

  NA_character_
}

prepare_it3_zvwk_hist_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
  if (!all(c("cost_bin", "n_population") %in% names(df))) return(tibble::tibble())

  df <- df |>
    dplyr::mutate(
      cost_bin = as.character(cost_bin),
      pop_num = numericize(n_population)
    ) |>
    dplyr::filter(!is.na(cost_bin), nzchar(cost_bin), cost_bin != "NA", !is.na(pop_num))

  if (nrow(df) == 0) return(tibble::tibble())

  bin_levels <- unique(df$cost_bin)
  df |>
    dplyr::mutate(cost_bin_label = factor(cost_bin, levels = bin_levels))
}

it3_cost_agg_total_dims <- c(
  "doodsoorzaak",
  "age_cat",
  "geslacht",
  "inkomen_klasse",
  "seswoa_cat",
  "migratie_achtergrond",
  "huishoudsamenstelling",
  "stedgem"
)

it3_cost_agg_core_cols <- c(
  "bin_size", "t", "cohort", "died", "n_totaal", "variable", "value", "type", "name", "sheet"
)

it3_cost_agg_dim_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character())
  cols <- setdiff(names(df), it3_cost_agg_core_cols)
  cols <- cols[vapply(cols, function(col) {
    vals <- as.character(df[[col]])
    any(!is.na(vals) & nzchar(vals))
  }, logical(1))]
  sort(cols)
}

it3_cost_agg_unique_dim_values <- function(values) {
  vals <- unique(as.character(values))
  vals[!is.na(vals) & nzchar(vals)]
}

it3_cost_agg_varied_dim_cols <- function(df) {
  cols <- it3_cost_agg_dim_cols(df)
  cols[vapply(cols, function(col) {
    length(it3_cost_agg_unique_dim_values(df[[col]])) > 1
  }, logical(1))]
}

it3_cost_dim_choice_values <- function(col, values) {
  vals <- it3_cost_agg_unique_dim_values(values)
  if (length(vals) <= 1) return(vals)

  if ("all" %in% vals) {
    rest <- setdiff(vals, "all")
    if (identical(col, "age_cat") || identical(col, "inkomen_klasse")) {
      rest <- ordered_split_values(col, rest)
    } else {
      rest <- sort(rest)
    }
    return(c("all", rest))
  }

  if (identical(col, "age_cat") || identical(col, "inkomen_klasse")) {
    return(ordered_split_values(col, vals))
  }

  ordered_values(vals)
}

it3_cost_dim_input_id <- function(col) {
  paste0("it3_cost_dim_", col)
}

filter_iteration3_cost_agg_totals <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  for (col in intersect(it3_cost_agg_total_dims, names(df))) {
    df <- df |> dplyr::filter(as.character(.data[[col]]) == "all")
  }
  df
}

normalize_iteration3_cost_agg_sheet <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  num_cols <- c("t", "n_totaal", "value")
  for (col in names(df)) {
    if (col %in% num_cols) {
      df[[col]] <- numericize(df[[col]])
    } else {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
}

it3_cost_agg_join_cols <- function(df) {
  setdiff(names(df), c("type", "value", "variable", "sheet"))
}

it3_cost_agg_format_value <- function(x, metric) {
  if (metric %in% c("kosten_per_persoon", "kosten_per_gebruiker")) {
    scales::number(x, big.mark = ",", decimal.mark = ".", accuracy = 0.01)
  } else if (identical(metric, "prevalentie_gebruik")) {
    scales::percent(x, accuracy = 0.01, big.mark = ",", decimal.mark = ".")
  } else {
    scales::comma(x, big.mark = ",", decimal.mark = ".")
  }
}

it3_cost_metric_label <- function(metric) {
  dplyr::recode(
    metric,
    kosten_per_persoon = "Som per persoon",
    kosten_per_gebruiker = "Som per gebruiker",
    prevalentie_gebruik = "Prevalentie/gebruik",
    .default = unname(pretty_stat(metric))
  )
}

it3_cost_agg_allows_derived_metrics <- function(sheet) {
  if (is.null(sheet) || !nzchar(as.character(sheet))) return(FALSE)
  !grepl("diag", as.character(sheet), ignore.case = TRUE)
}

it3_cost_agg_metric_choices <- function(df, sheet) {
  if (is.null(df) || nrow(df) == 0 || !"type" %in% names(df)) return(character())

  type_vals <- sort(unique(as.character(df$type)))
  type_vals <- type_vals[!is.na(type_vals) & nzchar(type_vals)]
  if (length(type_vals) == 0) return(character())

  choices <- choice_names(type_vals, function(x) unname(pretty_stat(x)))

  if (it3_cost_agg_allows_derived_metrics(sheet)) {
    derived <- c()
    if ("sum_totaal_groep" %in% type_vals) {
      derived <- c(derived, "Som per persoon" = "kosten_per_persoon")
    }
    if (all(c("sum_totaal_groep", "n_totaal_gebruikers") %in% type_vals)) {
      derived <- c(derived, "Som per gebruiker" = "kosten_per_gebruiker")
    }
    if ("n_totaal_gebruikers" %in% type_vals) {
      derived <- c(derived, "Prevalentie/gebruik" = "prevalentie_gebruik")
    }
    choices <- c(choices, derived)
  }

  choices
}

it3_cost_metric_choices_combined <- function(df, sheet, names_keep) {
  if (is.null(df) || nrow(df) == 0 || length(names_keep) == 0) return(character())

  per_name <- lapply(names_keep, function(nm) {
    sub <- df |> dplyr::filter(as.character(.data$name) == nm)
    unname(it3_cost_agg_metric_choices(sub, sheet))
  })
  common <- Reduce(intersect, per_name)
  if (length(common) == 0) return(character())

  full <- it3_cost_agg_metric_choices(
    df |> dplyr::filter(as.character(.data$name) %in% names_keep),
    sheet
  )
  full[unname(full) %in% common]
}

prepare_it3_cost_agg_metric_df <- function(df, metric) {
  if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
  if (!all(c("t", "type", "value") %in% names(df))) return(tibble::tibble())

  derived_metrics <- c("kosten_per_persoon", "kosten_per_gebruiker", "prevalentie_gebruik")

  if (!metric %in% derived_metrics) {
    return(
      df |>
        dplyr::filter(as.character(type) == as.character(metric)) |>
        dplyr::mutate(metric_value = numericize(value))
    )
  }

  sum_df <- df |> dplyr::filter(as.character(type) == "sum_totaal_groep")
  if (nrow(sum_df) == 0) return(tibble::tibble())

  if (identical(metric, "kosten_per_persoon")) {
    return(
      sum_df |>
        dplyr::mutate(
          metric_value = dplyr::if_else(
            is.na(numericize(n_totaal)) | numericize(n_totaal) == 0,
            NA_real_,
            numericize(value) / numericize(n_totaal)
          )
        )
    )
  }

  if (identical(metric, "kosten_per_gebruiker")) {
    join_cols <- it3_cost_agg_join_cols(df)
    gebr_df <- df |>
      dplyr::filter(as.character(type) == "n_totaal_gebruikers") |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(join_cols)),
        gebruikers_value = numericize(value)
      )

    return(
      sum_df |>
        dplyr::mutate(sum_value = numericize(value)) |>
        dplyr::left_join(gebr_df, by = join_cols) |>
        dplyr::mutate(
          metric_value = dplyr::if_else(
            is.na(gebruikers_value) | gebruikers_value == 0,
            NA_real_,
            sum_value / gebruikers_value
          )
        )
    )
  }

  if (identical(metric, "prevalentie_gebruik")) {
    gebr_df <- df |> dplyr::filter(as.character(type) == "n_totaal_gebruikers")
    if (nrow(gebr_df) == 0) return(tibble::tibble())

    return(
      gebr_df |>
        dplyr::mutate(
          metric_value = dplyr::if_else(
            is.na(numericize(n_totaal)) | numericize(n_totaal) == 0,
            NA_real_,
            numericize(value) / numericize(n_totaal)
          )
        )
    )
  }

  tibble::tibble()
}

prepare_it3_cost_agg_export_df <- function(df, metric) {
  out <- prepare_it3_cost_agg_metric_df(df, metric)
  if (nrow(out) == 0) return(out)

  label <- it3_cost_metric_label(metric)
  out |>
    dplyr::mutate(!!label := metric_value) |>
    dplyr::select(-dplyr::any_of(c("metric_value", "type", "sum_value", "gebruikers_value")))
}

it3_map_sheet <- "msz_prestaties"
it3_map_name <- "vektmszvergoedbedragzvw"

normalize_provincie_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  dplyr::recode(
    x,
    `Friesland` = "Fryslân",
    `Fryslan` = "Fryslân",
    .default = x
  )
}

load_it3_provinces_sf <- function() {
  geo_path <- resolve_existing_path(c(
    file.path("data", "geo", "provinces.json"),
    file.path("dashboard", "data", "geo", "provinces.json")
  ))
  if (is.na(geo_path)) {
    log_msg("[geo] provinces.json not found")
    return(NULL)
  }
  tryCatch({
    sf::st_read(geo_path, quiet = TRUE)
  }, error = function(e) {
    log_msg(sprintf("[geo] Failed to load provinces: %s", e$message))
    NULL
  })
}

it3_map_cost_base_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  df <- df |>
    dplyr::filter(
      as.character(.data$sheet) == it3_map_sheet,
      as.character(.data$name) == it3_map_name,
      as.character(.data$bin_size) == "1000"
    )
  if (nrow(df) == 0) return(df)

  filter_cols <- c(it3_cost_agg_total_dims, "wlz_start_period", "used_any_acp_2years")
  for (col in intersect(filter_cols, names(df))) {
    df <- df |> dplyr::filter(as.character(.data[[col]]) == "all")
  }

  if ("provincie" %in% names(df)) {
    df <- df |>
      dplyr::filter(
        as.character(.data$provincie) != "all",
        !tolower(as.character(.data$provincie)) %in% c("onbekend", "na", "")
      )
  }

  if ("t" %in% names(df) && "-1" %in% as.character(df$t)) {
    df <- df |> dplyr::filter(as.character(.data$t) == "-1")
  }

  df
}

build_it3_cost_map_sf <- function(metric_df, provincies_sf) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(NULL)
  if (is.null(provincies_sf)) return(NULL)

  prov_data <- metric_df |>
    dplyr::transmute(
      provincie = normalize_provincie_name(.data$provincie),
      metric_value = .data$metric_value
    ) |>
    dplyr::distinct(.data$provincie, .keep_all = TRUE)

  if (!"name" %in% names(provincies_sf)) {
    log_msg("[geo] Province geo missing 'name' column")
    return(NULL)
  }

  geo <- provincies_sf |>
    dplyr::mutate(name = normalize_provincie_name(.data$name))

  unmatched <- setdiff(prov_data$provincie, geo$name)
  if (length(unmatched) > 0) {
    log_msg(sprintf("[geo] Unmatched province names in data: %s", paste(unmatched, collapse = ", ")))
  }

  dplyr::left_join(geo, prov_data, by = c("name" = "provincie"))
}

it3_map_uses_diverging_scale <- function(metric) {
  metric %in% c("kosten_per_persoon", "kosten_per_gebruiker", "prevalentie_gebruik")
}

it3_map_prgn_colours <- function() {
  # Matches plotly's ColorBrewer PRGn colorscale stops.
  c(
    "#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#F7F7F7",
    "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"
  )
}

it3_map_fill_scale <- function(metric, metric_label, valid_vals) {
  if (it3_map_uses_diverging_scale(metric)) {
    min_val <- min(valid_vals, na.rm = TRUE)
    max_val <- max(valid_vals, na.rm = TRUE)
    if (!is.finite(min_val) || !is.finite(max_val)) {
      min_val <- 0
      max_val <- 1
    } else if (identical(min_val, max_val)) {
      pad <- max(abs(min_val) * 0.001, 1e-6)
      min_val <- min_val - pad
      max_val <- max_val + pad
    }

    return(ggplot2::scale_fill_gradientn(
      colours = it3_map_prgn_colours(),
      limits = c(min_val, max_val),
      na.value = "grey85",
      name = metric_label
    ))
  }

  max_val <- max(valid_vals, na.rm = TRUE)
  if (!is.finite(max_val) || max_val <= 0) {
    max_val <- 1
  }

  ggplot2::scale_fill_gradient(
    low = "#FFFFFF",
    high = "#2C3E7A",
    limits = c(0, max_val),
    na.value = "grey85",
    name = metric_label
  )
}

build_it3_cost_map_plot <- function(map_sf, metric, cohort, died) {
  metric_label <- it3_cost_metric_label(metric)
  cohort_label <- cohort %||% "-"
  died_label <- died %||% "-"

  if (is.null(map_sf) || nrow(map_sf) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Geen data beschikbaar voor deze selectie."
        ) +
        ggplot2::theme_void()
    )
  }

  valid_vals <- map_sf$metric_value[!is.na(map_sf$metric_value)]
  if (length(valid_vals) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Geen data beschikbaar voor deze selectie."
        ) +
        ggplot2::theme_void()
    )
  }

  ggplot2::ggplot(map_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = metric_value),
      color = "grey40",
      linewidth = 0.3
    ) +
    it3_map_fill_scale(metric, metric_label, valid_vals) +
    ggplot2::labs(
      title = sprintf("Totaal 1000 dagen (%s, %s)", metric_label, cohort_label),
      subtitle = sprintf("Populatie: %s | MSZ vergoed bedrag ZVW", died_label)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.key.height = ggplot2::unit(2, "cm"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}

regression_sig_label <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "Niet significant",
    p < 0.01 ~ "p < 0.01",
    p < 0.05 ~ "p < 0.05",
    p < 0.1 ~ "p < 0.10",
    TRUE ~ "Niet significant"
  )
}

regression_sig_palette <- c(
  "p < 0.01" = "#2563EB",
  "p < 0.05" = "#EAB308",
  "p < 0.10" = "#F97316",
  "Niet significant" = "#9CA3AF"
)

read_iteration3_cost_agg <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(tibble::tibble())
  }

  sheets <- readxl::excel_sheets(path)
  purrr::map(
    sheets,
    function(sh) {
      tmp <- readxl::read_excel(path, sheet = sh, .name_repair = "unique")
      tmp <- dplyr::as_tibble(tmp)
      tmp <- normalize_iteration3_cost_agg_sheet(tmp)
      tmp$sheet <- sh
      tmp
    }
  ) |>
    dplyr::bind_rows()
}

format_regression_dependent_var <- function(x) {
  x <- as.character(x)
  needs_suffix <- grepl("1000d$", x) | grepl("30d$", x)
  ifelse(needs_suffix, paste0(x, "_per_persoon"), x)
}

read_iteration3_regression <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(tibble::tibble())

  df <- readxl::read_excel(path, .name_repair = "unique")
  df <- dplyr::as_tibble(df)

  col_variable <- first_existing(names(df), c("variable", "Variable", "term", "coefficient"))
  col_estimate <- first_existing(names(df), c("Estimate", "estimate", "coef", "coefficient_estimate"))
  col_se <- {
    hit <- names(df)[grepl("^std", names(df), ignore.case = TRUE) & grepl("error", names(df), ignore.case = TRUE)]
    if (length(hit) > 0) hit[[1]] else first_existing(names(df), c("Std. Error", "std_error", "Std..Error", "se"))
  }
  col_dependent <- first_existing(names(df), c("dependent_var", "Dependent_var", "dependent_variable"))
  col_p <- {
    hit <- names(df)[grepl("pr", names(df), ignore.case = TRUE)]
    if (length(hit) > 0) hit[[1]] else NA_character_
  }
  col_n <- first_existing(names(df), c("n_obs", "n", "N"))
  col_used_cohorts <- dplyr::case_when(
    "used_cohorts" %in% names(df) ~ "used_cohorts",
    "cohorts_used" %in% names(df) ~ "cohorts_used",
    TRUE ~ NA_character_
  )

  required <- c(col_variable, col_estimate, col_se, col_dependent)
  if (any(is.na(required))) {
    warning(
      "regression_results.xlsx: ontbrekende kolommen. Gevonden: ",
      paste(names(df), collapse = ", ")
    )
    return(tibble::tibble())
  }

  out <- df |>
    dplyr::transmute(
      coefficient = as.character(.data[[col_variable]]),
      estimate = numericize(.data[[col_estimate]]),
      std_error = numericize(.data[[col_se]]),
      dependent_var = format_regression_dependent_var(.data[[col_dependent]]),
      p_value = if (!is.na(col_p)) numericize(.data[[col_p]]) else NA_real_,
      n_obs = if (!is.na(col_n)) numericize(.data[[col_n]]) else NA_real_,
      used_cohorts = if (!is.na(col_used_cohorts)) as.character(.data[[col_used_cohorts]]) else NA_character_
    ) |>
    dplyr::filter(
      !is.na(coefficient), nzchar(coefficient),
      !is.na(dependent_var), nzchar(dependent_var),
      !is.na(estimate), !is.na(std_error)
    ) |>
    dplyr::mutate(
      ci_lower = estimate - 1.96 * std_error,
      ci_upper = estimate + 1.96 * std_error
    )

  out
}

# ===== VARIABLE DECLARATIONS & UTILITIES =====

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cohort", "died", "n_totaal", "value", "name", "type", "t",
    "q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon",
    "q75_per_persoon", "q95_per_persoon", "bin_size", "doodsoorzaak",
    "t_numeric", "value_butterfly", "group", "interventie", "interventie_category",
    "wlz_start_period", "provincie", "used_any_acp_2years", "used_cohorts", "sheet",
    "metric_value", "name"
  ))
}

data_path <- resolve_existing_path(c(
  "data/data_iteration_1/all_output.xlsx",
  "dashboard/data/data_iteration_1/all_output.xlsx"
))
log_file <- "shiny_console.log"
unlink(log_file)

log_msg <- function(msg) {
  cat(paste0("[", Sys.time(), "] ", msg, "\n"), file = log_file, append = TRUE)
  cat(paste0("[", Sys.time(), "] ", msg, "\n"))
}

safe_read_iteration3_xlsx <- function(path, label = "iter3_xlsx") {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    log_msg(sprintf("[iter3] %s: bestand niet gevonden (%s)", label, path %||% "geen pad"))
    return(tibble::tibble())
  }
  tryCatch({
    df <- read_iteration3_xlsx(path)
    log_msg(sprintf("[iter3] %s: %d rijen geladen uit %s", label, nrow(df), path))
    df
  }, error = function(e) {
    log_msg(sprintf("[iter3] %s: laden mislukt (%s): %s", label, path, e$message))
    tibble::tibble()
  })
}

safe_read_iteration3_cost_agg <- function(path, label) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    log_msg(sprintf("[iter3] %s: bestand niet gevonden (%s)", label, path %||% "geen pad"))
    return(tibble::tibble())
  }
  tryCatch({
    df <- read_iteration3_cost_agg(path)
    log_msg(sprintf("[iter3] %s: %d rijen geladen uit %s", label, nrow(df), path))
    df
  }, error = function(e) {
    log_msg(sprintf("[iter3] %s: laden mislukt (%s): %s", label, path, e$message))
    tibble::tibble()
  })
}

safe_read_iteration3_regression <- function(path, label) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    log_msg(sprintf("[iter3] %s: bestand niet gevonden (%s)", label, path %||% "geen pad"))
    return(tibble::tibble())
  }
  tryCatch({
    df <- read_iteration3_regression(path)
    log_msg(sprintf("[iter3] %s: %d rijen geladen uit %s", label, nrow(df), path))
    df
  }, error = function(e) {
    log_msg(sprintf("[iter3] %s: laden mislukt (%s): %s", label, path, e$message))
    tibble::tibble()
  })
}

data_path_iteration3_zpk <- resolve_iteration3_subfile("iteration_3c", "zpk_categorieen_tellingen.xlsx")
data_path_iteration3_top50 <- resolve_iteration3_subfile("iteration_3c", "top50_activiteiten.xlsx")

it3_cost_agg_by_iter <- list(
  "3c" = safe_read_iteration3_cost_agg(
    resolve_iteration3_subfile("iteration_3c", "cost_aggregations.xlsx"),
    "cost_agg_3c"
  ),
  "3d" = safe_read_iteration3_cost_agg(
    resolve_iteration3_subfile("iteration_3d", "cost_aggregations.xlsx"),
    "cost_agg_3d"
  ),
  "3e" = safe_read_iteration3_cost_agg(
    resolve_iteration3_subfile("iteration_3e", "cost_aggregations.xlsx"),
    "cost_agg_3e"
  )
)
it3_regression_by_iter <- list(
  "3c" = safe_read_iteration3_regression(
    resolve_iteration3_subfile("iteration_3c", "regression_results.xlsx"),
    "regression_3c"
  ),
  "3d" = safe_read_iteration3_regression(
    resolve_iteration3_subfile("iteration_3d", "regression_results.xlsx"),
    "regression_3d"
  )
)
it3_cost_agg_raw_3c <- it3_cost_agg_by_iter[["3c"]]
it3_zpk_raw <- safe_read_iteration3_xlsx(data_path_iteration3_zpk, "zpk")
it3_top50_raw <- safe_read_iteration3_xlsx(data_path_iteration3_top50, "top50_activiteiten")
it3_top50_prest_raw <- safe_read_iteration3_xlsx(
  resolve_iteration3_subfile("iteration_3d", "top50_prestaties.xlsx"),
  "top50_prestaties"
)
it3_acp_ages_raw <- safe_read_iteration3_xlsx(
  resolve_iteration3_subfile("iteration_3d", "acp_average_ages.xlsx"),
  "acp_average_ages"
)
it3_acp_pop_raw <- safe_read_iteration3_xlsx(
  resolve_iteration3_subfile("iteration_3d", "ACP_population_descriptives.xlsx"),
  "acp_population_descriptives"
)
it3_zvwk_raw <- safe_read_iteration3_xlsx(
  resolve_iteration3_subfile("iteration_3d", "zvwk_distributions.xlsx"),
  "zvwk_distributions"
)

log_msg(sprintf("[startup] APP_ROOT=%s, getwd()=%s", APP_ROOT, normalizePath(getwd(), winslash = "/", mustWork = FALSE)))

it3_provinces_sf <- load_it3_provinces_sf()
if (!is.null(it3_provinces_sf)) {
  log_msg(sprintf("[geo] Loaded %d province polygons", nrow(it3_provinces_sf)))
}

read_all_data <- function(path = data_path) {
  if (!file.exists(path)) {
    msg <- sprintf("[read_all_data] File not found: %s", path)
    log_msg(msg)
    return(tibble::tibble())
  }
  
  sheets <- readxl::excel_sheets(path)
  df <- purrr::map_dfr(sheets, ~ readxl::read_excel(path, sheet = .x, col_types = "text") %>%
                         dplyr::mutate(across(c("cohort", "t", "died", "name", "type", "doodsoorzaak", "bin_size"), as.character),
                                       value = as.numeric(value),
                                       n_totaal = as.numeric(n_totaal)))
  log_msg(sprintf("[read_all_data] Loaded %d rows from %d sheets", nrow(df), length(sheets)))
  df
}

# Initialize data
log_msg("[startup] Initializing data...")
all_data_initial <- tryCatch(
  read_all_data(data_path),
  error = function(e) {
    msg <- sprintf("[startup] read_all_data failed: %s", e$message)
    log_msg(msg)
    tibble::tibble()
  }
)

base_names <- if (nrow(all_data_initial) > 0) {
  sort(unique(all_data_initial$name[!startsWith(all_data_initial$name, "gebruik") & !startsWith(all_data_initial$name, "heeft")]))
} else {
  character(0)
}

doodsoorzaken <- if (nrow(all_data_initial) > 0) {
  c("all", sort(unique(all_data_initial$doodsoorzaak[all_data_initial$doodsoorzaak != "all"])))
} else {
  "all"
}

log_msg(sprintf("[startup] Initialization complete: %d base names, %d doodsoorzaken", 
                length(base_names), length(doodsoorzaken)))

if (!is.na(data_path_iteration3_zpk) && nzchar(data_path_iteration3_zpk) && file.exists(data_path_iteration3_zpk)) {
  prest_col <- it3_zpk_prestatie_colname(it3_zpk_raw)
  log_msg(sprintf(
    "[startup] Iteratie 3 ZPK: %s (%d rows, prestatie_col=%s)",
    data_path_iteration3_zpk,
    if (is.null(it3_zpk_raw)) 0L else nrow(it3_zpk_raw),
    if (is.na(prest_col)) "MISSING" else prest_col
  ))
} else {
  log_msg("[startup] Iteratie 3 ZPK: bestand niet gevonden")
}

for (iter in names(it3_cost_agg_by_iter)) {
  df_iter <- it3_cost_agg_by_iter[[iter]]
  sheet_vals <- if (nrow(df_iter) > 0 && "sheet" %in% names(df_iter)) {
    sort(unique(as.character(df_iter$sheet)))
  } else {
    character(0)
  }
  log_msg(sprintf(
    "[startup] Iteratie 3 cost_agg_%s: %d rows, %d sheets (%s)",
    iter,
    nrow(df_iter),
    length(sheet_vals),
    if (length(sheet_vals) > 0) paste(sheet_vals, collapse = ", ") else "none"
  ))
}

# ===== HELPER FUNCTIONS =====
find_gebruikt_name <- function(name_choice, data) {
  if (is.null(data) || nrow(data) == 0) return(NA_character_)
  print(unique(data$name))

  # Generate all possible naming conventions
  candidates <- c(
    paste0("gebruik", name_choice),         # gebruikbedragwlzzin
    paste0("gebruikt_", name_choice),       # gebruik_bedragwlzzin
    paste0(name_choice, "_gebruik"),        # bedragwlzzin_gebruik
    paste0("heeft", name_choice),           # heeftbedragwlzzin
    paste0("heeft_", name_choice),          # heeft_bedragwlzzin
    paste0(name_choice, "_heeft")           # bedragwlzzin_heeft
  )

  valid <- intersect(candidates, unique(data$name))
  if (length(valid) > 0) valid[[1]] else NA_character_
}

# Helper function: Map interventions to their constituent names
get_interventie_categories <- function() {
  list(
    "AAA" = c("kosten_aaa_0303_0406_operatie", "kosten_aaa_0303_0406_totaal"),
    "Heup" = c("kosten_heup_0303_0218_operatie", "kosten_heup_0303_0218_prothese", "kosten_heup_0303_0218_totaal",
               "kosten_heup_0303_0219_operatie", "kosten_heup_0303_0219_totaal",
               "kosten_heup_0305_3019_operatie", "kosten_heup_0305_3019_prothese", "kosten_heup_0305_3019_totaal",
               "kosten_heup_0305_3020_operatie", "kosten_heup_0305_3020_totaal", "kosten_heupprothese"),
    "IC" = c("kosten_add_on_ic"),
    "Diagnostiek" = c("kosten_eerstelijn_zpk_4", "kosten_eerstelijn_zpk_7", "kosten_eerstelijn_zpk_8",
                      "kosten_eerstelijn_zpk_9", "kosten_eerstelijn_zpk_10", "kosten_eerstelijn_zpk_11",
                      "kosten_eerstelijn_zpk_4_7_11", "kosten_overig_tweedelijn_zpk_4", "kosten_overig_tweedelijn_zpk_7",
                      "kosten_overig_tweedelijn_zpk_10", "kosten_overig_tweedelijn_zpk_11", "kosten_overig_tweedelijn_zpk_4_7_11"),
    "Oncologie" = c("kosten_oncolgie_chemo", "kosten_oncolgie_immuno"),
    "Polyfarmacie" = c("gebruikt_minstens5_atc4")
  )
}

# Helper function: Get maatstaf options
get_maatstaf_options <- function() {
  c(
    "Totale kosten" = "sum_totaal_groep",
    "Kosten per persoon" = "gemiddelde_per_persoon",
    "Aantal gebruikers" = "n_totaal_gebruikers",
    "Kosten per gebruiker" = "gemiddelde_per_gebruiker",
    "Prevalentie per 100" = "prevalentie_per_100"
  )
}

get_maatstaf_label <- function(value) {
  labels <- get_maatstaf_options()
  match_idx <- which(labels == value)
  if (length(match_idx) > 0) names(labels)[match_idx[[1]]] else value
}

get_bin_size_label <- function(value) {
  if (identical(value, "monthly")) return("Maandelijks")
  if (identical(value, "1000days")) return("1000 dagen")
  value
}

get_time_axis_label <- function(value) {
  if (identical(value, "monthly")) return("Maanden voor overlijden (t)")
  "Tijdsbin voor overlijden (t)"
}

# Helper function: Process measurements for standardized filtering
process_measurements <- function(data, maatstaf, handle_prevalentie = TRUE) {
  if (is.null(data) || nrow(data) == 0) return(tibble::tibble())

  # If maatstaf is prevalentie_per_100, handle specially
  if (handle_prevalentie && maatstaf == "prevalentie_per_100") {
    # Prevalentie uses gebruik_ or heeft_ prefixed names with gemiddelde_per_persoon type
    result <- data %>%
      filter(type == "gemiddelde_per_persoon",
             (startsWith(name, "gebruik") | startsWith(name, "heeft"))) %>%
      mutate(type = "prevalentie_per_100")
  } else {
    # Standard filtering
    result <- data %>% filter(type == maatstaf)
  }

  return(result)
}

# Helper function: Get all intervention variable names
get_all_interventie_names <- function() {
  cats <- get_interventie_categories()
  unique(unlist(cats))
}


# ===== UI DEFINITION =====
ui <- navbarPage(
  title = "Laatste 1000 dagen",
  id = "main_nav",

  tabPanel(
    "Iteratie 1",
    do.call(
      tabsetPanel,
      c(
        id = "iter1_tabs",
        list(
          tabPanel("Basispopulatie",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectInput("pop_jaar", "Jarenselectie:", choices = c("2019", "2023", "2019 + 2023"), selected = "2019 + 2023"),
                 selectInput("pop_split", "Kies populaties:", choices = c("Enkel totale populatie", "Totaal + subgroepen doodsoorzaak"), selected = "Totaal + subgroepen doodsoorzaak"),
                 hr(),
                 chart_data_downloads_ui(
                   "iter1_basis_dl",
                   chart_type = "grouped_bar",
                   raw_label = "Gegevens downloaden",
                   thinkcell_label = "Download voor Think-cell"
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_basispopulatie", height = "600px")
               )
             )
          ),

          tabPanel("Zorg Totaal",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectInput("tot_pop", "Populatie:", choices = doodsoorzaken, selected = "all"),
                 selectInput("tot_bin_size", "Bin size:",
                             choices = c("monthly", "1000days"),
                             selected = "1000days"),
                 selectInput("tot_jaar", "Jaar:", choices = c("2019", "2023", "Beide"), selected = "2023"),
                 selectInput("tot_maatstaf", "Maatstaf:",
                             choices = c("Totale kosten" = "sum_totaal_groep",
                                         "Kosten per persoon" = "gemiddelde_per_persoon",
                                         "Aantal gebruikers" = "n_totaal_gebruikers",
                                         "Kosten per gebruiker" = "gemiddelde_per_gebruiker",
                                         "Prevalentie per 100" = "prevalentie_per_100"),
                             selected = "gemiddelde_per_persoon"),
                 selectizeInput("tot_variables", "Zorgvariabelen:",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "Alle (behalve interventies)")),
                 selectInput("tot_vgl", "Kies vergelijking:",
                             choices = c("Geen vergelijking", "Overleden vs. In leven (Controle)"),
                             selected = "Overleden vs. In leven (Controle)"),
                 hr(),
                 chart_data_downloads_ui(
                   "iter1_totaal_dl",
                   chart_type = "grouped_bar",
                   raw_label = "Gegevens downloaden",
                   thinkcell_label = "Download voor Think-cell"
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_zorg_totaal", height = "600px")
               )
             )
          ),

          tabPanel("Zorg over Tijd",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectizeInput("mnd_domein", "Zorgdomein (Variabele):", choices = base_names, selected = base_names[1], multiple = TRUE),
                 helpText("Meerdere selectie toont een stacked barchart in staafgrafiekmodus."),
                 selectInput("mnd_bin_size", "Bin size:",
                             choices = c("monthly", "1000days"),
                             selected = "monthly"),
                 selectInput("mnd_maatstaf", "Maatstaf:",
                             choices = c("Totale kosten" = "sum_totaal_groep",
                                         "Kosten per persoon" = "gemiddelde_per_persoon",
                                         "Aantal gebruikers" = "n_totaal_gebruikers",
                                         "Kosten per gebruiker" = "gemiddelde_per_gebruiker",
                                         "Prevalentie per 100" = "prevalentie_per_100"),
                             selected = "gemiddelde_per_persoon"),
                 selectInput("mnd_jaar", "Jaar:", choices = c("2019", "2023", "Beide"), selected = "2023"),
                 selectInput("mnd_pop", "Populatie (Doodsoorzaak):", choices = doodsoorzaken, selected = "all"),
                 selectInput("mnd_vgl", "Kies status:",
                             choices = c("Alleen overleden" = "Overleden",
                                         "Alleen in leven" = "In leven"),
                             selected = "Overleden"),
                 selectInput("mnd_grafiek", "Grafiektype:",
                             choices = c("Staafgrafiek", "Lijngrafiek"),
                             selected = "Staafgrafiek"),
                 selectInput("mnd_lijnmodus", "Lijngrafiek modus:",
                             choices = c("Status (met/zonder controle)" = "status",
                                         "Alle doodsoorzaken in 1 grafiek" = "doodsoorzaak",
                                         "Totale populatie 2019 vs 2023" = "cohort"),
                             selected = "status"),
                 selectizeInput("mnd_zichtbare_lijnen", "Zichtbare lijnen:",
                                choices = NULL, selected = NULL, multiple = TRUE),
                 helpText("Tip: in lijngrafiekmodus kun je lijnen aan/uit zetten via 'Zichtbare lijnen'"),
                 hr(),
                 chart_data_downloads_ui(
                   "iter1_tijd_dl",
                   chart_type = "line",
                   raw_label = "Gegevens downloaden",
                   thinkcell_label = "Download voor Think-cell"
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_zorg_maandelijks", height = "600px")
               )
             )
          ),

          tabPanel("Kosten Boxplot",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectInput("cost_var", "Kies variabele (name):", choices = base_names, selected = base_names[1]),
                 selectInput("cost_bin_size", "Bin size:",
                             choices = c("monthly", "1000days"),
                             selected = "monthly"),
                 selectInput("cost_pop", "Populatie (Doodsoorzaak):",
                             choices = doodsoorzaken,
                             selected = "all"),
                 helpText("Boxplot-achtig overzicht op basis van quantielen per cohort en status."),
                 hr(),
                 chart_data_downloads_ui(
                   "iter1_cost_dl",
                   chart_type = "scatter",
                   raw_label = "Gegevens downloaden",
                   thinkcell_label = "Download voor Think-cell"
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_cost", height = "600px")
               )
             )
          ),

          tabPanel("Zorg per Domein (Butterfly)",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectInput("butterfly_domein", "Zorgdomein:", choices = base_names, selected = base_names[1]),
                 selectInput("butterfly_maatstaf", "Maatstaf:",
                             choices = c("Aantal gebruikers" = "n_totaal_gebruikers",
                                         "Kosten per gebruiker" = "gemiddelde_per_persoon",
                                         "Totale kosten" = "sum_totaal_groep",
                                         "Kosten per gebruiker (alt)" = "gemiddelde_per_gebruiker",
                                         "Prevalentie per 100" = "prevalentie_per_100"),
                             selected = "gemiddelde_per_persoon"),
                 selectInput("butterfly_vgl", "Vergelijking (Links vs Rechts):",
                             choices = c("Geobserveerd 2023 vs. Controle 2023" = "obs_2023_vs_ctrl_2023",
                                         "Geobserveerd 2019 vs. Geobserveerd 2023" = "obs_2019_vs_obs_2023",
                                         "Geobserveerd 2019 vs. Controle 2019" = "obs_2019_vs_ctrl_2019"),
                             selected = "obs_2023_vs_ctrl_2023"),
                 hr(),
                 chart_data_downloads_ui(
                   "iter1_bfly_dl",
                   chart_type = "bar",
                   raw_label = "Gegevens downloaden",
                   thinkcell_label = "Download voor Think-cell"
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_butterfly", height = "700px")
               )
             )
          ),

          tabPanel("Interventies",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectizeInput("int_interventie", "Selecteer interventie variabele(s):",
                                choices = get_all_interventie_names(),
                                selected = get_all_interventie_names()[1],
                                multiple = TRUE),
                 selectInput("int_maatstaf", "Maatstaf:",
                             choices = get_maatstaf_options(),
                             selected = "gemiddelde_per_persoon"),
                 selectInput("int_bin_size", "Bin size:",
                             choices = c("monthly", "1000days"),
                             selected = "1000days"),
                 selectInput("int_jaar", "Jaar:",
                             choices = c("2019", "2023", "Beide"),
                             selected = "2023"),
                 selectInput("int_vgl", "Vergelijking:",
                             choices = c("Geen vergelijking", "Overleden vs. In leven (Controle)"),
                             selected = "Overleden vs. In leven (Controle)"),
                 hr(),
                 chart_data_downloads_ui(
                   "iter1_int_dl",
                   chart_type = "grouped_bar",
                   raw_label = "Gegevens downloaden",
                   thinkcell_label = "Download voor Think-cell"
                 )
               ),
               mainPanel(
                 plotlyOutput("plot_interventies", height = "600px")
               )
             )
          ),

          tabPanel("Systeem Logs",
             verbatimTextOutput("app_log")
          )
        )
      )
    )
  ),

  tabPanel(
    "Iteratie 2",
    iteration2_header(),
    do.call(tabsetPanel, c(id = "iter2_tabs", iteration2_panels()))
  ),

  tabPanel(
    "Iteratie 3",
    tabsetPanel(
      id = "iter3_tabs",
      tabPanel(
        "ZPK categorieën tellingen",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("it3_zpk_died", "Populatie (died)", choices = NULL),
            selectInput("it3_zpk_cohort", "Cohort", choices = NULL),
            selectInput("it3_zpk_setting", "Setting (vektmszsettingzpk)", choices = NULL),
            selectInput("it3_zpk_bin", "Bin size", choices = NULL),
            radioButtons(
              "it3_zpk_prestatie_type",
              "Prestatietype",
              choices = c("All" = "All", "DBC" = "DBC", "OZP" = "OZP"),
              selected = "DBC"
            ),
            uiOutput("it3_zpk_prestatie_notice"),
            radioButtons(
              "it3_zpk_metric",
              "Kies variabele",
              choices = it3_zpk_metric_choices,
              selected = "n_totaal_gebruikers"
            )
          ),
          mainPanel(
            width = 9,
            plotlyOutput("it3_plot_zpk", height = "720px"),
            br(),
            chart_data_downloads_ui(
              "iter3_zpk_dl",
              chart_type = "stacked_bar",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            )
          )
        )
      ),
      tabPanel(
        "Top 50 activiteiten",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("it3_top50_ranked_by", "Ranked by", choices = NULL),
            radioButtons(
              "it3_top50_metric",
              "Kies variabele",
              choices = c(
                "Aantal gebruikers" = "n_totaal_gebruikers",
                "Aantal activiteiten" = "n_totaal_activiteiten"
              ),
              selected = "n_totaal_gebruikers"
            ),
            selectInput(
              "it3_top50_compare_group",
              "Comparison group",
              choices = c("bin size", "died"),
              selected = "died"
            )
          ),
          mainPanel(
            width = 9,
            h4(textOutput("it3_top50_title")),
            fluidRow(
              column(9, plotlyOutput("it3_plot_top50_main", height = "820px")),
              column(3, plotlyOutput("it3_plot_top50_compare", height = "820px"))
            ),
            br(),
            chart_data_downloads_ui(
              "iter3_top50_main_dl",
              chart_type = "bar",
              raw_label = "Gegevens downloaden (hoofd)",
              thinkcell_label = "Download voor Think-cell (hoofd)"
            ),
            chart_data_downloads_ui(
              "iter3_top50_cmp_dl",
              chart_type = "bar",
              raw_label = "Gegevens downloaden (vergelijking)",
              thinkcell_label = "Download voor Think-cell (vergelijking)"
            )
          )
        )
      ),
      tabPanel(
        "Top 50 prestaties",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("it3_top50_prest_ranked_by", "Ranked by", choices = NULL),
            radioButtons(
              "it3_top50_prest_metric",
              "Kies variabele",
              choices = c(
                "Aantal gebruikers" = "n_totaal_gebruikers",
                "Aantal declaraties" = "n_totaal_declaraties",
                "Som totaal groep" = "sum_totaal_groep"
              ),
              selected = "n_totaal_gebruikers"
            ),
            selectInput(
              "it3_top50_prest_compare_group",
              "Comparison group",
              choices = c("bin size", "died"),
              selected = "died"
            )
          ),
          mainPanel(
            width = 9,
            h4(textOutput("it3_top50_prest_title")),
            fluidRow(
              column(9, plotlyOutput("it3_plot_top50_prest_main", height = "820px")),
              column(3, plotlyOutput("it3_plot_top50_prest_compare", height = "820px"))
            ),
            br(),
            chart_data_downloads_ui(
              "iter3_prest_main_dl",
              chart_type = "bar",
              raw_label = "Gegevens downloaden (hoofd)",
              thinkcell_label = "Download voor Think-cell (hoofd)"
            ),
            chart_data_downloads_ui(
              "iter3_prest_cmp_dl",
              chart_type = "bar",
              raw_label = "Gegevens downloaden (vergelijking)",
              thinkcell_label = "Download voor Think-cell (vergelijking)"
            )
          )
        )
      ),
      tabPanel(
        "Kosten over Tijd",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              "it3_cost_iter",
              "Select iteration data",
              choices = c("3c" = "3c", "3d" = "3d", "3e" = "3e"),
              selected = "3c"
            ),
            selectInput("it3_cost_dataset", "Dataset", choices = NULL),
            uiOutput("it3_cost_name_ui"),
            hr(),
            uiOutput("it3_cost_split_var_ui"),
            hr(),
            selectInput("it3_cost_cohort", "Cohort", choices = NULL),
            uiOutput("it3_cost_died_ui"),
            selectInput("it3_cost_bin", "Bin size", choices = NULL),
            uiOutput("it3_cost_dim_filters_ui"),
            radioButtons(
              "it3_cost_metric",
              "Kies variabele",
              choices = c("Laden..." = "__loading__"),
              selected = "__loading__"
            )
          ),
          mainPanel(
            width = 9,
            plotlyOutput("it3_plot_cost_agg", height = "720px"),
            br(),
            chart_data_downloads_ui(
              "iter3_cost_dl",
              chart_type = "line",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            )
          )
        )
      ),
      tabPanel(
        "Kostenkaart",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("it3_map_cohort", "Cohort", choices = NULL),
            selectInput("it3_map_died", "Populatie (died)", choices = NULL),
            radioButtons(
              "it3_map_metric",
              "Kies variabele (type)",
              choices = c("Laden..." = "__loading__"),
              selected = "__loading__"
            ),
            helpText("MSZ vergoed bedrag ZVW per provincie, totaal 1000 dagen.")
          ),
          mainPanel(
            width = 9,
            plotOutput("it3_plot_cost_map", height = "720px"),
            br(),
            downloadButton("it3_dl_cost_map", "Kaart downloaden (PNG)")
          )
        )
      ),
      tabPanel(
        "Regressie resultaten",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              "it3_reg_iter",
              "Select iteration data",
              choices = c("3c" = "3c", "3d" = "3d"),
              selected = "3c"
            ),
            selectInput("it3_reg_used_cohorts", "Gebruikte cohorten (used_cohorts)", choices = NULL),
            selectInput("it3_reg_dependent_var", "Afhankelijke variabele (dependent_var)", choices = NULL),
            helpText("Coëfficiënten met 95%-betrouwbaarheidsinterval (schatting ± 1,96 × standaardfout).")
          ),
          mainPanel(
            width = 9,
            h4(textOutput("it3_reg_title")),
            uiOutput("it3_reg_plot_ui"),
            br(),
            chart_data_downloads_ui(
              "iter3_reg_dl",
              chart_type = "bar",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            )
          )
        )
      ),
      tabPanel(
        "ACP descriptives",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("it3_acp_split_by", "Split variable", choices = NULL)
          ),
          mainPanel(
            width = 9,
            h4("Gemiddelde leeftijd ACP-gebruikers"),
            tableOutput("it3_table_acp_ages"),
            br(),
            h4("ACP populatie descriptives"),
            plotlyOutput("it3_plot_acp_pop", height = "720px"),
            br(),
            chart_data_downloads_ui(
              "iter3_acp_dl",
              chart_type = "grouped_bar",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            )
          )
        )
      ),
      tabPanel(
        "ZVW-kosten verdeling",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("it3_zvwk_cost_type", "Cost type", choices = NULL),
            selectInput("it3_zvwk_cohort", "Cohort", choices = NULL),
            radioButtons(
              "it3_zvwk_ref_line",
              "Show average line",
              choices = it3_zvwk_ref_line_choices,
              selected = "mean_costs_all"
            )
          ),
          mainPanel(
            width = 9,
            plotlyOutput("it3_plot_zvwk", height = "720px"),
            br(),
            chart_data_downloads_ui(
              "iter3_zvwk_dl",
              chart_type = "scatter",
              raw_label = "Gegevens downloaden",
              thinkcell_label = "Download voor Think-cell"
            )
          )
        )
      )
    )
  )
)

# ===== SERVER DEFINITION =====
server <- function(input, output, session) {
  
  error_log <- reactiveVal(character())
  add_error <- function(msg) {
    log_msg(msg)
    error_log(c(error_log(), msg))
  }
  
  # 1. Load Core Data Reactively
  all_data <- reactive({
    tryCatch({
      df <- read_all_data(data_path)
      df
    }, error = function(e) {
      add_error(sprintf("[reactive] all_data load failed: %s", e$message))
      tibble::tibble()
    })
  })
  
  # ==========================================
  # SERVER LOGIC: ITERATIE 3
  # ==========================================

  it3_cost_agg_raw <- reactive({
    iter <- input$it3_cost_iter %||% "3c"
    it3_cost_agg_by_iter[[iter]] %||% tibble::tibble()
  })

  it3_cost_dataset_df <- reactive({
    df <- it3_cost_agg_raw()
    req(!is.null(df), nrow(df) > 0)
    req(!is.null(input$it3_cost_dataset), nzchar(input$it3_cost_dataset))
    req("sheet" %in% names(df))
    df |> dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset))
  })

  it3_regression_raw <- reactive({
    iter <- input$it3_reg_iter %||% "3c"
    it3_regression_by_iter[[iter]] %||% tibble::tibble()
  })

  output$it3_zpk_prestatie_notice <- renderUI({
    if (is.na(data_path_iteration3_zpk) || !nzchar(data_path_iteration3_zpk) || !file.exists(data_path_iteration3_zpk)) {
      return(helpText(
        "Waarschuwing: ZPK-bestand niet gevonden op de server (verwacht in data/data_iteration_3/iteration_3c/zpk_categorieen_tellingen.xlsx). Controleer of de data is gedeployed."
      ))
    }
    if (is.null(it3_zpk_raw) || nrow(it3_zpk_raw) == 0) {
      return(helpText(
        "Waarschuwing: ZPK-bestand kon niet worden geladen (",
        basename(data_path_iteration3_zpk),
        ")."
      ))
    }
    prest_col <- it3_zpk_prestatie_colname(it3_zpk_raw)
    if (!is.na(prest_col)) return(NULL)
    helpText(
      "Waarschuwing: geladen ZPK-bestand mist kolom prestatie_type (",
      basename(data_path_iteration3_zpk),
      "). DBC/OZP-filter werkt dan niet."
    )
  })

  observe({
    # ZPK categorieën tellingen
    if (!is.null(it3_zpk_raw) && nrow(it3_zpk_raw) > 0) {
      if ("died" %in% names(it3_zpk_raw)) {
        died_vals <- ordered_values(it3_zpk_raw$died)
        updateSelectInput(session, "it3_zpk_died", choices = died_vals, selected = died_vals[[1]])
      }
      if ("cohort" %in% names(it3_zpk_raw)) {
        cohort_vals <- ordered_values(it3_zpk_raw$cohort)
        updateSelectInput(session, "it3_zpk_cohort", choices = cohort_vals, selected = if ("2023" %in% cohort_vals) "2023" else cohort_vals[[1]])
      }
      if ("vektmszsettingzpk" %in% names(it3_zpk_raw)) {
        setting_vals <- ordered_values(it3_zpk_raw$vektmszsettingzpk)
        updateSelectInput(
          session,
          "it3_zpk_setting",
          choices = choice_names(setting_vals, pretty_vektmszsettingzpk),
          selected = setting_vals[[1]]
        )
      }
      if ("bin_size" %in% names(it3_zpk_raw)) {
        bin_vals <- ordered_values(it3_zpk_raw$bin_size)
        updateSelectInput(
          session,
          "it3_zpk_bin",
          choices = bin_vals,
          selected = if ("monthly" %in% bin_vals) "monthly" else bin_vals[[1]]
        )
      }
      prest_col <- it3_zpk_prestatie_colname(it3_zpk_raw)
      if (!is.na(prest_col)) {
        prest_vals <- sort(unique(normalize_it3_filter_value(it3_zpk_raw[[prest_col]])))
        prest_vals <- prest_vals[!is.na(prest_vals) & prest_vals %in% c("DBC", "OZP")]
        if (length(prest_vals) > 0) {
          prest_choices <- c("All" = "All")
          if ("DBC" %in% prest_vals) prest_choices <- c(prest_choices, "DBC" = "DBC")
          if ("OZP" %in% prest_vals) prest_choices <- c(prest_choices, "OZP" = "OZP")
          updateRadioButtons(
            session,
            "it3_zpk_prestatie_type",
            choices = prest_choices,
            selected = if ("DBC" %in% names(prest_choices)) "DBC" else prest_choices[[1]]
          )
        }
      }
    }

    # Top 50 activiteiten
    if (!is.null(it3_top50_raw) && nrow(it3_top50_raw) > 0 && "ranked_by" %in% names(it3_top50_raw)) {
      ranked_vals <- ordered_values(it3_top50_raw$ranked_by)
      updateSelectInput(session, "it3_top50_ranked_by", choices = ranked_vals, selected = ranked_vals[[1]])
    }

    if (!is.null(it3_top50_prest_raw) && nrow(it3_top50_prest_raw) > 0 && "ranked_by" %in% names(it3_top50_prest_raw)) {
      ranked_vals <- ordered_values(it3_top50_prest_raw$ranked_by)
      updateSelectInput(session, "it3_top50_prest_ranked_by", choices = ranked_vals, selected = ranked_vals[[1]])
    }

    if (!is.null(it3_acp_pop_raw) && nrow(it3_acp_pop_raw) > 0 && "split_by" %in% names(it3_acp_pop_raw)) {
      split_vals <- sort(unique(as.character(it3_acp_pop_raw$split_by)))
      split_vals <- split_vals[!is.na(split_vals) & nzchar(split_vals)]
      if (length(split_vals) > 0) {
        updateSelectInput(
          session,
          "it3_acp_split_by",
          choices = choice_names(split_vals, pretty_split_name),
          selected = if ("cohort" %in% split_vals) "cohort" else split_vals[[1]]
        )
      }
    }

    if (!is.null(it3_zvwk_raw) && nrow(it3_zvwk_raw) > 0) {
      if ("cost_type" %in% names(it3_zvwk_raw)) {
        cost_vals <- ordered_values(it3_zvwk_raw$cost_type)
        updateSelectInput(
          session,
          "it3_zvwk_cost_type",
          choices = choice_names(cost_vals, pretty_metric_name),
          selected = if ("zvwktotaal" %in% cost_vals) "zvwktotaal" else cost_vals[[1]]
        )
      }
      if ("cohort" %in% names(it3_zvwk_raw)) {
        cohort_vals <- ordered_values(it3_zvwk_raw$cohort)
        updateSelectInput(
          session,
          "it3_zvwk_cohort",
          choices = cohort_vals,
          selected = if ("2023" %in% cohort_vals) "2023" else cohort_vals[[1]]
        )
      }
    }
  })

  observeEvent(input$it3_reg_iter, {
    df <- it3_regression_raw()
    if (is.null(df) || nrow(df) == 0) return()

    cohort_vals <- sort(unique(as.character(df$used_cohorts)))
    cohort_vals <- cohort_vals[!is.na(cohort_vals) & nzchar(cohort_vals)]
    if (length(cohort_vals) > 0) {
      updateSelectInput(session, "it3_reg_used_cohorts", choices = cohort_vals, selected = cohort_vals[[1]])
    }
    dep_vals <- sort(unique(as.character(df$dependent_var)))
    if (length(dep_vals) > 0) {
      updateSelectInput(session, "it3_reg_dependent_var", choices = dep_vals, selected = dep_vals[[1]])
    }
  }, ignoreInit = FALSE)

  observeEvent(input$it3_cost_iter, {
    df <- it3_cost_agg_raw()
    if (is.null(df) || nrow(df) == 0 || !"sheet" %in% names(df)) return()

    sheet_vals <- sort(unique(as.character(df$sheet)))
    updateSelectInput(
      session,
      "it3_cost_dataset",
      choices = choice_names(sheet_vals, pretty_sheet),
      selected = if (length(sheet_vals) > 0) sheet_vals[[1]] else NULL
    )
  }, ignoreInit = FALSE)

  observeEvent(
    list(input$it3_cost_iter, input$it3_cost_dataset),
    {
      df <- it3_cost_agg_raw()
      req(!is.null(df), nrow(df) > 0)
      req(!is.null(input$it3_cost_dataset), nzchar(input$it3_cost_dataset))
      req("sheet" %in% names(df))

      df <- df |> dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset))
      req(nrow(df) > 0)

      pick_choice <- function(choices, current = NULL, preferred = NULL) {
        choices <- choices[!is.na(choices) & nzchar(as.character(choices))]
        if (length(choices) == 0) return(NULL)
        if (!is.null(current) && current %in% choices) return(current)
        if (!is.null(preferred) && preferred %in% choices) return(preferred)
        choices[[1]]
      }

      if ("cohort" %in% names(df)) {
        cohort_vals <- ordered_values(df$cohort)
        selected_cohort <- pick_choice(cohort_vals, input$it3_cost_cohort, preferred = "2023")
        if (!is.null(selected_cohort)) {
          updateSelectInput(session, "it3_cost_cohort", choices = cohort_vals, selected = selected_cohort)
        }
      }
      if ("bin_size" %in% names(df)) {
        bin_vals <- ordered_values(df$bin_size)
        selected_bin <- pick_choice(bin_vals, input$it3_cost_bin)
        if (!is.null(selected_bin)) {
          updateSelectInput(session, "it3_cost_bin", choices = bin_vals, selected = selected_bin)
        }
      }
    },
    ignoreInit = TRUE
  )

  output$it3_cost_split_var_ui <- renderUI({
    df <- it3_cost_dataset_df()
    req(nrow(df) > 0)

    dim_cols <- it3_cost_agg_varied_dim_cols(df)
    choices <- c(
      "Geen verdeling (all)" = "none",
      stats::setNames(dim_cols, vapply(dim_cols, pretty_split_name, character(1)))
    )
    selected <- isolate(input$it3_cost_split_var) %||% "none"
    if (!selected %in% unname(choices)) selected <- "none"

    radioButtons(
      "it3_cost_split_var",
      "Verdeel naar (split variable)",
      choices = choices,
      selected = selected
    )
  })

  output$it3_cost_dim_filters_ui <- renderUI({
    df <- it3_cost_dataset_df()
    req(nrow(df) > 0)

    split_var <- input$it3_cost_split_var %||% "none"
    filter_cols <- setdiff(it3_cost_agg_varied_dim_cols(df), split_var)
    if (length(filter_cols) == 0) return(NULL)

    tagList(lapply(filter_cols, function(col) {
      vals <- it3_cost_dim_choice_values(col, df[[col]])
      input_id <- it3_cost_dim_input_id(col)
      selected <- isolate(input[[input_id]])
      if (is.null(selected) || !selected %in% vals) {
        selected <- if ("all" %in% vals) "all" else vals[[1]]
      }
      selectInput(
        input_id,
        paste0(pretty_split_name(col), " (filter)"),
        choices = choice_names(vals, function(x) pretty_value(col, x)),
        selected = selected
      )
    }))
  })

  selected_it3_cost_names <- reactive({
    df <- it3_cost_agg_raw()
    req(!is.null(df), nrow(df) > 0, "name" %in% names(df))
    req(!is.null(input$it3_cost_dataset), nzchar(input$it3_cost_dataset))

    df <- df |> dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset))
    req(nrow(df) > 0)

    names_choices <- sort(unique(as.character(df$name)))
    names_choices <- names_choices[!is.na(names_choices) & nzchar(names_choices)]
    req(length(names_choices) > 0)

    split_var <- input$it3_cost_split_var %||% "none"
    if (!identical(split_var, "none")) {
      selected <- intersect(input$it3_cost_name_single %||% character(0), names_choices)
      if (length(selected) == 0) selected <- intersect(input$it3_cost_name_multi %||% character(0), names_choices)
      if (length(selected) == 0) selected <- names_choices[[1]]
      return(selected[[1]])
    }

    if (identical(input$it3_cost_name_mode %||% "single", "multi")) {
      selected <- intersect(input$it3_cost_name_multi %||% character(0), names_choices)
    } else {
      selected <- intersect(input$it3_cost_name_single %||% character(0), names_choices)
    }
    if (length(selected) == 0) selected <- names_choices[[1]]
    selected
  })

  output$it3_cost_name_ui <- renderUI({
    df <- it3_cost_agg_raw()
    req(!is.null(df), nrow(df) > 0, "name" %in% names(df))
    req(!is.null(input$it3_cost_dataset), nzchar(input$it3_cost_dataset))

    df <- df |> dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset))
    req(nrow(df) > 0)

    names_choices <- sort(unique(as.character(df$name)))
    names_choices <- names_choices[!is.na(names_choices) & nzchar(names_choices)]
    req(length(names_choices) > 0)

    split_var <- input$it3_cost_split_var %||% "none"
    single_selected <- intersect(isolate(input$it3_cost_name_single) %||% character(0), names_choices)
    multi_selected <- intersect(isolate(input$it3_cost_name_multi) %||% character(0), names_choices)
    if (length(single_selected) == 0) {
      single_selected <- intersect(multi_selected, names_choices)
      if (length(single_selected) == 0) single_selected <- names_choices[[1]]
    }
    if (length(multi_selected) == 0) multi_selected <- single_selected

    choices <- choice_names(names_choices, function(x) pretty_metric_name(x, input$it3_cost_dataset))
    if (!identical(split_var, "none")) {
      return(selectInput("it3_cost_name_single", "Uitkomst", choices = choices, selected = single_selected[[1]]))
    }

    mode <- input$it3_cost_name_mode %||% "single"
    tagList(
      radioButtons(
        "it3_cost_name_mode",
        "Keuze uitkomst",
        choices = c("Een uitkomst" = "single", "Meerdere uitkomsten" = "multi"),
        selected = if (mode %in% c("single", "multi")) mode else "single"
      ),
      if (identical(mode, "multi")) {
        tagList(
          div(
            style = "display: flex; gap: 8px; margin-bottom: 8px;",
            actionButton("it3_cost_select_all", "Alles selecteren"),
            actionButton("it3_cost_select_none", "Alles wissen")
          ),
          checkboxGroupInput("it3_cost_name_multi", "Uitkomst", choices = choices, selected = multi_selected)
        )
      } else {
        selectInput("it3_cost_name_single", "Uitkomst", choices = choices, selected = single_selected[[1]])
      }
    )
  })

  observeEvent(input$it3_cost_select_all, {
    df <- it3_cost_agg_raw()
    req(!is.null(df), nrow(df) > 0, "name" %in% names(df))
    req(!is.null(input$it3_cost_dataset), nzchar(input$it3_cost_dataset))
    name_vals <- df |>
      dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset)) |>
      dplyr::pull(name) |>
      unique()
    updateCheckboxGroupInput(session, "it3_cost_name_multi", selected = sort(as.character(name_vals)))
  })

  observeEvent(input$it3_cost_select_none, {
    updateCheckboxGroupInput(session, "it3_cost_name_multi", selected = character(0))
  })

  output$it3_cost_died_ui <- renderUI({
    df <- it3_cost_agg_raw()
    req(!is.null(df), nrow(df) > 0)
    req(!is.null(input$it3_cost_dataset), nzchar(input$it3_cost_dataset))
    df <- df |> dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset))
    req(nrow(df) > 0, "died" %in% names(df))

    values <- ordered_values(df$died)
    if (length(values) <= 1) return(NULL)

    selected <- isolate(input$it3_cost_died)
    if (is.null(selected) || length(intersect(selected, values)) == 0) {
      selected <- values
    } else {
      selected <- intersect(selected, values)
    }

    checkboxGroupInput(
      "it3_cost_died",
      "Populatie",
      choices = choice_names(values, population_label),
      selected = selected
    )
  })

  it3_zpk_filtered <- reactive({
    df <- it3_zpk_raw
    req(!is.null(df), nrow(df) > 0)

    df <- filter_it3_zpk_eq(df, "died", input$it3_zpk_died)
    df <- filter_it3_zpk_eq(df, "cohort", input$it3_zpk_cohort)
    df <- filter_it3_zpk_eq(df, "vektmszsettingzpk", input$it3_zpk_setting)
    df <- filter_it3_zpk_eq(df, "bin_size", input$it3_zpk_bin)

    prest_col <- it3_zpk_prestatie_colname(df)
    prest_sel <- normalize_it3_filter_value(input$it3_zpk_prestatie_type)
    prest_sel <- if (length(prest_sel) == 1) prest_sel else NA_character_

    if (!is.na(prest_col) && !is.na(prest_sel) && identical(prest_sel, "All")) {
      df <- df |>
        dplyr::filter(normalize_it3_filter_value(.data[[prest_col]]) %in% c("DBC", "OZP"))
      df <- it3_zpk_combine_prestatie_types(df)
    } else if (!is.na(prest_col)) {
      df <- filter_it3_zpk_eq(df, prest_col, input$it3_zpk_prestatie_type)
    }

    df
  })

  it3_cost_agg_filtered <- reactive({
    df <- it3_cost_agg_raw()
    req(!is.null(df), nrow(df) > 0)

    split_var <- input$it3_cost_split_var %||% "none"

    if ("sheet" %in% names(df) && !is.null(input$it3_cost_dataset) && nzchar(input$it3_cost_dataset)) {
      df <- df |> dplyr::filter(as.character(sheet) == as.character(input$it3_cost_dataset))
    }

    # Filter by selected outcome(s) (name)
    names_keep <- selected_it3_cost_names()
    if ("name" %in% names(df) && length(names_keep) > 0) {
      df <- df |> dplyr::filter(as.character(.data$name) %in% names_keep)
    }

    # Apply demographic filters / split handling
    dim_cols <- it3_cost_agg_dim_cols(df)
    varied_cols <- it3_cost_agg_varied_dim_cols(df)
    for (col in dim_cols) {
      if (identical(split_var, col)) {
        df <- df |> dplyr::filter(as.character(.data[[col]]) != "all")
      } else if (col %in% varied_cols) {
        sel <- input[[it3_cost_dim_input_id(col)]]
        if (!is.null(sel) && nzchar(sel)) {
          df <- df |> dplyr::filter(as.character(.data[[col]]) == as.character(sel))
        }
      } else if ("all" %in% as.character(df[[col]])) {
        df <- df |> dplyr::filter(as.character(.data[[col]]) == "all")
      }
    }

    # Apply cohort / died / bin filters
    if ("died" %in% names(df) && !is.null(input$it3_cost_died) && length(input$it3_cost_died) > 0) {
      died_values <- ordered_values(df$died)
      selected_died <- intersect(input$it3_cost_died, died_values)
      if (length(selected_died) == 0) selected_died <- died_values
      df <- df |> dplyr::filter(as.character(died) %in% selected_died)
    }
    if ("cohort" %in% names(df) && !is.null(input$it3_cost_cohort) && nzchar(input$it3_cost_cohort)) {
      df <- df |> dplyr::filter(as.character(cohort) == as.character(input$it3_cost_cohort))
    }
    if ("bin_size" %in% names(df) && !is.null(input$it3_cost_bin) && nzchar(input$it3_cost_bin)) {
      df <- df |> dplyr::filter(as.character(bin_size) == as.character(input$it3_cost_bin))
    }
    
    df
  })

  observe({
    df <- it3_cost_agg_filtered()
    if (is.null(df) || nrow(df) == 0 || !"type" %in% names(df)) return()

    names_keep <- selected_it3_cost_names()
    if (length(names_keep) == 0) return()

    choices <- it3_cost_metric_choices_combined(df, input$it3_cost_dataset, names_keep)
    if (length(choices) == 0) return()

    type_vals <- sort(unique(as.character(df$type)))
    type_vals <- type_vals[!is.na(type_vals) & nzchar(type_vals)]

    selected <- input$it3_cost_metric
    if (is.null(selected) || identical(selected, "__loading__") || !selected %in% unname(choices)) {
      selected <- dplyr::case_when(
        "n_totaal_gebruikers" %in% type_vals ~ "n_totaal_gebruikers",
        "sum_totaal_groep" %in% type_vals ~ "sum_totaal_groep",
        TRUE ~ unname(choices)[[1]]
      )
    }

    updateRadioButtons(session, "it3_cost_metric", choices = choices, selected = selected)
  })

  output$it3_plot_cost_agg <- renderPlotly({
    df <- it3_cost_agg_filtered()
    metric <- input$it3_cost_metric
    req(!is.null(metric), nzchar(metric), !identical(metric, "__loading__"))
    split_var <- input$it3_cost_split_var %||% "none"
    sheet <- input$it3_cost_dataset

    df <- prepare_it3_cost_agg_metric_df(df, metric)
    if (nrow(df) == 0) {
      return(
        plotly::plot_ly() |>
          plotly::layout(
            annotations = list(
              list(
                text = "Geen data beschikbaar voor deze selectie.",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE
              )
            )
          )
      )
    }

    if ("name" %in% names(df)) {
      df$outcome_label <- pretty_metric_name(df$name, sheet)
    } else {
      df$outcome_label <- "Uitkomst"
    }
    if ("died" %in% names(df)) {
      df$died_label <- population_label(df$died)
    } else {
      df$died_label <- "Totaal"
    }

    df <- df |>
      dplyr::mutate(
        t_num = numericize(t),
        t_label = as.character(t),
        split_value = if (split_var == "none" || !split_var %in% names(df)) NA_character_ else as.character(.data[[split_var]])
      ) |>
      dplyr::filter(!is.na(metric_value))

    req(nrow(df) > 0)

    multiple_outcomes <- "name" %in% names(df) && dplyr::n_distinct(df$name) > 1
    multiple_populations <- "died" %in% names(df) && dplyr::n_distinct(df$died_label) > 1
    y_label <- it3_cost_metric_label(metric)
    bin_uses_split_bar <- as.character(input$it3_cost_bin %||% "") %in% c("1000", "24months")
    single_t <- dplyr::n_distinct(df$t_label) == 1
    use_split_bar <- !identical(split_var, "none") && bin_uses_split_bar && single_t

    if (identical(split_var, "none")) {
      t_levels <- df |>
        dplyr::distinct(t_label, t_num) |>
        dplyr::arrange(t_num) |>
        dplyr::pull(t_label)

      tooltip_text <- paste0(
        if (multiple_outcomes) paste0("Uitkomst: ", df$outcome_label, "<br>") else "",
        "t: ", df$t_label, "<br>",
        "Populatie: ", df$died_label, "<br>",
        "Waarde: ", it3_cost_agg_format_value(df$metric_value, metric)
      )

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = factor(t_label, levels = t_levels),
          y = metric_value,
          fill = died_label,
          text = tooltip_text
        )
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.75, preserve = "single")) +
        ggplot2::scale_fill_manual(values = population_palette) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = if (multiple_populations) "bottom" else "none", panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::labs(x = "t", y = y_label, fill = NULL)

      if (multiple_outcomes) {
        p <- p + ggplot2::facet_wrap(~ outcome_label, scales = "fixed")
      }
    } else if (use_split_bar) {
      levels_order <- pretty_value(split_var, ordered_split_values(split_var, df$split_value))
      df <- df |>
        dplyr::mutate(
          split_label = pretty_value(split_var, split_value),
          split_label = if (length(levels_order) > 0) {
            factor(split_label, levels = levels_order)
          } else {
            stats::reorder(split_label, metric_value)
          }
        ) |>
        dplyr::arrange(outcome_label, split_label, died_label)

      tooltip_text <- paste0(
        if (multiple_outcomes) paste0("Uitkomst: ", df$outcome_label, "<br>") else "",
        pretty_split_name(split_var), ": ", df$split_label, "<br>",
        "Populatie: ", df$died_label, "<br>",
        "Waarde: ", it3_cost_agg_format_value(df$metric_value, metric)
      )

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = split_label,
          y = metric_value,
          fill = died_label,
          text = tooltip_text
        )
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.75, preserve = "single")) +
        ggplot2::scale_fill_manual(values = population_palette) +
        ggplot2::scale_x_discrete(labels = function(x) axis_label(x, 14)) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = if (multiple_populations) "bottom" else "none",
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
        ) +
        ggplot2::labs(x = NULL, y = y_label, fill = NULL)

      if (multiple_outcomes) {
        p <- p + ggplot2::facet_wrap(~ outcome_label, scales = "fixed")
      }
    } else {
      df <- df |>
        dplyr::arrange(outcome_label, split_value, t_num)

      tooltip_text <- paste0(
        if (multiple_outcomes) paste0("Uitkomst: ", df$outcome_label, "<br>") else "",
        "t: ", df$t_label, "<br>",
        pretty_split_name(split_var), ": ", pretty_value(split_var, df$split_value), "<br>",
        if (multiple_populations) paste0("Populatie: ", df$died_label, "<br>") else "",
        "Waarde: ", it3_cost_agg_format_value(df$metric_value, metric)
      )

      line_group <- if (multiple_populations) {
        paste(df$split_value, df$outcome_label, df$died_label, sep = " | ")
      } else {
        paste(df$split_value, df$outcome_label, sep = " | ")
      }
      line_label <- if (multiple_populations) {
        paste(pretty_value(split_var, df$split_value), df$died_label, sep = " | ")
      } else {
        pretty_value(split_var, df$split_value)
      }

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = t_num,
          y = metric_value,
          color = line_label,
          group = line_group,
          text = tooltip_text
        )
      ) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::labs(x = "t", y = y_label, color = NULL)

      if (multiple_outcomes) {
        p <- p + ggplot2::facet_wrap(~ outcome_label, scales = "fixed")
      }
    }

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  it3_cost_export_data <- reactive({
    metric <- input$it3_cost_metric
    req(!is.null(metric), nzchar(metric), !identical(metric, "__loading__"))

    df <- prepare_it3_cost_agg_metric_df(it3_cost_agg_filtered(), metric)
    if (nrow(df) == 0) return(df)

    split_var <- input$it3_cost_split_var %||% "none"
    if ("died" %in% names(df)) {
      df$died_label <- population_label(df$died)
    } else {
      df$died_label <- "Totaal"
    }

    multiple_outcomes <- "name" %in% names(df) && dplyr::n_distinct(df$name) > 1
    bin_uses_split_bar <- as.character(input$it3_cost_bin %||% "") %in% c("1000", "24months")
    single_t <- dplyr::n_distinct(df$t) == 1
    use_split_bar <- !identical(split_var, "none") && bin_uses_split_bar && single_t

    if (identical(split_var, "none")) {
      t_levels <- df |>
        dplyr::distinct(t_label = as.character(t), t_num = numericize(t)) |>
        dplyr::arrange(t_num) |>
        dplyr::pull(t_label)
      df |>
        dplyr::mutate(
          category = factor(as.character(t), levels = t_levels),
          series = if (multiple_outcomes) combine_series(died_label, name) else as.character(died_label),
          export_value = metric_value
        ) |>
        dplyr::mutate(category = as.character(category))
    } else if (use_split_bar) {
      df |>
        dplyr::mutate(
          category = as.character(pretty_value(split_var, .data[[split_var]])),
          series = as.character(died_label),
          export_value = metric_value
        )
    } else {
      df |>
        dplyr::mutate(
          category = as.character(numericize(t)),
          series = combine_series(pretty_value(split_var, .data[[split_var]]), died_label),
          export_value = metric_value
        )
    }
  })

  it3_cost_chart_type <- reactive({
    split_var <- input$it3_cost_split_var %||% "none"
    bin_uses_split_bar <- as.character(input$it3_cost_bin %||% "") %in% c("1000", "24months")
    df <- it3_cost_agg_filtered()
    single_t <- nrow(df) > 0 && dplyr::n_distinct(df$t) == 1
    use_split_bar <- !identical(split_var, "none") && bin_uses_split_bar && single_t
    if (identical(split_var, "none") || use_split_bar) "grouped_bar" else "line"
  })

  chart_data_downloads_server(
    id = "iter3_cost_dl",
    data = it3_cost_export_data,
    chart_type = it3_cost_chart_type,
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_cost_agg",
    agg_fun = NULL
  )

  observe({
    df <- it3_map_cost_base_data(it3_cost_agg_raw_3c)
    if (is.null(df) || nrow(df) == 0) return()

    pick_choice <- function(choices, current, preferred = NULL) {
      if (!is.null(current) && current %in% choices) return(current)
      if (!is.null(preferred) && preferred %in% choices) return(preferred)
      choices[[1]]
    }

    if ("cohort" %in% names(df)) {
      cohort_vals <- ordered_values(df$cohort)
      selected_cohort <- pick_choice(cohort_vals, input$it3_map_cohort, preferred = "2023")
      if (!is.null(selected_cohort)) {
        updateSelectInput(session, "it3_map_cohort", choices = cohort_vals, selected = selected_cohort)
      }
    }
    if ("died" %in% names(df)) {
      died_vals <- ordered_values(df$died)
      selected_died <- pick_choice(died_vals, input$it3_map_died)
      if (!is.null(selected_died)) {
        updateSelectInput(session, "it3_map_died", choices = died_vals, selected = selected_died)
      }
    }
  })

  observe({
    df <- it3_map_cost_base_data(it3_cost_agg_raw_3c)
    if (is.null(df) || nrow(df) == 0 || !"type" %in% names(df)) return()

    if ("cohort" %in% names(df) && !is.null(input$it3_map_cohort) && nzchar(input$it3_map_cohort)) {
      df <- df |> dplyr::filter(as.character(.data$cohort) == as.character(input$it3_map_cohort))
    }
    if ("died" %in% names(df) && !is.null(input$it3_map_died) && nzchar(input$it3_map_died)) {
      df <- df |> dplyr::filter(as.character(.data$died) == as.character(input$it3_map_died))
    }
    if (nrow(df) == 0) return()

    choices <- it3_cost_agg_metric_choices(df, it3_map_sheet)
    if (length(choices) == 0) return()

    type_vals <- sort(unique(as.character(df$type)))
    type_vals <- type_vals[!is.na(type_vals) & nzchar(type_vals)]

    selected <- input$it3_map_metric
    if (is.null(selected) || identical(selected, "__loading__") || !selected %in% unname(choices)) {
      selected <- dplyr::case_when(
        "kosten_per_persoon" %in% unname(choices) ~ "kosten_per_persoon",
        "n_totaal_gebruikers" %in% type_vals ~ "n_totaal_gebruikers",
        "sum_totaal_groep" %in% type_vals ~ "sum_totaal_groep",
        TRUE ~ unname(choices)[[1]]
      )
    }

    updateRadioButtons(session, "it3_map_metric", choices = choices, selected = selected)
  })

  it3_map_cost_filtered <- reactive({
    df <- it3_map_cost_base_data(it3_cost_agg_raw_3c)
    req(!is.null(df), nrow(df) > 0)

    if ("cohort" %in% names(df) && !is.null(input$it3_map_cohort) && nzchar(input$it3_map_cohort)) {
      df <- df |> dplyr::filter(as.character(.data$cohort) == as.character(input$it3_map_cohort))
    }
    if ("died" %in% names(df) && !is.null(input$it3_map_died) && nzchar(input$it3_map_died)) {
      df <- df |> dplyr::filter(as.character(.data$died) == as.character(input$it3_map_died))
    }

    metric <- input$it3_map_metric
    req(!is.null(metric), nzchar(metric), !identical(metric, "__loading__"))

    prepare_it3_cost_agg_metric_df(df, metric)
  })

  it3_map_cost_sf <- reactive({
    df <- it3_map_cost_filtered()
    build_it3_cost_map_sf(df, it3_provinces_sf)
  })

  output$it3_plot_cost_map <- renderPlot({
    metric <- input$it3_map_metric
    req(!is.null(metric), nzchar(metric), !identical(metric, "__loading__"))

    build_it3_cost_map_plot(
      it3_map_cost_sf(),
      metric = metric,
      cohort = input$it3_map_cohort,
      died = input$it3_map_died
    )
  })

  output$it3_dl_cost_map <- downloadHandler(
    filename = function() {
      paste0(
        "iteratie3_kostenkaart_",
        input$it3_map_metric %||% "metric",
        "_",
        input$it3_map_cohort %||% "cohort",
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      metric <- input$it3_map_metric
      req(!is.null(metric), nzchar(metric), !identical(metric, "__loading__"))

      p <- build_it3_cost_map_plot(
        it3_map_cost_sf(),
        metric = metric,
        cohort = input$it3_map_cohort,
        died = input$it3_map_died
      )
      ggplot2::ggsave(
        filename = file,
        plot = p,
        device = "png",
        width = 12,
        height = 9,
        dpi = 300,
        bg = "white"
      )
    }
  )

  output$it3_plot_zpk <- renderPlotly({
    metric <- input$it3_zpk_metric %||% "n_totaal_gebruikers"

    if (it3_zpk_metric_unavailable_for_all(metric, input$it3_zpk_prestatie_type)) {
      return(it3_zpk_empty_plot(
        "Kan niet worden berekend bij Prestatietype All voor deze variabele."
      ))
    }

    df <- it3_zpk_filtered()

    req("t" %in% names(df), "zpk_category" %in% names(df))
    if (!identical(metric, "gemiddelde_kosten_per_persoon")) {
      req(metric %in% names(df))
    } else {
      req("sum_totaal_groep" %in% names(df), "n_totaal_population" %in% names(df))
    }

    df <- df |>
      dplyr::mutate(
        t_num = numericize(t),
        t_label = as.character(t),
        zpk_category = as.character(zpk_category),
        metric_value = it3_zpk_metric_values(df, metric),
        pop_value = numericize(.data[["n_totaal_population"]] %||% NA_real_),
        tooltip = paste0(
          "t: ", t_label, "<br>",
          "ZPK categorie: ", zpk_category, "<br>",
          "Waarde: ", it3_zpk_format_metric(metric_value, metric), "<br>",
          "n_totaal_population: ", scales::comma(pop_value, big.mark = ",", decimal.mark = ".")
        )
      ) |>
      dplyr::filter(!is.na(metric_value))

    req(nrow(df) > 0)

    title_parts <- c(
      "Iteratie 3 | ZPK categorieën tellingen",
      paste0("died: ", input$it3_zpk_died %||% "-"),
      paste0("cohort: ", input$it3_zpk_cohort %||% "-"),
      paste0("setting: ", pretty_vektmszsettingzpk(input$it3_zpk_setting %||% "-")),
      paste0("bin: ", input$it3_zpk_bin %||% "-"),
      paste0("prestatietype: ", input$it3_zpk_prestatie_type %||% "-"),
      paste0("variabele: ", pretty_it3_zpk_metric(metric))
    )

    t_levels <- df |>
      dplyr::distinct(t_label, t_num) |>
      dplyr::arrange(t_num) |>
      dplyr::pull(t_label)

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = factor(t_label, levels = t_levels), y = metric_value, fill = zpk_category, text = tooltip)
    ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x = "t", y = NULL, fill = "ZPK categorie") +
      ggplot2::ggtitle(paste(title_parts, collapse = " | "))

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  it3_zpk_export_data <- reactive({
    metric <- input$it3_zpk_metric %||% "n_totaal_gebruikers"
    if (it3_zpk_metric_unavailable_for_all(metric, input$it3_zpk_prestatie_type)) {
      return(tibble::tibble())
    }

    df <- it3_zpk_filtered()
    req("t" %in% names(df), "zpk_category" %in% names(df))
    if (!identical(metric, "gemiddelde_kosten_per_persoon")) {
      req(metric %in% names(df))
    } else {
      req("sum_totaal_groep" %in% names(df), "n_totaal_population" %in% names(df))
    }

    df |>
      dplyr::mutate(
        t_label = as.character(t),
        zpk_category = as.character(zpk_category),
        metric_value = it3_zpk_metric_values(df, metric)
      ) |>
      dplyr::filter(!is.na(metric_value)) |>
      dplyr::transmute(
        category = t_label,
        series = zpk_category,
        metric_value = metric_value
      )
  })

  chart_data_downloads_server(
    id = "iter3_zpk_dl",
    data = it3_zpk_export_data,
    chart_type = "stacked_bar",
    category_col = "category",
    series_col = "series",
    value_col = "metric_value",
    filename_prefix = "iteratie3_zpk",
    agg_fun = NULL
  )

  it3_top50_filtered <- reactive({
    df <- it3_top50_raw
    req(!is.null(df), nrow(df) > 0, "ranked_by" %in% names(df))
    rb <- input$it3_top50_ranked_by
    req(!is.null(rb), nzchar(rb))
    df |> dplyr::filter(as.character(ranked_by) == as.character(rb))
  })

  it3_top50_compare_suffix <- reactive({
    rb <- input$it3_top50_ranked_by %||% ""
    group <- input$it3_top50_compare_group %||% "died"
    if (identical(group, "died")) {
      if (stringr::str_detect(rb, "_In_leven")) return("_Overleden")
      if (stringr::str_detect(rb, "_Overleden")) return("_In_leven")
      return(NA_character_)
    }
    # group == "bin size"
    if (stringr::str_detect(rb, "_1000d")) return("_30d")
    if (stringr::str_detect(rb, "_30d")) return("_1000d")
    NA_character_
  })

  it3_top50_opposite_label <- reactive({
    rb <- input$it3_top50_ranked_by %||% ""
    group <- input$it3_top50_compare_group %||% "died"
    if (identical(group, "died")) {
      if (stringr::str_detect(rb, "_In_leven")) return("Overleden")
      if (stringr::str_detect(rb, "_Overleden")) return("In leven")
      return("onbekend")
    }
    if (stringr::str_detect(rb, "_1000d")) return("30d")
    if (stringr::str_detect(rb, "_30d")) return("1000d")
    "onbekend"
  })

  output$it3_top50_title <- renderText({
    rb <- input$it3_top50_ranked_by %||% "-"
    group <- input$it3_top50_compare_group %||% "-"
    opposite <- it3_top50_opposite_label()
    paste0("Top 50 ", rb, " compared to ", group, ": ", opposite)
  })

  it3_top50_item_label <- function(df, desc_col, code_col) {
    desc <- if (desc_col %in% names(df)) as.character(df[[desc_col]]) else rep("", nrow(df))
    code <- if (code_col %in% names(df)) as.character(df[[code_col]]) else rep("", nrow(df))
    lbl <- ifelse(!is.na(desc) & nzchar(desc), desc, code)
    lbl[is.na(lbl)] <- ""
    lbl
  }

  it3_top50_label_levels_from_df <- function(df, desc_col, code_col) {
    df_lbl <- df |>
      dplyr::mutate(
        lbl = stringr::str_trunc(it3_top50_item_label(df, desc_col, code_col), 70),
        rank_val = if ("ranking" %in% names(df)) as.character(ranking) else as.character(dplyr::row_number())
      )
    if ("ranking" %in% names(df_lbl)) df_lbl <- df_lbl |> dplyr::arrange(ranking)
    lbl_vec <- df_lbl |> dplyr::pull(lbl)
    rank_vec <- df_lbl |> dplyr::pull(rank_val)
    dup <- duplicated(lbl_vec) | duplicated(lbl_vec, fromLast = TRUE)
    if (any(dup, na.rm = TRUE)) lbl_vec[dup] <- paste0(lbl_vec[dup], " (#", rank_vec[dup], ")")
    rev(unique(lbl_vec))
  }

  it3_top50_compare_suffix_for <- function(rb, group) {
    if (identical(group, "died")) {
      if (stringr::str_detect(rb, "_In_leven")) return("_Overleden")
      if (stringr::str_detect(rb, "_Overleden")) return("_In_leven")
      return(NA_character_)
    }
    if (stringr::str_detect(rb, "_1000d")) return("_30d")
    if (stringr::str_detect(rb, "_30d")) return("_1000d")
    NA_character_
  }

  it3_top50_opposite_label_for <- function(rb, group) {
    if (identical(group, "died")) {
      if (stringr::str_detect(rb, "_In_leven")) return("Overleden")
      if (stringr::str_detect(rb, "_Overleden")) return("In leven")
      return("onbekend")
    }
    if (stringr::str_detect(rb, "_1000d")) return("30d")
    if (stringr::str_detect(rb, "_30d")) return("1000d")
    "onbekend"
  }

  it3_top50_plot_df <- function(
    df,
    metric_col,
    desc_col = "mszzorgactiviteitomschrijving",
    code_col = "vektmszdeclaratiecode",
    item_label = "Activiteit",
    label_prefix = "",
    label_levels = NULL,
    include_median = FALSE
  ) {
    req(metric_col %in% names(df))
    lbl <- it3_top50_item_label(df, desc_col, code_col)
    lbl_short <- stringr::str_trunc(lbl, 70)
    rank_val <- if ("ranking" %in% names(df)) as.character(df$ranking) else as.character(seq_len(nrow(df)))
    label_display <- lbl_short
    dup <- duplicated(label_display) | duplicated(label_display, fromLast = TRUE)
    if (any(dup, na.rm = TRUE)) {
      label_display[dup] <- paste0(label_display[dup], " (#", rank_val[dup], ")")
    }

    out <- df |>
      dplyr::mutate(
        label_full = lbl,
        label_short = label_display,
        value_num = numericize(.data[[metric_col]]),
        pop_value = numericize(.data[["n_totaal_population"]] %||% NA_real_),
        median_value = if (include_median && "median_cost_per_declaratie" %in% names(df)) {
          numericize(.data[["median_cost_per_declaratie"]])
        } else {
          NA_real_
        },
        tooltip = paste0(
          label_prefix,
          if (nzchar(label_prefix)) "<br>" else "",
          item_label, ": ", lbl, "<br>",
          "Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = "."), "<br>",
          if (include_median && !all(is.na(median_value))) {
            paste0("Mediaan kosten per declaratie: ", scales::comma(median_value, big.mark = ",", decimal.mark = "."), "<br>")
          } else {
            ""
          },
          "n_totaal_population: ", scales::comma(pop_value, big.mark = ",", decimal.mark = ".")
        )
      )
    out <- out |> dplyr::filter(!is.na(value_num))
    out <- if ("ranking" %in% names(out)) dplyr::arrange(out, ranking) else dplyr::arrange(out, dplyr::desc(value_num))
    out <- out |>
      dplyr::mutate(
        label_short = {
          if (!is.null(label_levels) && length(label_levels) > 0) {
            factor(label_short, levels = label_levels)
          } else {
            factor(label_short, levels = unique(label_short))
          }
        }
      )

    out
  }

  it3_top50_render_bar_plot <- function(df_plot, fill_color, hide_y_axis = FALSE) {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = label_short, y = value_num, text = tooltip)) +
      ggplot2::geom_col(fill = fill_color) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.y = if (hide_y_axis) ggplot2::element_blank() else ggplot2::element_text(),
        axis.ticks.y = if (hide_y_axis) ggplot2::element_blank() else ggplot2::element_line()
      ) +
      ggplot2::labs(x = NULL, y = NULL)

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  }

  output$it3_plot_top50_main <- renderPlotly({
    df <- it3_top50_filtered()
    metric <- input$it3_top50_metric %||% "n_totaal_gebruikers"
    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszzorgactiviteitomschrijving",
      "vektmszdeclaratiecode"
    )

    df_plot <- it3_top50_plot_df(
      df,
      metric,
      label_prefix = "Geselecteerd (ranked_by)",
      label_levels = label_levels
    )
    req(nrow(df_plot) > 0)
    it3_top50_render_bar_plot(df_plot, "#2563EB")
  })

  output$it3_plot_top50_compare <- renderPlotly({
    df <- it3_top50_filtered()
    metric <- input$it3_top50_metric %||% "n_totaal_gebruikers"
    suffix <- it3_top50_compare_suffix()
    validate(need(!is.na(suffix) && nzchar(suffix), "Kan geen vergelijking bepalen uit ranked_by."))

    compare_col <- paste0(metric, suffix)
    validate(need(compare_col %in% names(df), paste0("Vergelijkingskolom ontbreekt: ", compare_col)))

    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszzorgactiviteitomschrijving",
      "vektmszdeclaratiecode"
    )

    df_plot <- it3_top50_plot_df(
      df,
      compare_col,
      label_prefix = paste0("Vergelijking: ", compare_col),
      label_levels = label_levels
    )
    req(nrow(df_plot) > 0)
    it3_top50_render_bar_plot(df_plot, "#10B981", hide_y_axis = TRUE)
  })

  it3_top50_main_export <- reactive({
    df <- it3_top50_filtered()
    metric <- input$it3_top50_metric %||% "n_totaal_gebruikers"
    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszzorgactiviteitomschrijving",
      "vektmszdeclaratiecode"
    )
    df_plot <- it3_top50_plot_df(df, metric, label_levels = label_levels)
    if (nrow(df_plot) == 0) return(df_plot)
    df_plot |>
      dplyr::transmute(
        category = as.character(label_short),
        series = "Geselecteerd",
        export_value = value_num
      )
  })

  it3_top50_compare_export <- reactive({
    df <- it3_top50_filtered()
    metric <- input$it3_top50_metric %||% "n_totaal_gebruikers"
    suffix <- it3_top50_compare_suffix()
    if (is.na(suffix) || !nzchar(suffix)) return(tibble::tibble())
    compare_col <- paste0(metric, suffix)
    if (!compare_col %in% names(df)) return(tibble::tibble())
    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszzorgactiviteitomschrijving",
      "vektmszdeclaratiecode"
    )
    df_plot <- it3_top50_plot_df(
      df,
      compare_col,
      label_prefix = paste0("Vergelijking: ", compare_col),
      label_levels = label_levels
    )
    if (nrow(df_plot) == 0) return(df_plot)
    df_plot |>
      dplyr::transmute(
        category = as.character(label_short),
        series = "Vergelijking",
        export_value = value_num
      )
  })

  chart_data_downloads_server(
    id = "iter3_top50_main_dl",
    data = it3_top50_main_export,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_top50_main",
    agg_fun = NULL
  )

  chart_data_downloads_server(
    id = "iter3_top50_cmp_dl",
    data = it3_top50_compare_export,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_top50_compare",
    agg_fun = NULL
  )

  it3_top50_prest_filtered <- reactive({
    df <- it3_top50_prest_raw
    req(!is.null(df), nrow(df) > 0, "ranked_by" %in% names(df))
    rb <- input$it3_top50_prest_ranked_by
    req(!is.null(rb), nzchar(rb))
    df |> dplyr::filter(as.character(ranked_by) == as.character(rb))
  })

  it3_top50_prest_compare_suffix <- reactive({
    it3_top50_compare_suffix_for(
      input$it3_top50_prest_ranked_by %||% "",
      input$it3_top50_prest_compare_group %||% "died"
    )
  })

  output$it3_top50_prest_title <- renderText({
    rb <- input$it3_top50_prest_ranked_by %||% "-"
    group <- input$it3_top50_prest_compare_group %||% "-"
    opposite <- it3_top50_opposite_label_for(rb, group)
    paste0("Top 50 ", rb, " compared to ", group, ": ", opposite)
  })

  output$it3_plot_top50_prest_main <- renderPlotly({
    df <- it3_top50_prest_filtered()
    metric <- input$it3_top50_prest_metric %||% "n_totaal_gebruikers"
    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszdbczorgproductomschrijving",
      "vektmszdbczorgproduct"
    )

    df_plot <- it3_top50_plot_df(
      df,
      metric,
      desc_col = "mszdbczorgproductomschrijving",
      code_col = "vektmszdbczorgproduct",
      item_label = "Prestatie",
      label_prefix = "Geselecteerd (ranked_by)",
      label_levels = label_levels,
      include_median = TRUE
    )
    req(nrow(df_plot) > 0)
    it3_top50_render_bar_plot(df_plot, "#2563EB")
  })

  output$it3_plot_top50_prest_compare <- renderPlotly({
    df <- it3_top50_prest_filtered()
    metric <- input$it3_top50_prest_metric %||% "n_totaal_gebruikers"
    suffix <- it3_top50_prest_compare_suffix()
    validate(need(!is.na(suffix) && nzchar(suffix), "Kan geen vergelijking bepalen uit ranked_by."))

    compare_col <- paste0(metric, suffix)
    validate(need(compare_col %in% names(df), paste0("Vergelijkingskolom ontbreekt: ", compare_col)))

    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszdbczorgproductomschrijving",
      "vektmszdbczorgproduct"
    )

    df_plot <- it3_top50_plot_df(
      df,
      compare_col,
      desc_col = "mszdbczorgproductomschrijving",
      code_col = "vektmszdbczorgproduct",
      item_label = "Prestatie",
      label_prefix = paste0("Vergelijking: ", compare_col),
      label_levels = label_levels,
      include_median = TRUE
    )
    req(nrow(df_plot) > 0)
    it3_top50_render_bar_plot(df_plot, "#10B981", hide_y_axis = TRUE)
  })

  it3_top50_prest_main_export <- reactive({
    df <- it3_top50_prest_filtered()
    metric <- input$it3_top50_prest_metric %||% "n_totaal_gebruikers"
    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszdbczorgproductomschrijving",
      "vektmszdbczorgproduct"
    )
    df_plot <- it3_top50_plot_df(
      df,
      metric,
      desc_col = "mszdbczorgproductomschrijving",
      code_col = "vektmszdbczorgproduct",
      item_label = "Prestatie",
      label_levels = label_levels,
      include_median = TRUE
    )
    if (nrow(df_plot) == 0) return(df_plot)
    df_plot |>
      dplyr::transmute(
        category = as.character(label_short),
        series = "Geselecteerd",
        export_value = value_num
      )
  })

  it3_top50_prest_compare_export <- reactive({
    df <- it3_top50_prest_filtered()
    metric <- input$it3_top50_prest_metric %||% "n_totaal_gebruikers"
    suffix <- it3_top50_prest_compare_suffix()
    if (is.na(suffix) || !nzchar(suffix)) return(tibble::tibble())
    compare_col <- paste0(metric, suffix)
    if (!compare_col %in% names(df)) return(tibble::tibble())
    label_levels <- it3_top50_label_levels_from_df(
      df,
      "mszdbczorgproductomschrijving",
      "vektmszdbczorgproduct"
    )
    df_plot <- it3_top50_plot_df(
      df,
      compare_col,
      desc_col = "mszdbczorgproductomschrijving",
      code_col = "vektmszdbczorgproduct",
      item_label = "Prestatie",
      label_prefix = paste0("Vergelijking: ", compare_col),
      label_levels = label_levels,
      include_median = TRUE
    )
    if (nrow(df_plot) == 0) return(df_plot)
    df_plot |>
      dplyr::transmute(
        category = as.character(label_short),
        series = "Vergelijking",
        export_value = value_num
      )
  })

  chart_data_downloads_server(
    id = "iter3_prest_main_dl",
    data = it3_top50_prest_main_export,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_top50_prest_main",
    agg_fun = NULL
  )

  chart_data_downloads_server(
    id = "iter3_prest_cmp_dl",
    data = it3_top50_prest_compare_export,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_top50_prest_compare",
    agg_fun = NULL
  )

  it3_acp_pop_filtered <- reactive({
    df <- it3_acp_pop_raw
    req(!is.null(df), nrow(df) > 0, "split_by" %in% names(df))
    split_sel <- input$it3_acp_split_by
    req(!is.null(split_sel), nzchar(split_sel))
    df <- df |> dplyr::filter(as.character(split_by) == as.character(split_sel))
    req(nrow(df) > 0)

    df |>
      dplyr::mutate(
        group_label = if (identical(split_sel, "cohort")) {
          as.character(group)
        } else {
          pretty_value(split_sel, group)
        },
        metric_value = numericize(n_users_acp_consults_2years),
        died_label = population_label(died)
      ) |>
      dplyr::filter(!is.na(metric_value))
  })

  output$it3_table_acp_ages <- renderTable({
    df <- it3_acp_ages_raw
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    df |>
      dplyr::transmute(
        Populatie = died,
        Geslacht = geslacht,
        `Gemiddelde leeftijd` = round(numericize(age_acp_user), 1),
        `N populatie` = scales::comma(numericize(n_population), big.mark = ",", decimal.mark = ".")
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$it3_plot_acp_pop <- renderPlotly({
    df <- it3_acp_pop_filtered()
    req(nrow(df) > 0)

    multiple_populations <- dplyr::n_distinct(df$died_label) > 1

    df <- df |>
      dplyr::mutate(
        tooltip = paste0(
          "Groep: ", group_label, "<br>",
          "Populatie: ", died_label, "<br>",
          "Aantal gebruikers: ",
          scales::comma(metric_value, big.mark = ",", decimal.mark = ".")
        )
      )

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = reorder(group_label, metric_value),
        y = metric_value,
        fill = died_label,
        text = tooltip
      )
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.75, preserve = "single")) +
      ggplot2::scale_fill_manual(values = population_palette) +
      ggplot2::scale_y_continuous(labels = scales::comma_format(big.mark = ",", decimal.mark = ".")) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        legend.position = if (multiple_populations) "bottom" else "none",
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      ) +
      ggplot2::labs(x = NULL, y = "Aantal gebruikers", fill = NULL)

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  it3_acp_export_data <- reactive({
    df <- it3_acp_pop_filtered()
    if (nrow(df) == 0) return(df)
    df |>
      dplyr::transmute(
        category = as.character(group_label),
        series = as.character(died_label),
        export_value = metric_value
      )
  })

  chart_data_downloads_server(
    id = "iter3_acp_dl",
    data = it3_acp_export_data,
    chart_type = "grouped_bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_acp_descriptives",
    agg_fun = NULL
  )

  it3_zvwk_filtered <- reactive({
    df <- it3_zvwk_raw
    req(!is.null(df), nrow(df) > 0)

    if ("cost_type" %in% names(df) && !is.null(input$it3_zvwk_cost_type) && nzchar(input$it3_zvwk_cost_type)) {
      df <- df |> dplyr::filter(as.character(cost_type) == as.character(input$it3_zvwk_cost_type))
    }
    if ("cohort" %in% names(df) && !is.null(input$it3_zvwk_cohort) && nzchar(input$it3_zvwk_cohort)) {
      df <- df |> dplyr::filter(as.character(cohort) == as.character(input$it3_zvwk_cohort))
    }
    df
  })

  output$it3_plot_zvwk <- renderPlotly({
    df <- it3_zvwk_filtered()
    req(nrow(df) > 0, "cost_bin" %in% names(df), "n_population" %in% names(df))

    ref_col <- input$it3_zvwk_ref_line %||% "mean_costs_all"
    req(ref_col %in% names(df))
    ref_val <- numericize(df[[ref_col]][1])

    df <- prepare_it3_zvwk_hist_df(df)
    req(nrow(df) > 0)

    ref_label <- names(it3_zvwk_ref_line_choices)[match(ref_col, it3_zvwk_ref_line_choices)]
    ref_bin <- if (!is.na(ref_val)) {
      it3_zvwk_bin_for_value(levels(df$cost_bin_label), ref_val)
    } else {
      NA_character_
    }

    df <- df |>
      dplyr::mutate(
        tooltip = paste0(
          "Kostenbin: ", cost_bin, "<br>",
          "Aantal personen: ", scales::comma(pop_num, big.mark = ",", decimal.mark = "."),
          if (!is.na(ref_val)) {
            paste0(
              "<br>", ref_label, ": ",
              scales::comma(ref_val, big.mark = ",", decimal.mark = ".")
            )
          } else {
            ""
          }
        )
      )

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = cost_bin_label, y = pop_num, text = tooltip)
    ) +
      ggplot2::geom_col(fill = "#2563EB", width = 0.85) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      ) +
      ggplot2::labs(
        x = "Kosten (cost_bin)",
        y = "Aantal personen (n_population)",
        title = paste0(
          "ZVW-kosten verdeling | ",
          pretty_metric_name(input$it3_zvwk_cost_type %||% "-"), " | cohort ",
          input$it3_zvwk_cohort %||% "-"
        )
      )

    if (!is.na(ref_bin) && nzchar(ref_bin)) {
      p <- p +
        ggplot2::geom_vline(
          xintercept = ref_bin,
          color = "#DC2626",
          linewidth = 1,
          linetype = "dashed"
        )
    }

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  it3_zvwk_export_data <- reactive({
    prepare_it3_zvwk_hist_df(it3_zvwk_filtered())
  })

  chart_data_downloads_server(
    id = "iter3_zvwk_dl",
    data = it3_zvwk_export_data,
    chart_type = "scatter",
    category_col = "cost_bin",
    series_col = "cost_bin",
    value_col = "pop_num",
    filename_prefix = "iteratie3_zvwk",
    agg_fun = NULL
  )

  it3_regression_filtered <- reactive({
    df <- it3_regression_raw()
    req(!is.null(df), nrow(df) > 0)
    
    cohort <- input$it3_reg_used_cohorts
    if (!is.null(cohort) && nzchar(cohort) && "used_cohorts" %in% names(df)) {
      df <- df |> dplyr::filter(as.character(used_cohorts) == as.character(cohort))
    }
    
    dep <- input$it3_reg_dependent_var
    req(!is.null(dep), nzchar(dep))
    df |> dplyr::filter(as.character(dependent_var) == as.character(dep))
  })

  output$it3_reg_title <- renderText({
    dep <- input$it3_reg_dependent_var %||% "-"
    paste0("Regressiecoëfficiënten voor ", dep)
  })

  output$it3_reg_plot_ui <- renderUI({
    df <- it3_regression_filtered()
    n <- if (!is.null(df)) nrow(df) else 0
    plot_height <- max(580, min(2800, n * 28 + 160))
    plotlyOutput("it3_plot_regression", height = paste0(plot_height, "px"))
  })

  output$it3_plot_regression <- renderPlotly({
    df <- it3_regression_filtered()
    req(nrow(df) > 0)

    df_plot <- df |>
      dplyr::mutate(
        coef_label = stringr::str_wrap(coefficient, width = 42),
        sig_label = factor(
          regression_sig_label(p_value),
          levels = names(regression_sig_palette)
        ),
        tooltip = paste0(
          "Coëfficiënt: ", coefficient, "<br>",
          "Schatting: ", scales::number(estimate, accuracy = 0.0001), "<br>",
          "Standaardfout: ", scales::number(std_error, accuracy = 0.0001), "<br>",
          "95% CI: [", scales::number(ci_lower, accuracy = 0.0001), ", ",
          scales::number(ci_upper, accuracy = 0.0001), "]",
          if (!is.na(p_value[1])) paste0("<br>p-waarde: ", scales::number(p_value, accuracy = 0.0001)) else "",
          if (!is.na(n_obs[1])) paste0("<br>n_obs: ", scales::comma(n_obs[1], big.mark = ",")) else ""
        )
      ) |>
      dplyr::arrange(coefficient) |>
      dplyr::mutate(coef_label = factor(coef_label, levels = rev(unique(coef_label))))

    p <- ggplot2::ggplot(
      df_plot,
      ggplot2::aes(
        x = estimate,
        y = coef_label,
        color = sig_label,
        text = tooltip
      )
    ) +
      ggplot2::geom_vline(xintercept = 0, color = "#9CA3AF", linewidth = 0.35, linetype = "dashed") +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
        height = 0.22,
        linewidth = 0.55,
        color = "#4B5563"
      ) +
      ggplot2::geom_point(size = 2.4) +
      ggplot2::scale_color_manual(values = regression_sig_palette, name = "Significantie", drop = FALSE) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 9),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = "Schatting (95% betrouwbaarheidsinterval)",
        y = NULL
      )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        margin = list(l = 220),
        hovermode = "closest",
        showlegend = TRUE
      ) |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  it3_regression_export_data <- reactive({
    df <- it3_regression_filtered()
    if (nrow(df) == 0) return(df)
    df |>
      dplyr::transmute(
        category = as.character(coefficient),
        series = "Coëfficiënt",
        export_value = estimate
      )
  })

  chart_data_downloads_server(
    id = "iter3_reg_dl",
    data = it3_regression_export_data,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "iteratie3_regressie",
    agg_fun = NULL
  )

  # ==========================================
  # SERVER LOGIC: TAB 1 - Basispopulatie
  # ==========================================
  data_basis <- reactive({
    req(nrow(all_data()) > 0)
    df <- all_data() %>%
      filter(bin_size == "1000days", type == "n_totaal_gebruikers") %>%
      # Use zvwktotaal which has complete population data for all doodsoorzaken
      filter(name == "zvwktotaal")
    
    if(input$pop_jaar != "2019 + 2023") {
      df <- df %>% filter(cohort == as.numeric(input$pop_jaar))
    }
    
    if(input$pop_split == "Enkel totale populatie") {
      df <- df %>% filter(doodsoorzaak == "all")
    }
    
    # Aggregate n_totaal
    df %>%
      group_by(cohort, doodsoorzaak, died) %>%
      summarise(n_mensen = mean(n_totaal, na.rm=TRUE), .groups = "drop")
  })
  
  output$plot_basispopulatie <- plotly::renderPlotly({
    df <- data_basis()
    p <- ggplot(df, aes(x = doodsoorzaak, y = n_mensen, fill = died)) +
      geom_col(position = position_dodge()) +
      facet_wrap(~cohort) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Basispopulatie", x = "Populatie / Doodsoorzaak", y = "Aantal")
    plotly::ggplotly(p)
  })

  data_basis_export <- reactive({
    df <- data_basis()
    if (nrow(df) == 0) return(df)
    df |>
      dplyr::mutate(series = combine_series(died, cohort))
  })

  chart_data_downloads_server(
    id = "iter1_basis_dl",
    data = data_basis_export,
    chart_type = "grouped_bar",
    category_col = "doodsoorzaak",
    series_col = "series",
    value_col = "n_mensen",
    filename_prefix = "basispopulatie",
    agg_fun = NULL
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 2 - Zorg Totaal 1000 dgn
  # ==========================================

  # Update variable choices for tot_variables (exclude intervention variables and gebruik_/heeft_ by default)
  observeEvent(nrow(all_data()) > 0, {
    all_vars <- sort(unique(all_data()$name))
    intervention_names <- get_all_interventie_names()
    # Exclude interventies, gebruik_, and heeft_ prefixed variables
    clean_vars <- all_vars[!all_vars %in% intervention_names &
                           !startsWith(all_vars, "gebruik") &
                           !startsWith(all_vars, "heeft")]
    clean_vars <- sort(clean_vars)

    updateSelectizeInput(
      session,
      "tot_variables",
      choices = clean_vars,
      selected = clean_vars,
      server = TRUE
    )
  }, ignoreInit = FALSE)

  data_totaal <- reactive({
    req(nrow(all_data()) > 0)
    df <- process_measurements(all_data(), input$tot_maatstaf) %>%
      filter(bin_size == input$tot_bin_size,
             doodsoorzaak == input$tot_pop)

    # Handle variable filtering based on measurement type
    if (input$tot_maatstaf == "prevalentie_per_100") {
      # For prevalentie, only show gebruik_ and heeft_ prefixed variables
      df <- df %>% filter(startsWith(name, "gebruik") | startsWith(name, "heeft"))
    } else if (!is.null(input$tot_variables) && length(input$tot_variables) > 0) {
      # For other measurements, filter by selected variables
      df <- df %>% filter(name %in% input$tot_variables)
    }

    if(input$tot_jaar != "Beide") {
      df <- df %>% filter(cohort == as.numeric(input$tot_jaar))
    }
    if(input$tot_vgl == "Geen vergelijking") {
      df <- df %>% filter(died == "Overleden")
    }

    result <- df %>%
      group_by(name, cohort, died) %>%
      summarise(waarde = mean(value, na.rm=TRUE), .groups = "drop")

    # Multiply by 100 for prevalentie_per_100 to convert from decimal to percentage
    if (input$tot_maatstaf == "prevalentie_per_100") {
      result <- result %>% mutate(waarde = waarde * 100)
    }

    result
  })
  
  output$plot_zorg_totaal <- plotly::renderPlotly({
    df <- data_totaal()
    y_label <- if (input$tot_maatstaf == "prevalentie_per_100") "Prevalentie per 100" else "Waarde"
    p <- ggplot(df, aes(x = reorder(name, waarde), y = waarde, fill = died)) +
      geom_col(position = position_dodge()) +
      facet_wrap(~cohort) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Zorg Totaal",
        subtitle = paste0(
          "Maatstaf: ", get_maatstaf_label(input$tot_maatstaf),
          " | Bin size: ", get_bin_size_label(input$tot_bin_size),
          " | Populatie: ", input$tot_pop,
          " | Jaar: ", input$tot_jaar,
          " | Vergelijking: ", input$tot_vgl
        ),
        x = "Zorgdomein",
        y = y_label
      )
    plotly::ggplotly(p)
  })

  data_totaal_export <- reactive({
    df <- data_totaal()
    if (nrow(df) == 0) return(df)
    if (identical(input$tot_jaar, "Beide")) {
      df <- df |> dplyr::mutate(series = combine_series(died, cohort))
    } else {
      df <- df |> dplyr::mutate(series = as.character(died))
    }
    df
  })

  chart_data_downloads_server(
    id = "iter1_totaal_dl",
    data = data_totaal_export,
    chart_type = "grouped_bar",
    category_col = "name",
    series_col = "series",
    value_col = "waarde",
    filename_prefix = "zorg_totaal",
    agg_fun = NULL
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 3 - Zorg over Tijd
  # ==========================================
  data_maandelijks <- reactive({
    req(nrow(all_data()) > 0)
    selected_domains <- input$mnd_domein

    if (is.null(selected_domains) || length(selected_domains) == 0) {
      return(tibble::tibble())
    }

    df <- process_measurements(all_data(), input$mnd_maatstaf) %>%
      filter(bin_size == input$mnd_bin_size)

    # For prevalentie_per_100, map each selected base name to its gebruik_/heeft_ variant.
    if (input$mnd_maatstaf == "prevalentie_per_100") {
      monthly_parts <- lapply(selected_domains, function(selected_domain) {
        target_name <- find_gebruikt_name(selected_domain, df)
        if (is.na(target_name)) {
          return(NULL)
        }
        df %>%
          filter(name == target_name) %>%
          mutate(selected_domein = selected_domain)
      })
      df <- dplyr::bind_rows(monthly_parts)
    } else {
      # For other measurements, use the selected domains directly
      df <- df %>%
        filter(name %in% selected_domains) %>%
        mutate(selected_domein = name)
    }

    if (nrow(df) == 0) {
      return(tibble::tibble())
    }

    if(input$mnd_jaar != "Beide") {
      df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
    }

    if(input$mnd_pop == "all") {
      df <- df %>% filter(doodsoorzaak == "all")
    } else {
      df <- df %>% filter(doodsoorzaak == input$mnd_pop)
    }

    df <- df %>% filter(died == input$mnd_vgl)

    result <- df %>%
      mutate(t_numeric = as.numeric(t)) %>%
      arrange(desc(t_numeric))

    # Multiply by 100 for prevalentie_per_100
    if (input$mnd_maatstaf == "prevalentie_per_100") {
      result <- result %>% mutate(value = value * 100)
    }

    result
  })

  data_maandelijks_lijn <- reactive({
    req(nrow(all_data()) > 0)
    selected_domains <- input$mnd_domein

    if (is.null(selected_domains) || length(selected_domains) == 0) {
      return(tibble::tibble())
    }

    selected_domain <- selected_domains[1]
    df <- process_measurements(all_data(), input$mnd_maatstaf) %>%
      filter(bin_size == input$mnd_bin_size)

    # For prevalentie_per_100, use the first selected base name and resolve its variant.
    if (input$mnd_maatstaf == "prevalentie_per_100") {
      target_name <- find_gebruikt_name(selected_domain, df)
      if (is.na(target_name)) {
        # If no gebruik_/heeft_ variant found, return empty
        return(tibble::tibble())
      }
      df <- df %>% filter(name == target_name)
    } else {
      # For other measurements, use the selected domain directly
      df <- df %>% filter(name == selected_domain)
    }

    log_msg(sprintf("[data_maandelijks_lijn] Base: %d rows (domein=%s, maatstaf=%s)",
                    nrow(df), selected_domain, input$mnd_maatstaf))

    has_all_pop <- any(df$doodsoorzaak == "all", na.rm = TRUE)

    if (input$mnd_lijnmodus == "doodsoorzaak") {
      if(input$mnd_jaar != "Beide") {
        df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
      }
      df <- df %>% filter(doodsoorzaak != "all", died == input$mnd_vgl)
    } else if (input$mnd_lijnmodus == "cohort") {
      if (has_all_pop) {
        df <- df %>% filter(doodsoorzaak == "all")
      }
      if(input$mnd_jaar != "Beide") {
        df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
      }
      df <- df %>% filter(died == input$mnd_vgl)
    } else {
      if(input$mnd_jaar != "Beide") {
        df <- df %>% filter(cohort == as.numeric(input$mnd_jaar))
      }
      if(input$mnd_pop == "all") {
        if (has_all_pop) {
          df <- df %>% filter(doodsoorzaak == "all")
        }
      } else {
        df <- df %>% filter(doodsoorzaak == input$mnd_pop)
      }
      df <- df %>% filter(died == input$mnd_vgl)
    }

    df <- df %>%
      mutate(t_numeric = as.numeric(t)) %>%
      filter(!is.na(t_numeric), !is.na(value)) %>%
      arrange(t_numeric)

    log_msg(sprintf("[data_maandelijks_lijn] Post-filter: %d rows", nrow(df)))
    df
  })

  lijn_data_maandelijks <- reactive({
    df <- data_maandelijks_lijn()
    if (nrow(df) == 0) return(tibble::tibble())

    if (input$mnd_lijnmodus == "doodsoorzaak") {
      df <- df %>% mutate(lijn = doodsoorzaak)
    } else if (input$mnd_lijnmodus == "cohort") {
      df <- df %>% mutate(lijn = paste0("Cohort ", cohort, " - ", died))
    } else {
      df <- df %>% mutate(lijn = died)
    }

    df <- df %>%
      mutate(lijn = trimws(as.character(lijn))) %>%
      group_by(t_numeric, lijn) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    # Multiply by 100 for prevalentie_per_100 to convert from decimal to percentage
    if (input$mnd_maatstaf == "prevalentie_per_100") {
      df <- df %>% mutate(value = value * 100)
    }

    # DEBUG: Print summarized data
    log_msg(sprintf("[lijn_data_maandelijks] Summary: %d rows, lijnen: %s",
                    nrow(df), paste(unique(df$lijn), collapse=", ")))
    df
  })

  lijn_choices_maandelijks <- reactive({
    df <- data_maandelijks_lijn()
    if (nrow(df) == 0) return(character(0))

    if (input$mnd_lijnmodus == "doodsoorzaak") {
      sort(unique(df$doodsoorzaak))
    } else if (input$mnd_lijnmodus == "cohort") {
      sort(unique(paste0("Cohort ", df$cohort, " - ", df$died)))
    } else {
      sort(unique(df$died))
    }
  })

  observeEvent(list(lijn_choices_maandelijks(), input$mnd_grafiek), {
    lijn_choices <- lijn_choices_maandelijks()

    if (input$mnd_grafiek != "Lijngrafiek" || length(lijn_choices) == 0) {
      freezeReactiveValue(input, "mnd_zichtbare_lijnen")
      updateSelectizeInput(
        session,
        "mnd_zichtbare_lijnen",
        choices = lijn_choices,
        selected = character(0),
        server = TRUE
      )
      return()
    }

    selected_lijnen <- isolate(input$mnd_zichtbare_lijnen)
    if (is.null(selected_lijnen)) selected_lijnen <- character(0)

    selected_lijnen <- intersect(selected_lijnen, lijn_choices)
    if (length(selected_lijnen) == 0) {
      selected_lijnen <- lijn_choices
    }

    freezeReactiveValue(input, "mnd_zichtbare_lijnen")
    updateSelectizeInput(
      session,
      "mnd_zichtbare_lijnen",
      choices = lijn_choices,
      selected = selected_lijnen,
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  output$plot_zorg_maandelijks <- plotly::renderPlotly({
    if (input$mnd_grafiek == "Lijngrafiek") {
      df <- lijn_data_maandelijks()
      selected_domains <- input$mnd_domein
      selected_domain <- if (!is.null(selected_domains) && length(selected_domains) > 0) selected_domains[1] else ""

      input_selection <- input$mnd_zichtbare_lijnen
      available_lijnen <- unique(df$lijn)

      # Determine effective selection robustly
      if (is.null(input_selection) || length(input_selection) == 0) {
        selected_lijnen <- available_lijnen
      } else {
        # Check intersection with available lines to handle stale inputs
        input_clean <- trimws(as.character(input_selection))
        valid_selection <- intersect(input_clean, available_lijnen)

        if (length(valid_selection) > 0) {
          selected_lijnen <- valid_selection
        } else {
          # If input selects nothing valid (stale), fallback to showing all
          selected_lijnen <- available_lijnen
        }
      }

      log_msg(sprintf("[renderPlotly] Input: %s. Available: %s. Effective: %s",
              paste(input_selection, collapse=","),
              paste(available_lijnen, collapse=","),
              paste(selected_lijnen, collapse=",")))

      df <- df %>% filter(lijn %in% selected_lijnen)

      if (nrow(df) == 0) {
        log_msg("[renderPlotly] Empty DF after line filter")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen lijn-data beschikbaar voor de gekozen filters."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      p <- ggplot(df, aes(x = t_numeric, y = value, color = lijn, group = lijn)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(
          title = paste("Zorg over Tijd (Lijn):", selected_domain),
          subtitle = paste0(
            "Maatstaf: ", get_maatstaf_label(input$mnd_maatstaf),
            " | Bin size: ", get_bin_size_label(input$mnd_bin_size),
            " | Jaar: ", input$mnd_jaar,
            " | Populatie: ", input$mnd_pop,
            " | Modus: ", input$mnd_lijnmodus,
            " | Status: ", input$mnd_vgl
          ),
          x = get_time_axis_label(input$mnd_bin_size),
          y = if (input$mnd_maatstaf == "prevalentie_per_100") "Prevalentie per 100" else "Waarde",
          color = "Lijn"
        )
    } else {
      df <- data_maandelijks()
      selected_domains <- input$mnd_domein

      # Check if data is empty and show message
      if (nrow(df) == 0) {
        log_msg("[renderPlotly] No monthly data for this domain")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen maandelijkse data beschikbaar voor deze domein.\nControleer of de domein maandelijke metingen bevat."), size = 4) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      if (length(selected_domains) > 1) {
        df <- df %>%
          mutate(stack_group = selected_domein)

        p <- ggplot(df, aes(x = factor(t_numeric, levels = sort(unique(t_numeric))), y = value, fill = stack_group)) +
          geom_col(position = "stack") +
          theme_minimal() +
          labs(
            title = paste("Zorg over Tijd:", paste(selected_domains, collapse = ", ")),
            subtitle = paste0(
              "Maatstaf: ", get_maatstaf_label(input$mnd_maatstaf),
              " | Bin size: ", get_bin_size_label(input$mnd_bin_size),
              " | Jaar: ", input$mnd_jaar,
              " | Populatie: ", input$mnd_pop,
              " | Status: ", input$mnd_vgl
            ),
            x = get_time_axis_label(input$mnd_bin_size),
            y = if (input$mnd_maatstaf == "prevalentie_per_100") "Prevalentie per 100" else "Waarde",
            fill = "Groep"
          )
      } else {
        p <- ggplot(df, aes(x = factor(t_numeric, levels = sort(unique(t_numeric))), y = value, fill = died)) +
          geom_col(position = position_dodge()) +
          theme_minimal() +
          labs(
            title = paste("Zorg over Tijd:", selected_domains[1]),
            subtitle = paste0(
              "Maatstaf: ", get_maatstaf_label(input$mnd_maatstaf),
              " | Bin size: ", get_bin_size_label(input$mnd_bin_size),
              " | Jaar: ", input$mnd_jaar,
              " | Populatie: ", input$mnd_pop,
              " | Status: ", input$mnd_vgl
            ),
            x = get_time_axis_label(input$mnd_bin_size),
            y = if (input$mnd_maatstaf == "prevalentie_per_100") "Prevalentie per 100" else "Waarde"
          )
      }
      if (input$mnd_jaar == "Beide") {
        p <- p + facet_wrap(~cohort, nrow = 1)
      }
    }

    plotly::ggplotly(p)
  })

  data_maandelijks_export <- reactive({
    if (identical(input$mnd_grafiek, "Lijngrafiek")) {
      df <- lijn_data_maandelijks()
      if (nrow(df) == 0) return(df)
      selected_lijnen <- input$mnd_zichtbare_lijnen
      available_lijnen <- unique(df$lijn)
      if (is.null(selected_lijnen) || length(selected_lijnen) == 0) {
        selected_lijnen <- available_lijnen
      } else {
        valid_selection <- intersect(trimws(as.character(selected_lijnen)), available_lijnen)
        selected_lijnen <- if (length(valid_selection) > 0) valid_selection else available_lijnen
      }
      df |>
        dplyr::filter(lijn %in% selected_lijnen) |>
        dplyr::transmute(
          category = as.character(t_numeric),
          series = as.character(lijn),
          export_value = value
        )
    } else {
      df <- data_maandelijks()
      if (nrow(df) == 0) return(df)
      selected_domains <- input$mnd_domein
      if (length(selected_domains) > 1) {
        df |>
          dplyr::transmute(
            category = as.character(t_numeric),
            series = as.character(selected_domein),
            export_value = value
          )
      } else {
        df |>
          dplyr::transmute(
            category = as.character(t_numeric),
            series = as.character(died),
            export_value = value
          )
      }
    }
  })

  iter1_tijd_chart_type <- reactive({
    if (identical(input$mnd_grafiek, "Lijngrafiek")) "line" else "stacked_bar"
  })

  chart_data_downloads_server(
    id = "iter1_tijd_dl",
    data = data_maandelijks_export,
    chart_type = iter1_tijd_chart_type,
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "zorg_over_tijd",
    agg_fun = NULL
  )
  
  # ==========================================
  # SERVER LOGIC: TAB 4 - Kosten Boxplot
  # ==========================================
  selected_data <- reactive({
    log_msg(sprintf("[reactive] Filtering for cost_var: %s", input$cost_var))
    data <- all_data()
    if (nrow(data) == 0) {
      log_msg("[reactive] selected_data: parent data is empty")
      return(tibble::tibble())
    }
    if (!is.character(input$cost_var) || input$cost_var == "") {
      log_msg("[reactive] selected_data: invalid input$cost_var")
      return(tibble::tibble())
    }

    if (!is.null(input$cost_bin_size)) {
      data <- data %>% filter(bin_size == input$cost_bin_size)
    }
    if (nrow(data) == 0) {
      log_msg(sprintf("[reactive] selected_data: no rows after bin_size filter (%s)", input$cost_bin_size))
      return(tibble::tibble())
    }

    if (!is.null(input$cost_pop)) {
      data <- data %>% filter(doodsoorzaak == input$cost_pop)
    }
    if (nrow(data) == 0) {
      log_msg(sprintf("[reactive] selected_data: no rows after doodsoorzaak filter (%s)", input$cost_pop))
      return(tibble::tibble())
    }

    result <- data %>% filter(name == input$cost_var)
    log_msg(sprintf("[reactive] selected_data result: %d rows", nrow(result)))
    result
  })

  plot_cost_data <- reactive({
    log_msg("[reactive] Computing plot_cost_data...")
    tryCatch({
      data <- selected_data()
      if (nrow(data) == 0) {
        log_msg("[reactive] plot_cost_data: selected_data is empty")
        return(tibble::tibble())
      }
      
      df <- data %>%
        filter(type %in% c("q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon", "q75_per_persoon", "q95_per_persoon")) %>%
        group_by(cohort, died, t, type) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from = type,
          values_from = value,
          values_fn = mean,
          values_fill = NA_real_
        )

      df <- df %>%
        mutate(t = factor(as.numeric(t), levels = sort(unique(as.numeric(t))))) %>%
        filter(!is.na(mediaan_per_persoon))
      log_msg(sprintf("[reactive] plot_cost_data result: %d rows", nrow(df)))
      df
    }, error = function(e) {
      msg <- sprintf("[plot_cost_data] failed: %s", e$message)
      add_error(msg)
      tibble::tibble()
    })
  })

  output$plot_cost <- plotly::renderPlotly({
    log_msg("[render] Rendering plot_cost with plotly...")
    tryCatch({
      df <- plot_cost_data()
      if (nrow(df) == 0) {
        log_msg("[render] plot_cost: no data available")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen kosten-data beschikbaar."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }

      log_msg(sprintf("[render] plot_cost: rendering %d rows", nrow(df)))
      p <- ggplot(df, aes(x = factor(t), group = died, color = died, fill = died)) +
        geom_errorbar(aes(ymin = q05_per_persoon, ymax = q95_per_persoon),
                      position = position_dodge(width = 0.8), width = 0.2) +
        geom_crossbar(aes(y = mediaan_per_persoon, ymin = q25_per_persoon, ymax = q75_per_persoon),
                      position = position_dodge(width = 0.8), width = 0.35, alpha = 0.35) +
        geom_point(aes(y = mediaan_per_persoon), position = position_dodge(width = 0.8), size = 2) +
        facet_wrap(~cohort, nrow = 1) +
        labs(
          title = paste("Kostenboxplot voor", input$cost_var),
          x = "t", y = "Kosten (per persoon)",
          color = "Status", fill = "Status"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      plotly::ggplotly(p)
    }, error = function(e) {
      msg <- sprintf("[plot_cost render] %s", e$message)
      add_error(msg)
      p <- ggplot() +
        geom_text(aes(0, 0, label = "Plot failed to render."), size = 5) +
        xlab(NULL) + ylab(NULL) + theme_void()
      plotly::ggplotly(p)
    })
  })

  plot_cost_export <- reactive({
    df <- plot_cost_data()
    if (nrow(df) == 0) return(df)
    df |> dplyr::mutate(series = combine_series(died, cohort))
  })

  chart_data_downloads_server(
    id = "iter1_cost_dl",
    data = plot_cost_export,
    chart_type = "scatter",
    category_col = "t",
    series_col = "series",
    value_col = "mediaan_per_persoon",
    filename_prefix = "kosten_boxplot",
    agg_fun = NULL
  )

  # ==========================================
  # SERVER LOGIC: TAB 5 - Zorg per Domein Butterfly
  # ==========================================
  data_butterfly <- reactive({
    log_msg("[reactive] Computing butterfly data...")
    tryCatch({
      data <- all_data()
      if (nrow(data) == 0) {
        log_msg("[reactive] butterfly data: all_data is empty")
        return(tibble::tibble())
      }

      # Filter for 1000 days, selected domain and measure using process_measurements
      df <- process_measurements(data, input$butterfly_maatstaf) %>%
        filter(bin_size == "1000days")

      # For prevalentie_per_100, we need to find the gebruik_/heeft_ variant of the domain
      if (input$butterfly_maatstaf == "prevalentie_per_100") {
        target_name <- find_gebruikt_name(input$butterfly_domein, df)
        if (is.na(target_name)) {
          # If no gebruik_/heeft_ variant found, return empty
          return(tibble::tibble())
        }
        df <- df %>% filter(name == target_name)
      } else {
        # For other measurements, use the selected domain directly
        df <- df %>% filter(name == input$butterfly_domein)
      }

      if (nrow(df) == 0) {
        log_msg("[reactive] butterfly: no data for selected filters")
        return(tibble::tibble())
      }

      # Parse comparison choice and create left/right groups
      if (input$butterfly_vgl == "obs_2023_vs_ctrl_2023") {
        # Left: Observed 2023, Right: Control 2023
        left_filter <- df %>% filter(cohort == "2023", died == "Overleden")
        right_filter <- df %>% filter(cohort == "2023", died == "In leven")
        left_label <- "Observed 2023"
        right_label <- "Control 2023"
      } else if (input$butterfly_vgl == "obs_2019_vs_obs_2023") {
        # Left: Observed 2019, Right: Observed 2023
        left_filter <- df %>% filter(cohort == "2019", died == "Overleden")
        right_filter <- df %>% filter(cohort == "2023", died == "Overleden")
        left_label <- "Observed 2019"
        right_label <- "Observed 2023"
      } else if (input$butterfly_vgl == "obs_2019_vs_ctrl_2019") {
        # Left: Observed 2019, Right: Control 2019
        left_filter <- df %>% filter(cohort == "2019", died == "Overleden")
        right_filter <- df %>% filter(cohort == "2019", died == "In leven")
        left_label <- "Observed 2019"
        right_label <- "Control 2019"
      }

      # Aggregate by doodsoorzaak
      left_agg <- left_filter %>%
        group_by(doodsoorzaak) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(group = left_label, value_butterfly = -value)

      right_agg <- right_filter %>%
        group_by(doodsoorzaak) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(group = right_label, value_butterfly = value)

      result <- bind_rows(left_agg, right_agg) %>%
        arrange(doodsoorzaak)

      # Multiply by 100 for prevalentie_per_100 to convert from decimal to percentage
      if (input$butterfly_maatstaf == "prevalentie_per_100") {
        result <- result %>% mutate(value_butterfly = value_butterfly * 100)
      }

      log_msg(sprintf("[reactive] butterfly data computed: %d rows", nrow(result)))
      result
    }, error = function(e) {
      msg <- sprintf("[butterfly data] failed: %s", e$message)
      add_error(msg)
      tibble::tibble()
    })
  })
  
  output$plot_butterfly <- plotly::renderPlotly({
    log_msg("[render] Rendering butterfly chart...")
    tryCatch({
      df <- data_butterfly()
      if (nrow(df) == 0) {
        log_msg("[render] butterfly: no data available")
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Geen data beschikbaar voor butterfly chart."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }
      
      # Pivot to get left and right values side by side
      pivot_df <- df %>%
        pivot_wider(
          names_from = group,
          values_from = value_butterfly,
          values_fill = 0
        ) %>%
        mutate(doodsoorzaak = factor(doodsoorzaak, levels = sort(unique(doodsoorzaak))))
      
      # Get group names dynamically
      group_cols <- setdiff(colnames(pivot_df), c("doodsoorzaak", "value"))
      
      log_msg(sprintf("[render] butterfly: rendering %d rows with groups: %s", nrow(pivot_df), paste(group_cols, collapse=", ")))
      
      if (length(group_cols) < 2) {
        p <- ggplot() +
          geom_text(aes(0, 0, label = "Onvoldoende data voor vergelijking."), size = 5) +
          xlab(NULL) + ylab(NULL) + theme_void()
        return(plotly::ggplotly(p))
      }
      
      left_col <- group_cols[1]
      right_col <- group_cols[2]
      
      # Create butterfly chart with both sides
      p <- ggplot(pivot_df) +
        geom_col(aes(x = !!sym(left_col), y = doodsoorzaak, fill = left_col), 
                 position = "identity", alpha = 0.8) +
        geom_col(aes(x = !!sym(right_col), y = doodsoorzaak, fill = right_col), 
                 position = "identity", alpha = 0.8) +
        geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
        scale_x_continuous(labels = function(x) abs(x)) +
        labs(
          title = paste("Zorg per Domein:", input$butterfly_domein),
          subtitle = paste("Maatstaf:", input$butterfly_maatstaf),
          x = "Waarde (absolute schaal)", y = "Populatie / Doodsoorzaak",
          fill = "Groep"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.y = element_text(size = 10),
          plot.title = element_text(face = "bold")
        )
      
      plotly::ggplotly(p)
    }, error = function(e) {
      msg <- sprintf("[butterfly render] %s", e$message)
      add_error(msg)
      p <- ggplot() +
        geom_text(aes(0, 0, label = "Butterfly chart render error."), size = 5) +
        xlab(NULL) + ylab(NULL) + theme_void()
      plotly::ggplotly(p)
    })
  })

  data_butterfly_export <- reactive({
    df <- data_butterfly()
    if (nrow(df) == 0) return(df)
    df |>
      dplyr::transmute(
        category = as.character(doodsoorzaak),
        series = as.character(group),
        export_value = abs(value_butterfly),
        value_butterfly = value_butterfly
      )
  })

  chart_data_downloads_server(
    id = "iter1_bfly_dl",
    data = data_butterfly_export,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "zorg_butterfly",
    agg_fun = NULL
  )

  # ==========================================
  # SERVER LOGIC: TAB 6 - Interventies
  # ==========================================
  data_interventies <- reactive({
    req(nrow(all_data()) > 0)
    selected_interventies <- input$int_interventie

    if (is.null(selected_interventies) || length(selected_interventies) == 0) {
      return(tibble::tibble())
    }

    # Use process_measurements to handle the maatstaf filtering
    df <- process_measurements(all_data(), input$int_maatstaf) %>%
      filter(bin_size == input$int_bin_size,
             doodsoorzaak == "all")

    # Filter by all selected variable names
    if (input$int_maatstaf == "prevalentie_per_100") {
      # For prevalentie, find the "gebruik_" or "heeft_" variants of selected names
      matching_names <- c()
      for (selected_name in selected_interventies) {
        variant <- find_gebruikt_name(selected_name, df)
        if (!is.na(variant)) {
          matching_names <- c(matching_names, variant)
        }
      }
      if (length(matching_names) == 0) {
        # No prevalentie data for selected variables
        return(tibble::tibble())
      }
      df <- df %>% filter(name %in% matching_names)
    } else {
      # For other measurements, filter by all selected names
      df <- df %>%
        filter(name %in% selected_interventies,
               !startsWith(name, "gebruik") & !startsWith(name, "heeft"))
    }

    if (nrow(df) == 0) {
      log_msg(sprintf("[data_interventies] No data for selected interventies, maatstaf=%s",
                      input$int_maatstaf))
      return(tibble::tibble())
    }

    if (input$int_jaar != "Beide") {
      df <- df %>% filter(cohort == as.numeric(input$int_jaar))
    }

    # Apply comparison filter
    if (input$int_vgl == "Geen vergelijking") {
      df <- df %>% filter(died == "Overleden")
    } else if (input$int_vgl == "Geobserveerd 2019 vs. Geobserveerd 2023") {
      df <- df %>% filter(died == "Overleden")
    }
    # else: "Geobserveerd vs. Controle" - keep both

    result <- df %>%
      mutate(waarde = value)

    # Multiply by 100 for prevalentie_per_100 to convert from decimal to percentage
    if (input$int_maatstaf == "prevalentie_per_100") {
      result <- result %>% mutate(waarde = waarde * 100)
    }

    result
  })

  output$plot_interventies <- plotly::renderPlotly({
    df <- data_interventies()
    if (nrow(df) == 0) {
      p <- ggplot() +
        geom_text(aes(0, 0, label = "Geen data beschikbaar voor deze interventie.\nControleer of de interventie data bevat voor de gekozen filters."), size = 4) +
        xlab(NULL) + ylab(NULL) + theme_void()
      return(plotly::ggplotly(p))
    }

    p <- ggplot(df, aes(x = name, y = waarde, fill = died)) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Curatieve Interventies",
        subtitle = paste0(
          "Maatstaf: ", get_maatstaf_label(input$int_maatstaf),
          " | Bin size: ", get_bin_size_label(input$int_bin_size),
          " | Jaar: ", input$int_jaar,
          " | Vergelijking: ", input$int_vgl
        ),
        x = "Interventie",
        y = if (input$int_maatstaf == "prevalentie_per_100") "Prevalentie per 100" else "Waarde",
        fill = "Status"
      )

    if (input$int_jaar == "Beide") {
      p <- p + facet_wrap(~cohort, nrow = 1)
    }

    plotly::ggplotly(p)
  })

  data_interventies_export <- reactive({
    df <- data_interventies()
    if (nrow(df) == 0) return(df)
    if (identical(input$int_jaar, "Beide")) {
      df <- df |> dplyr::mutate(series = combine_series(died, cohort))
    } else {
      df <- df |> dplyr::mutate(series = as.character(died))
    }
    df
  })

  chart_data_downloads_server(
    id = "iter1_int_dl",
    data = data_interventies_export,
    chart_type = "grouped_bar",
    category_col = "name",
    series_col = "series",
    value_col = "waarde",
    filename_prefix = "interventies",
    agg_fun = NULL
  )

  # ==========================================
  # SERVER LOGIC: TAB 7 - Logs
  # ==========================================
  output$app_log <- renderText({
    errors <- error_log()
    if (length(errors) > 0) paste("=== ERROR LOG ===\n", paste(errors, collapse = "\n"))
    else "=== NO ERRORS ===\nApp is running normally."
  })

  # ==========================================
  # SERVER LOGIC: ITERATIE 2
  # ==========================================
  active_agg_sheet <- reactive({
    req(input$agg_sheet)
    input$agg_sheet
  })

  agg_raw <- reactive({
    get_sheet(active_agg_sheet())
  })

  selected_agg_names <- reactive({
    df <- agg_raw()
    req(nrow(df) > 0)
    names_choices <- sort(unique(as.character(df$name)))
    split_col <- input$agg_split %||% "none"
    if (!identical(split_col, "none")) {
      selected <- intersect(input$agg_name_single %||% character(0), names_choices)
      if (length(selected) == 0) selected <- intersect(input$agg_name_multi %||% character(0), names_choices)
      if (length(selected) == 0) selected <- names_choices[[1]]
      return(selected[[1]])
    }

    if (identical(input$agg_name_mode %||% "single", "multi")) {
      selected <- intersect(input$agg_name_multi %||% character(0), names_choices)
    } else {
      selected <- intersect(input$agg_name_single %||% character(0), names_choices)
    }
    if (length(selected) == 0) selected <- names_choices[[1]]
    selected
  })

  agg_stat_choices <- reactive({
    df <- agg_raw()
    names_selected <- selected_agg_names()
    choices <- lapply(names_selected, function(x) stat_choices_for(df, x))
    choices <- Reduce(intersect, choices)
    if (length(choices) == 0) stat_choices_for(df, names_selected[[1]]) else choices
  })

  agg_corrected_basic_available <- function() {
    corrected <- corrected_sheet_for(active_agg_sheet())
    if (is.na(corrected)) return(FALSE)
    names_selected <- selected_agg_names()
    if (length(names_selected) == 0) return(FALSE)
    stat <- input$agg_stat
    stat_choices <- agg_stat_choices()
    if (is.null(stat) || !stat %in% stat_choices) stat <- stat_choices[[1]]
    all(vapply(names_selected, is_cost_outcome, logical(1))) && is_cost_stat(stat)
  }

  agg_corrected_available <- reactive({
    if (!agg_corrected_basic_available()) return(FALSE)
    if (!identical(effective_agg_split(), "none")) return(FALSE)
    TRUE
  })

  agg_cohort_values <- reactive({
    df <- agg_raw()
    req(nrow(df) > 0)
    if (!"cohort" %in% names(df)) return(character(0))
    values <- ordered_values(df$cohort)
    corrected <- corrected_sheet_for(active_agg_sheet())
    if (isTRUE(input$agg_corrected) && isTRUE(agg_corrected_basic_available()) && !is.na(corrected)) {
      corrected_df <- get_sheet(corrected)
      if ("cohort" %in% names(corrected_df)) {
        corrected_df <- corrected_df |> dplyr::filter(name %in% selected_agg_names())
        values <- ordered_values(c(values, corrected_df$cohort))
      }
    }
    values
  })

  selected_agg_cohort <- reactive({
    values <- agg_cohort_values()
    if (length(values) == 0) return(NULL)
    selected <- input$agg_cohort
    if (is.null(selected) || length(selected) == 0 || is.na(selected) || !selected %in% values) {
      selected <- if ("2023" %in% values) "2023" else values[[1]]
    }
    selected
  })

  effective_agg_split <- function() {
    split_col <- input$agg_split %||% "none"
    if (identical(split_col, "none")) return("none")
    df <- agg_raw()
    if (!split_col %in% allowed_split_columns(active_agg_sheet(), names(df))) return("none")
    if ("cohort" %in% names(df)) {
      cohort <- selected_agg_cohort()
      if (!is.null(cohort) && !cohort %in% ordered_values(df$cohort)) return("none")
    }
    split_col
  }

  observeEvent(input$agg_corrected, {
    if (isTRUE(input$agg_corrected)) {
      values <- agg_cohort_values()
      if ("2023" %in% values) updateRadioButtons(session, "agg_cohort", selected = "2023")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$agg_sheet, {
    values <- agg_cohort_values()
    if ("2023" %in% values) updateRadioButtons(session, "agg_cohort", selected = "2023")
  }, ignoreInit = TRUE)

  output$agg_name_ui <- renderUI({
    df <- agg_raw()
    req(nrow(df) > 0)
    names_choices <- sort(unique(as.character(df$name)))
    split_col <- input$agg_split %||% "none"
    single_selected <- intersect(isolate(input$agg_name_single) %||% character(0), names_choices)
    multi_selected <- intersect(isolate(input$agg_name_multi) %||% character(0), names_choices)
    if (length(single_selected) == 0) {
      single_selected <- intersect(multi_selected, names_choices)
      if (length(single_selected) == 0) single_selected <- names_choices[[1]]
    }
    if (length(multi_selected) == 0) multi_selected <- single_selected

    choices <- choice_names(names_choices, function(x) pretty_metric_name(x, active_agg_sheet()))
    if (!identical(split_col, "none")) {
      return(selectInput("agg_name_single", "Uitkomst", choices = choices, selected = single_selected[[1]]))
    }

    mode <- input$agg_name_mode %||% "single"
    tagList(
      radioButtons(
        "agg_name_mode",
        "Keuze uitkomst",
        choices = c("Een uitkomst" = "single", "Meerdere uitkomsten" = "multi"),
        selected = if (mode %in% c("single", "multi")) mode else "single"
      ),
      if (identical(mode, "multi")) {
        tagList(
          div(
            style = "display: flex; gap: 8px; margin-bottom: 8px;",
            actionButton("agg_select_all", "Alles selecteren"),
            actionButton("agg_select_none", "Alles wissen")
          ),
          checkboxGroupInput("agg_name_multi", "Uitkomst", choices = choices, selected = multi_selected)
        )
      } else {
        selectInput("agg_name_single", "Uitkomst", choices = choices, selected = single_selected[[1]])
      }
    )
  })

  observeEvent(input$agg_select_all, {
    df <- agg_raw()
    req(nrow(df) > 0)
    updateCheckboxGroupInput(session, "agg_name_multi", selected = sort(unique(as.character(df$name))))
  })

  observeEvent(input$agg_select_none, {
    updateCheckboxGroupInput(session, "agg_name_multi", selected = character(0))
  })

  output$agg_corrected_ui <- renderUI({
    if (!isTRUE(agg_corrected_available())) return(NULL)
    checkboxInput("agg_corrected", "Inflatiecorrectie tonen", value = FALSE)
  })

  output$agg_stat_ui <- renderUI({
    df <- agg_raw()
    req(nrow(df) > 0, length(selected_agg_names()) > 0)
    choices <- agg_stat_choices()
    if (length(choices) <= 1) return(NULL)
    selected <- isolate(input$agg_stat)
    if (is.null(selected) || !selected %in% choices) selected <- choices[[1]]

    radioButtons("agg_stat", "Maat", choices = choice_names(choices, pretty_stat), selected = selected)
  })

  output$agg_view_ui <- renderUI({
    df <- agg_raw() |>
      dplyr::filter(name %in% selected_agg_names())
    req(nrow(df) > 0)
    choices <- view_choices_for(df)
    if (length(choices) <= 1) return(NULL)
    selected <- isolate(input$agg_view)
    if (is.null(selected) || !selected %in% choices) selected <- choices[[1]]
    radioButtons("agg_view", "Tijdvenster", choices = choices, selected = selected)
  })

  output$agg_cohort_ui <- renderUI({
    values <- agg_cohort_values()
    if (length(values) <= 1) return(NULL)
    selected <- selected_agg_cohort()
    radioButtons("agg_cohort", "Cohort", choices = values, selected = selected)
  })

  output$agg_died_ui <- renderUI({
    df <- agg_raw()
    req(nrow(df) > 0)
    if (!"died" %in% names(df)) return(NULL)
    values <- ordered_values(df$died)
    if (length(values) <= 1) return(NULL)
    checkboxGroupInput("agg_died", "Populatie", choices = choice_names(values, population_label), selected = values)
  })

  available_split_cols <- reactive({
    df <- agg_raw()
    req(nrow(df) > 0)
    if ("cohort" %in% names(df)) {
      cohort <- selected_agg_cohort()
      if (!is.null(cohort) && !cohort %in% ordered_values(df$cohort)) return(character(0))
    }
    dims <- allowed_split_columns(active_agg_sheet(), names(df))
    dims[vapply(dims, function(col) length(setdiff(ordered_values(df[[col]]), "all")) > 0, logical(1))]
  })

  output$agg_split_ui <- renderUI({
    dims <- available_split_cols()
    if (length(dims) == 0) return(NULL)
    choices <- c("none", dims)
    selectInput(
      "agg_split",
      "Uitsplitsing",
      choices = choice_names(choices, function(x) if (x == "none") "Totaal" else pretty_split_name(x)),
      selected = {
        current <- isolate(input$agg_split)
        if (is.null(current) || !current %in% choices) "none" else current
      }
    )
  })

  output$agg_split_values_ui <- renderUI({
    df <- agg_raw()
    req(nrow(df) > 0, input$agg_split)
    split_col <- effective_agg_split()
    if (split_col == "none" || !split_col %in% names(df)) return(NULL)
    values <- ordered_split_values(split_col, df[[split_col]])
    if (length(values) <= 1) return(NULL)
    selected <- isolate(input$agg_split_values)
    if (is.null(selected) || length(intersect(selected, values)) == 0) {
      selected <- values
    } else {
      selected <- intersect(selected, values)
    }
    checkboxGroupInput(
      "agg_split_values",
      pretty_split_name(split_col),
      choices = choice_names(values, function(x) pretty_value(split_col, x)),
      selected = selected
    )
  })

  build_filtered_agg_base <- function(sheet, versie) {
    df <- get_sheet(sheet)
    req(nrow(df) > 0, length(selected_agg_names()) > 0)
    df <- df |> dplyr::filter(name %in% selected_agg_names())
    view <- input$agg_view
    choices <- view_choices_for(df)
    if (is.null(view) || !view %in% choices) view <- choices[[1]]

    df <- df |>
      dplyr::mutate(
        value_num_raw = numericize(value),
        n_totaal_num = numericize(n_totaal),
        versie = versie
      )

    if ("bin_size" %in% names(df)) {
      if (view == "maandelijks") {
        df <- df |> dplyr::filter(as.character(bin_size) == "30")
      } else if (view == "laatste_30") {
        # t == -1 is the last month only for 30-day bins; for 1000-day bins it is the full-period total.
        df <- df |> dplyr::filter(as.character(bin_size) == "30", as.character(t) == "-1")
      } else if (view == "laatste_1000") {
        df <- df |> dplyr::filter(as.character(bin_size) == "1000")
      }
    }
    if (nrow(df) == 0) return(df)
    if ("cohort" %in% names(df)) {
      cohort_values <- ordered_values(df$cohort)
      selected_cohort <- selected_agg_cohort()
      if (length(cohort_values) == 0) return(df[0, , drop = FALSE])
      if (is.null(selected_cohort) || length(selected_cohort) == 0 || is.na(selected_cohort) || !selected_cohort %in% cohort_values) return(df[0, , drop = FALSE])
      df <- df |> dplyr::filter(as.character(cohort) == selected_cohort)
    }
    if (nrow(df) == 0) return(df)
    if ("died" %in% names(df) && !is.null(input$agg_died) && length(input$agg_died) > 0) {
      died_values <- ordered_values(df$died)
      selected_died <- intersect(input$agg_died, died_values)
      if (length(selected_died) == 0) selected_died <- died_values
      df <- df |> dplyr::filter(as.character(died) %in% selected_died)
    }
    if (nrow(df) == 0) return(df)

    dims <- intersect(demographic_cols_iteration2, names(df))
    split_col <- effective_agg_split()
    for (col in dims) {
      if (identical(col, split_col)) {
        selected <- input$agg_split_values
        if (is.null(selected) || length(selected) == 0) {
          selected <- ordered_split_values(col, df[[col]])
        }
        if (length(selected) > 0) {
          df <- df |> dplyr::filter(as.character(.data[[col]]) %in% selected)
        }
      } else if ("all" %in% as.character(df[[col]])) {
        df <- df |> dplyr::filter(as.character(.data[[col]]) == "all")
      }
    }

    df
  }

  filtered_agg_base <- reactive({
    base <- build_filtered_agg_base(active_agg_sheet(), "Geobserveerd")
    corrected <- corrected_sheet_for(active_agg_sheet())
    if (!is.na(corrected) && isTRUE(input$agg_corrected) && isTRUE(agg_corrected_available())) {
      names_selected <- selected_agg_names()
      stat <- input$agg_stat
      stat_choices <- agg_stat_choices()
      if (is.null(stat) || !stat %in% stat_choices) stat <- stat_choices[[1]]
      if (all(vapply(names_selected, is_cost_outcome, logical(1))) && is_cost_stat(stat)) {
        base <- dplyr::bind_rows(base, build_filtered_agg_base(corrected, "Inflatiecorrectie"))
      }
    }
    base
  })

  filtered_agg <- reactive({
    df <- filtered_agg_base()
    req(nrow(df) > 0, length(selected_agg_names()) > 0)
    stat <- input$agg_stat
    stat_choices <- agg_stat_choices()
    if (is.null(stat) || !stat %in% stat_choices) stat <- stat_choices[[1]]

    id_cols <- setdiff(names(df), c("variable", "type", "value", "value_num_raw"))
    df_wide <- df |>
      dplyr::select(dplyr::all_of(id_cols), type, value_num_raw) |>
      tidyr::pivot_wider(
        names_from = type,
        values_from = value_num_raw,
        values_fn = list(value_num_raw = ~ dplyr::first(.x))
      )

    for (col in c("sum_totaal_groep", "n_totaal_gebruikers", "gemiddelde_per_persoon")) {
      if (!col %in% names(df_wide)) df_wide[[col]] <- NA_real_
    }

    df_wide |>
      dplyr::mutate(
        value_num = dplyr::case_when(
          stat == "sum_totaal_groep" ~ .data[["sum_totaal_groep"]],
          stat == "n_totaal_gebruikers" ~ .data[["n_totaal_gebruikers"]],
          stat == "aandeel_gebruikers_berekend" ~ .data[["n_totaal_gebruikers"]] / n_totaal_num,
          stat == "gemiddelde_per_gebruiker_berekend" ~ .data[["sum_totaal_groep"]] / .data[["n_totaal_gebruikers"]],
          stat == "gemiddelde_per_persoon_berekend" ~ .data[["sum_totaal_groep"]] / n_totaal_num,
          stat == "gemiddelde_per_persoon" ~ .data[["gemiddelde_per_persoon"]],
          TRUE ~ NA_real_
        ),
        maat = pretty_stat(stat)
      )
  })

  agg_plot_obj <- reactive({
    df <- filtered_agg()
    req(nrow(df) > 0)

    view <- input$agg_view
    choices <- view_choices_for(agg_raw() |> dplyr::filter(name %in% selected_agg_names()))
    if (is.null(view) || !view %in% choices) view <- choices[[1]]

    split_col <- effective_agg_split()
    has_split <- split_col != "none" && split_col %in% names(df)

    df_plot <- df |>
      dplyr::filter(!is.na(value_num)) |>
      dplyr::mutate(
        versie = dplyr::coalesce(as.character(versie), "Geobserveerd"),
        outcome_label = pretty_metric_name(name, active_agg_sheet()),
        split_value = if (has_split) as.character(.data[[split_col]]) else "Totaal",
        split_label = if (has_split) pretty_value(split_col, split_value) else "Totaal",
        population_value = dplyr::coalesce(if ("died" %in% names(df)) population_label(died) else "Totaal", "Totaal"),
        cohort_value = if ("cohort" %in% names(df)) as.character(cohort) else "",
        tooltip = paste0(
          "Dataset: ", pretty_sheet(active_agg_sheet()), "<br>",
          "Versie: ", versie, "<br>",
          "Uitkomst: ", pretty_metric_name(name, active_agg_sheet()), "<br>",
          "Maat: ", maat, "<br>",
          if (has_split) paste0(pretty_split_name(split_col), ": ", split_label, "<br>") else "",
          if ("died" %in% names(df)) paste0("Populatie: ", population_value, "<br>") else "",
          if ("cohort" %in% names(df)) paste0("Cohort: ", cohort_value, "<br>") else "",
          "Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = "."), "<br>",
          "Aantal personen: ", scales::comma(n_totaal_num, big.mark = ",", decimal.mark = ".")
        )
      )

    req(nrow(df_plot) > 0)
    df_plot <- df_plot |>
      dplyr::filter(!is.na(population_value), !is.na(versie))
    req(nrow(df_plot) > 0)
    y_label <- dplyr::first(df_plot$maat)
    multiple_outcomes <- dplyr::n_distinct(df_plot$name) > 1
    multiple_versions <- dplyr::n_distinct(df_plot$versie) > 1
    version_labels_bar <- c("Geobserveerd" = "Niet gecorrigeerd", "Inflatiecorrectie" = "Inflatiecorrectie")
    version_labels_line <- c("Geobserveerd" = "Niet gecorrigeerd", "Inflatiecorrectie" = "Inflatiecorrectie (gestreept)")

    if (view == "maandelijks" && "t" %in% names(df_plot)) {
      df_plot <- df_plot |>
        dplyr::mutate(
          x_value = numericize(t),
          versie_label = dplyr::recode(versie, !!!version_labels_line, .default = versie),
          line_base = dplyr::case_when(
            has_split && multiple_outcomes ~ paste(outcome_label, split_label, population_value, sep = " | "),
            has_split ~ split_label,
            multiple_outcomes ~ paste(outcome_label, population_value, sep = " | "),
            TRUE ~ population_value
          ),
          line_value = if (multiple_versions) paste(line_base, versie_label, sep = ", ") else line_base,
          line_group = paste(name, split_value, population_value, cohort_value, versie, sep = " | ")
        ) |>
        dplyr::arrange(line_group, x_value)

      if (multiple_versions) {
        base_order <- c("Overleden", "Controle", sort(setdiff(unique(df_plot$line_base), c("Overleden", "Controle"))))
        base_order <- base_order[base_order %in% unique(df_plot$line_base)]
        line_levels <- as.vector(t(outer(base_order, unname(version_labels_line[c("Geobserveerd", "Inflatiecorrectie")]), paste, sep = ", ")))
        line_levels <- line_levels[line_levels %in% unique(df_plot$line_value)]
        df_plot <- df_plot |>
          dplyr::mutate(line_value = factor(line_value, levels = line_levels))
      }
      line_levels <- levels(df_plot$line_value) %||% unique(as.character(df_plot$line_value))
      line_bases <- stringr::str_remove(line_levels, ", (Niet gecorrigeerd|Inflatiecorrectie \\(gestreept\\))$")
      line_base_palette <- if (all(line_bases %in% names(population_palette))) {
        population_palette[line_bases]
      } else {
        base_levels <- unique(line_bases)
        stats::setNames(build_palette(length(base_levels)), base_levels)[line_bases]
      }
      line_color_values <- stats::setNames(line_base_palette, line_levels)
      line_type_values <- stats::setNames(ifelse(grepl("Inflatiecorrectie", line_levels), "dashed", "solid"), line_levels)

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = x_value, y = value_num, color = line_value, group = line_group, text = tooltip)
      )
      p <- p +
        if (multiple_versions) {
          ggplot2::geom_line(ggplot2::aes(linetype = line_value), linewidth = 0.8)
        } else {
          ggplot2::geom_line(linewidth = 0.8)
        }
      if (!multiple_versions) {
        p <- p + ggplot2::geom_point(size = 2)
      }
      p <- p +
        ggplot2::scale_color_manual(values = line_color_values, drop = TRUE) +
        ggplot2::labs(x = "Maand", y = NULL, color = NULL)
      if (multiple_versions) {
        p <- p +
          ggplot2::scale_linetype_manual(
            values = line_type_values,
            drop = TRUE,
            name = NULL
          )
      }
    } else if (has_split) {
      levels_order <- if (has_split) pretty_value(split_col, ordered_split_values(split_col, df_plot$split_value)) else NULL
      df_plot <- df_plot |>
        dplyr::group_by(name, outcome_label, split_value, split_label, population_value, versie) |>
        dplyr::summarise(value_num = sum(value_num, na.rm = TRUE), tooltip = dplyr::first(tooltip), .groups = "drop") |>
        dplyr::mutate(split_label = if (!is.null(levels_order)) factor(split_label, levels = levels_order) else stats::reorder(split_label, value_num))

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = split_label, y = value_num, fill = population_value, text = tooltip)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge2(width = 0.75, preserve = "single")) +
        ggplot2::scale_fill_manual(values = if (all(unique(df_plot$population_value) %in% names(population_palette))) population_palette[unique(df_plot$population_value)] else build_palette(length(unique(df_plot$population_value)))) +
        ggplot2::scale_x_discrete(labels = function(x) axis_label(x, 14)) +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
      if (multiple_outcomes || multiple_versions) {
        p <- p + ggplot2::facet_grid(
          rows = if (multiple_outcomes) ggplot2::vars(outcome_label) else ggplot2::vars(),
          cols = if (multiple_versions) ggplot2::vars(versie) else ggplot2::vars(),
          scales = "free_y"
        )
      }
    } else {
      df_plot <- df_plot |>
        dplyr::mutate(
          x_value = dplyr::case_when(
            multiple_outcomes && multiple_versions && "died" %in% names(df_plot) ~ paste(outcome_label, population_value, sep = "\n"),
            multiple_outcomes ~ outcome_label,
            multiple_versions && "died" %in% names(df_plot) ~ population_value,
            multiple_versions ~ versie,
            "died" %in% names(df_plot) ~ population_value,
            "cohort" %in% names(df_plot) ~ cohort_value,
            TRUE ~ "Totaal"
          ),
          fill_value = population_value,
          dodge_value = if (multiple_versions) versie else population_value
        ) |>
        dplyr::group_by(x_value, fill_value, dodge_value, population_value, versie) |>
        dplyr::summarise(value_num = sum(value_num, na.rm = TRUE), tooltip = dplyr::first(tooltip), .groups = "drop") |>
        dplyr::mutate(
          versie_label = dplyr::recode(versie, !!!version_labels_bar, .default = versie),
          legend_label = if (multiple_versions) paste(population_value, versie_label, sep = ", ") else population_value
        )

      x_levels <- unique(df_plot$x_value)
      fill_levels <- if (multiple_versions) {
        pop_order <- c("Overleden", "Controle", sort(setdiff(unique(df_plot$population_value), c("Overleden", "Controle"))))
        pop_order <- pop_order[pop_order %in% unique(df_plot$population_value)]
        version_order <- c("Niet gecorrigeerd", "Inflatiecorrectie")
        as.vector(t(outer(pop_order, version_order, paste, sep = ", ")))
      } else {
        unique(df_plot$legend_label)
      }
      fill_levels <- fill_levels[fill_levels %in% unique(df_plot$legend_label)]

      df_plot <- df_plot |>
        dplyr::mutate(
          x_value = factor(x_value, levels = x_levels),
          legend_label = factor(legend_label, levels = fill_levels)
        ) |>
        dplyr::group_by(x_value) |>
        dplyr::mutate(
          x_base = as.numeric(x_value),
          n_dodge = dplyr::n_distinct(dodge_value),
          dodge_rank = match(dodge_value, unique(dodge_value)),
          x_pos = x_base + dplyr::if_else(n_dodge > 1, (dodge_rank - (n_dodge + 1) / 2) * (0.75 / n_dodge), 0),
          bar_width = dplyr::if_else(n_dodge > 1, 0.7 / n_dodge, 0.7)
        ) |>
        dplyr::ungroup()

      bar_width_value <- min(df_plot$bar_width, na.rm = TRUE)

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = x_pos, y = value_num, fill = legend_label, group = dodge_value, text = tooltip)
      ) +
        ggplot2::geom_col(width = bar_width_value, color = "white", linewidth = 0.2)
      p <- p +
        ggplot2::scale_fill_manual(values = {
          fill_levels <- levels(df_plot$legend_label)
          fill_population <- stringr::str_remove(fill_levels, ", .*$")
          fill_is_corrected <- grepl("Inflatiecorrectie", fill_levels)
          fill_colors <- if (all(fill_population %in% names(population_palette))) {
            population_palette[fill_population]
          } else {
            stats::setNames(build_palette(length(fill_levels)), fill_levels)
          }
          fill_colors[fill_is_corrected] <- lighten_color(fill_colors[fill_is_corrected])
          if (all(fill_levels %in% names(population_palette))) {
            population_palette[fill_levels]
          } else {
            stats::setNames(fill_colors, fill_levels)
          }
        }) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(x_levels),
          labels = function(x) axis_label(x_levels[round(x)], 16)
        ) +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
    }

    show_plot_legend <- (view == "maandelijks" && "t" %in% names(df_plot)) || multiple_versions
    p <- p +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        legend.position = if (show_plot_legend) "bottom" else "none",
        panel.grid.minor = ggplot2::element_blank()
      ) +
      {if (view == "maandelijks" && "t" %in% names(df_plot)) ggplot2::guides(fill = "none", color = ggplot2::guide_legend(nrow = if (multiple_versions) 2 else 1, byrow = TRUE), linetype = if (multiple_versions) ggplot2::guide_legend(nrow = 2, byrow = TRUE) else "none") else ggplot2::guides(fill = if (multiple_versions) ggplot2::guide_legend(nrow = 2, byrow = TRUE) else "none", color = "none", alpha = "none")} +
      ggplot2::ggtitle(
        paste(
          pretty_sheet(active_agg_sheet()),
          "|",
          if (length(selected_agg_names()) <= 2) paste(pretty_metric_name(selected_agg_names(), active_agg_sheet()), collapse = ", ") else "Meerdere uitkomsten",
          "|",
          y_label
        )
      )

    p
  })

  output$plot_agg <- renderPlotly({
    df <- filtered_agg()
    view <- input$agg_view
    choices <- view_choices_for(agg_raw() |> dplyr::filter(name %in% selected_agg_names()))
    if (is.null(view) || !view %in% choices) view <- choices[[1]]
    custom_annotations <- build_agg_version_annotations(df, view)
    show_legend <- (view == "maandelijks" && "t" %in% names(df) && any(!is.na(df$t))) || dplyr::n_distinct(df$versie) > 1
    plot_obj <- plotly::ggplotly(agg_plot_obj(), tooltip = "text")
    plot_obj$x$data <- lapply(plot_obj$x$data, function(trace) {
      trace_name <- trace$name %||% ""
      if (length(custom_annotations) > 0 || !nzchar(trace_name) || grepl("^NA| NA", trace_name)) {
        trace$showlegend <- FALSE
      }
      trace
    })
    plot_obj |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = show_legend && length(custom_annotations) == 0,
        annotations = custom_annotations,
        margin = list(b = if (length(custom_annotations) > 0) 165 else 70),
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$tbl_agg <- renderDT({
    DT::datatable(filtered_agg(), options = list(pageLength = 15, scrollX = TRUE))
  })

  output$dl_agg_plot <- downloadHandler(
    filename = function() {
      paste0(
        build_export_name(
          "grafiek_uitkomsten",
          active_agg_sheet(),
          paste(selected_agg_names(), collapse = "_"),
          input$agg_stat %||% "maat"
        ),
        ".png"
      )
    },
    content = function(file) {
      save_plot_png(file, agg_plot_obj())
    }
  )

  agg_export_data <- reactive({
    df <- filtered_agg()
    req(nrow(df) > 0)

    view <- input$agg_view
    choices <- view_choices_for(agg_raw() |> dplyr::filter(name %in% selected_agg_names()))
    if (is.null(view) || !view %in% choices) view <- choices[[1]]

    split_col <- effective_agg_split()
    has_split <- split_col != "none" && split_col %in% names(df)

    df_plot <- df |>
      dplyr::filter(!is.na(value_num)) |>
      dplyr::mutate(
        versie = dplyr::coalesce(as.character(versie), "Geobserveerd"),
        outcome_label = pretty_metric_name(name, active_agg_sheet()),
        split_value = if (has_split) as.character(.data[[split_col]]) else "Totaal",
        split_label = if (has_split) pretty_value(split_col, split_value) else "Totaal",
        population_value = dplyr::coalesce(if ("died" %in% names(df)) population_label(died) else "Totaal", "Totaal"),
        cohort_value = if ("cohort" %in% names(df)) as.character(cohort) else ""
      )
    req(nrow(df_plot) > 0)

    if (identical(view, "maandelijks") && "t" %in% names(df_plot)) {
      df_plot |>
        dplyr::mutate(
          line_base = dplyr::case_when(
            has_split && dplyr::n_distinct(name) > 1 ~ paste(outcome_label, split_label, population_value, sep = " | "),
            has_split ~ split_label,
            dplyr::n_distinct(name) > 1 ~ paste(outcome_label, population_value, sep = " | "),
            TRUE ~ population_value
          ),
          versie_label = dplyr::recode(versie, Geobserveerd = "Niet gecorrigeerd", Inflatiecorrectie = "Inflatiecorrectie", .default = versie),
          series = if (dplyr::n_distinct(versie) > 1) paste(line_base, versie_label, sep = ", ") else line_base
        ) |>
        dplyr::transmute(
          category = as.character(numericize(t)),
          series = as.character(series),
          export_value = value_num
        )
    } else if (has_split) {
      df_plot |>
        dplyr::group_by(name, outcome_label, split_value, split_label, population_value, versie) |>
        dplyr::summarise(export_value = sum(value_num, na.rm = TRUE), .groups = "drop") |>
        dplyr::transmute(
          category = as.character(split_label),
          series = combine_series(population_value, versie),
          export_value = export_value
        )
    } else {
      df_plot |>
        dplyr::transmute(
          category = as.character(outcome_label),
          series = combine_series(population_value, versie, cohort_value),
          export_value = value_num
        )
    }
  })

  agg_export_chart_type <- reactive({
    if (identical(input$agg_view, "maandelijks")) "line" else "grouped_bar"
  })

  chart_data_downloads_server(
    id = "iter2_agg_dl",
    data = agg_export_data,
    chart_type = agg_export_chart_type,
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "rvs_uitkomsten",
    agg_fun = NULL
  )

  heatmap_available_splits <- reactive({
    cols <- Reduce(
      intersect,
      list(names(get_sheet("zvw")), names(get_sheet("msz_prestaties")))
    )
    dims <- intersect(allowed_split_columns("zvw", cols), allowed_split_columns("msz_prestaties", cols))
    dims[vapply(dims, function(col) {
      zvw_vals <- setdiff(unique(as.character(get_sheet("zvw")[[col]])), c(NA_character_, "all"))
      msz_vals <- setdiff(unique(as.character(get_sheet("msz_prestaties")[[col]])), c(NA_character_, "all"))
      length(intersect(zvw_vals, msz_vals)) > 0
    }, logical(1))]
  })

  output$hm_split_ui <- renderUI({
    dims <- heatmap_available_splits()
    req(length(dims) > 0)
    selected <- selected_hm_split()
    selectInput("hm_split", "Categorie", choices = choice_names(dims, pretty_split_name), selected = selected)
  })

  selected_hm_split <- reactive({
    dims <- heatmap_available_splits()
    req(length(dims) > 0)
    selected <- input$hm_split %||% isolate(input$hm_split)
    if (length(selected) == 0 || is.na(selected) || !selected %in% dims) {
      selected <- first_preferred(c("age_cat", "inkomen_klasse", "geslacht"), dims)
    }
    selected
  })

  selected_hm_stat <- reactive({
    choices <- names(metric_choices_basic)
    selected <- input$hm_stat %||% isolate(input$hm_stat)
    if (length(selected) > 0 && selected %in% unname(metric_choices_basic)) {
      selected <- names(metric_choices_basic)[match(selected, unname(metric_choices_basic))]
    }
    if (length(selected) == 0 || is.na(selected) || !selected %in% choices) selected <- "sum_totaal_groep"
    selected
  })

  output$hm_stat_ui <- renderUI({
    choices <- names(metric_choices_basic)
    selected <- selected_hm_stat()
    radioButtons("hm_stat", "Maat", choices = choice_names(choices, pretty_stat), selected = selected)
  })

  output$hm_cohort_ui <- renderUI({
    cohorts <- intersect(ordered_values(get_sheet("zvw")$cohort), ordered_values(get_sheet("msz_prestaties")$cohort))
    cohorts <- setdiff(cohorts, "all")
    if (length(cohorts) <= 1) return(NULL)
    selected <- isolate(input$hm_cohort)
    if (is.null(selected) || !selected %in% cohorts) selected <- if ("2023" %in% cohorts) "2023" else cohorts[[1]]
    radioButtons("hm_cohort", "Jaar", choices = cohorts, selected = selected)
  })

  heatmap_data_all <- reactive({
    split_col <- selected_hm_split()
    stat <- selected_hm_stat()
    cohort_values <- intersect(ordered_values(get_sheet("zvw")$cohort), ordered_values(get_sheet("msz_prestaties")$cohort))
    cohort <- input$hm_cohort
    if (length(cohort_values) == 0) cohort_values <- "2023"
    if (is.null(cohort) || length(cohort) == 0 || is.na(cohort) || !cohort %in% cohort_values) {
      cohort <- if ("2023" %in% cohort_values) "2023" else cohort_values[[1]]
    }

    zvw_names <- intersect(domain_order_zvw, unique(get_sheet("zvw")$name))
    msz_names <- intersect(c("vektmszvergoedbedragzvw"), unique(get_sheet("msz_prestaties")$name))
    sources <- list(
      list(sheet = "zvw", names = zvw_names),
      list(sheet = "msz_prestaties", names = msz_names)
    )

    dplyr::bind_rows(lapply(sources, function(src) {
      if (length(src$names) == 0) return(data.frame())
      base_df <- get_sheet(src$sheet)
      split_values <- heatmap_split_values(split_col, base_df[[split_col]])
      total <- aggregate_metric_data(
        src$sheet,
        names_keep = src$names,
        stat = stat,
        bin_size_filter = 1000,
        t_value = NULL,
        cohort_filter = cohort,
        died_filter = "Overleden"
      ) |>
        dplyr::mutate(kolom = "Totaal")
      split_df <- aggregate_metric_data(
        src$sheet,
        names_keep = src$names,
        stat = stat,
        bin_size_filter = 1000,
        t_value = NULL,
        cohort_filter = cohort,
        died_filter = "Overleden",
        split_col = split_col,
        split_values = split_values
      ) |>
        dplyr::mutate(kolom = pretty_value(split_col, .data[[split_col]]))
      dplyr::bind_rows(total, split_df) |>
        dplyr::mutate(
          dataset = ifelse(src$sheet == "msz_prestaties", "MSZ prestatie", pretty_sheet(src$sheet)),
          rij = if (identical(src$sheet, "msz_prestaties")) dataset else paste(dataset, pretty_metric_name(name, src$sheet), sep = " | "),
          waarde = value_num
        )
    }))
  })

  output$hm_rows_ui <- renderUI({
    df <- heatmap_data_all()
    req(nrow(df) > 0)
    rows <- unique(df$rij)
    div(
      style = "max-height: 280px; overflow-y: auto; padding-right: 4px;",
      checkboxGroupInput(
        "hm_rows",
        "Uitkomst",
        choices = rows,
        selected = rows
      )
    )
  })

  selected_hm_rows <- reactive({
    df <- heatmap_data_all()
    req(nrow(df) > 0)
    rows <- unique(df$rij)
    selected <- input$hm_rows
    if (is.null(selected)) return(rows)
    selected <- intersect(selected, rows)
    if (length(selected) == 0) rows else selected
  })

  heatmap_data <- reactive({
    heatmap_data_all() |>
      dplyr::filter(rij %in% selected_hm_rows())
  })

  format_heatmap_value <- function(x, stat) {
    if (identical(stat, "aandeel_gebruikers_berekend")) {
      scales::number(x, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
    } else {
      scales::comma(x, big.mark = ",", decimal.mark = ".", accuracy = 1)
    }
  }

  heatmap_display_data <- reactive({
    df <- heatmap_data()
    split_col <- selected_hm_split()
    stat <- selected_hm_stat()
    req(nrow(df) > 0, split_col %in% names(df))
    category_values <- heatmap_split_values(split_col, df[[split_col]])
    column_levels <- c("Totaal", pretty_value(split_col, category_values))
    row_levels <- unique(df$rij)

    df |>
      dplyr::mutate(
        Uitkomst = factor(rij, levels = row_levels),
        kolom = factor(kolom, levels = column_levels),
        waarde_weergave = ifelse(is.na(waarde), "", format_heatmap_value(waarde, stat))
      ) |>
      dplyr::select(Uitkomst, kolom, waarde_weergave) |>
      tidyr::pivot_wider(
        names_from = kolom,
        values_from = waarde_weergave,
        values_fn = list(waarde_weergave = ~ dplyr::first(.x)),
        values_fill = ""
      ) |>
      dplyr::arrange(Uitkomst) |>
      dplyr::mutate(Uitkomst = as.character(Uitkomst)) |>
      dplyr::select(Uitkomst, dplyr::any_of(column_levels))
  })

  heatmap_plot_obj <- reactive({
    df <- heatmap_data()
    split_col <- selected_hm_split()
    stat <- selected_hm_stat()
    req(nrow(df) > 0, split_col %in% names(df))
    category_values <- heatmap_split_values(split_col, df[[split_col]])
    column_levels <- c("Totaal", pretty_value(split_col, category_values))
    row_levels <- unique(df$rij)
    non_total_values <- df$waarde[!is.na(df$waarde) & df$kolom != "Totaal"]
    if (length(non_total_values) == 0) non_total_values <- 0
    value_range <- range(non_total_values, na.rm = TRUE)
    color_ramp <- grDevices::colorRampPalette(c("#FFFFFF", "#238B45"))(100)
    df <- df |>
      dplyr::mutate(
        kolom = factor(kolom, levels = column_levels),
        rij = factor(rij, levels = rev(row_levels)),
        fill_index = dplyr::case_when(
          as.character(kolom) == "Totaal" | is.na(waarde) ~ NA_integer_,
          diff(value_range) == 0 ~ 100L,
          TRUE ~ pmax(1L, pmin(100L, as.integer(round(sqrt((waarde - value_range[[1]]) / diff(value_range)) * 99 + 1))))
        ),
        fill_color = ifelse(is.na(fill_index), "#FFFFFF", color_ramp[fill_index]),
        label = ifelse(is.na(waarde), "", format_heatmap_value(waarde, stat)),
        tooltip = paste0(
          "Rij: ", rij, "<br>",
          "Kolom: ", kolom, "<br>",
          "Maat: ", pretty_stat(stat), "<br>",
          "Waarde: ", format_heatmap_value(waarde, stat), "<br>",
          "Aantal personen: ", scales::comma(n_totaal_num, big.mark = ",", decimal.mark = ".", accuracy = 1)
        )
      )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = kolom, y = rij, fill = fill_color, text = tooltip)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = label), size = 2.7, color = "#111827") +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
        panel.grid = ggplot2::element_blank(),
        legend.position = "none"
      ) +
      ggplot2::guides(fill = "none") +
      ggplot2::labs(fill = NULL) +
      ggplot2::ggtitle(paste("Heatmap", pretty_split_name(split_col), pretty_stat(stat), sep = " | "))

    p
  })

  output$plot_heatmap <- renderPlotly({
    plotly::ggplotly(heatmap_plot_obj(), tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE
      ) |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$dl_heatmap_plot <- downloadHandler(
    filename = function() {
      paste0(
        build_export_name(
          "grafiek_heatmap",
          selected_hm_split(),
          selected_hm_stat()
        ),
        ".png"
      )
    },
    content = function(file) {
      save_plot_png(file, heatmap_plot_obj())
    }
  )

  chart_data_downloads_server(
    id = "iter2_hm_dl",
    data = heatmap_display_data,
    chart_type = "scatter",
    category_col = "name",
    series_col = "name",
    value_col = "value_num",
    filename_prefix = "rvs_heatmap",
    agg_fun = NULL
  )

  observe({
    zvw_names <- intersect(domain_order_zvw, unique(get_sheet("zvw")$name))
    updateSelectInput(session, "pk_zvw_domain", choices = choice_names(zvw_names, function(x) pretty_metric_name(x, "zvw")), selected = "zvwktotaal")
  })

  pk_split_values <- reactive({
    df <- get_sheet("zvw")
    req(input$pk_split, input$pk_split %in% names(df))
    ordered_split_values(input$pk_split, df[[input$pk_split]])
  })

  output$plot_pk_bar <- renderPlotly({
    died_keep <- if (isTRUE(input$pk_show_control_bar)) c("Overleden", "In leven") else "Overleden"
    df <- aggregate_metric_data(
      "zvw",
      names_keep = input$pk_zvw_domain,
      stat = input$pk_metric,
      bin_size_filter = 1000,
      cohort = input$pk_year,
      died = died_keep,
      split_col = input$pk_split,
      split_values = pk_split_values()
    )
    req(nrow(df) > 0)
    df <- df |>
      dplyr::mutate(split_label = pretty_value(input$pk_split, .data[[input$pk_split]]))
    df$split_label <- factor(df$split_label, levels = rev(pretty_value(input$pk_split, pk_split_values())))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = split_label, y = value_num, fill = died, text = paste0(pretty_split_name(input$pk_split), ": ", split_label, "<br>Populatie: ", died, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = population_palette) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::ggtitle(paste("ZVW", pretty_metric_name(input$pk_zvw_domain, "zvw"), pretty_stat(input$pk_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$plot_pk_heatmap <- renderPlotly({
    df <- aggregate_metric_data(
      "zvw",
      names_keep = intersect(domain_order_zvw, unique(get_sheet("zvw")$name)),
      stat = input$pk_metric,
      bin_size_filter = 1000,
      cohort = input$pk_year,
      died = "Overleden",
      split_col = input$pk_split,
      split_values = pk_split_values()
    )
    req(nrow(df) > 0)
    df <- df |>
      dplyr::mutate(
        domein = factor(pretty_metric_name(name, "zvw"), levels = pretty_metric_name(rev(intersect(domain_order_zvw, unique(name))), "zvw")),
        split_label = factor(pretty_value(input$pk_split, .data[[input$pk_split]]), levels = pretty_value(input$pk_split, pk_split_values()))
      )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = split_label, y = domein, fill = value_num, text = paste0("Domein: ", domein, "<br>", pretty_split_name(input$pk_split), ": ", split_label, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient(low = "#E8F1F2", high = "#2D6A7E") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "bottom") +
      ggplot2::ggtitle(paste("Alle ZVW-domeinen", pretty_stat(input$pk_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$plot_pk_msz_line <- renderPlotly({
    df_main <- aggregate_metric_data(
      "msz_prestaties",
      names_keep = "vektmszvergoedbedragzvw",
      stat = input$pk_metric,
      bin_size_filter = 30,
      cohort = input$pk_year,
      died = "Overleden",
      split_col = input$pk_split,
      split_values = pk_split_values()
    )
    if (isTRUE(input$pk_show_control_line)) {
      df_control <- aggregate_metric_data("msz_prestaties", names_keep = "vektmszvergoedbedragzvw", stat = input$pk_metric, bin_size_filter = 30, cohort = input$pk_year, died = "In leven")
      df_control$line_label <- "In leven totaal"
    } else {
      df_control <- data.frame()
    }
    req(nrow(df_main) > 0)
    df_main$line_label <- pretty_value(input$pk_split, df_main[[input$pk_split]])
    df <- dplyr::bind_rows(df_main, df_control) |> dplyr::mutate(t_num = numericize(t))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = t_num, y = value_num, color = line_label, group = line_label, text = paste0("Lijn: ", line_label, "<br>Maand: ", t, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x = "Maand", y = NULL, color = NULL) +
      ggplot2::ggtitle(paste("MSZ over tijd", pretty_split_name(input$pk_split), pretty_stat(input$pk_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  diag_totals_data <- reactive({
    died_keep <- if (isTRUE(input$diag_show_control)) c("Overleden", "In leven") else "Overleden"
    df <- aggregate_metric_data("msz_activit_diag", names_keep = diag_activity_names, stat = input$diag_metric, bin_size_filter = input$diag_period, t_value = -1, cohort = input$diag_year, died = died_keep)
    df |> dplyr::mutate(activity = pretty_metric_name(name))
  })

  output$plot_diag_totals <- renderPlotly({
    df <- diag_totals_data()
    req(nrow(df) > 0)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = activity, y = value_num, fill = died, text = paste0("Activiteit: ", activity, "<br>Populatie: ", died, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = population_palette) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "bottom") +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::ggtitle(paste("Diagnostische activiteiten", pretty_stat(input$diag_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$plot_diag_compare <- renderPlotly({
    df <- dplyr::bind_rows(
      aggregate_metric_data("msz_activit_diag", diag_activity_names, input$diag_metric, 1000, -1, input$diag_year, "Overleden") |> dplyr::mutate(period = "Laatste 1000 dagen"),
      aggregate_metric_data("msz_activit_diag", diag_activity_names, input$diag_metric, 30, -1, input$diag_year, "Overleden") |> dplyr::mutate(period = "Laatste 30 dagen")
    ) |>
      dplyr::mutate(activity = pretty_metric_name(name))
    req(nrow(df) > 0)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = activity, y = value_num, fill = period, text = paste0("Activiteit: ", activity, "<br>Periode: ", period, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::ggtitle("Diagnostische activiteiten | 1000 dagen versus 30 dagen")
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  observe({
    df <- get_sheet("top_20_codes_activit_1000")
    cats <- c("Totaal activiteiten", ordered_values(df$beeldvorming_hoofdcategorie))
    updateSelectInput(session, "diag_top_cat", choices = cats, selected = "Totaal activiteiten")
  })

  output$tbl_diag_top20 <- renderDT({
    sheet_1000 <- get_sheet("top_20_codes_activit_1000")
    sheet_30 <- get_sheet("top_20_codes_activit_30")
    filter_cat <- function(df, period_value) {
      df <- df |> dplyr::filter(as.character(cohort) == input$diag_year, died == "Overleden")
      if ("period" %in% names(df)) df <- df |> dplyr::filter(period == period_value)
      if (!identical(input$diag_top_cat, "Totaal activiteiten")) df <- df |> dplyr::filter(beeldvorming_hoofdcategorie == input$diag_top_cat)
      df
    }
    base <- if (input$diag_top_period == "1000") filter_cat(sheet_1000, "laatste_1000_dagen") else filter_cat(sheet_30, "laatste_30_dagen")
    code_order <- base |> dplyr::arrange(dplyr::desc(.data[[input$diag_top_metric]])) |> dplyr::slice_head(n = 20) |> dplyr::pull(vektmszzorgactiviteit)
    tbl1000 <- filter_cat(sheet_1000, "laatste_1000_dagen") |> dplyr::filter(vektmszzorgactiviteit %in% code_order) |> dplyr::select(vektmszzorgactiviteit, mszzorgactiviteitomschrijving, n_gebruikers_1000 = n_totaal_gebruikers, n_declaraties_1000 = n_totaal_declaraties)
    tbl30 <- filter_cat(sheet_30, "laatste_30_dagen") |> dplyr::filter(vektmszzorgactiviteit %in% code_order) |> dplyr::select(vektmszzorgactiviteit, n_gebruikers_30 = n_totaal_gebruikers, n_declaraties_30 = n_totaal_declaraties)
    out <- tbl1000 |>
      dplyr::left_join(tbl30, by = "vektmszzorgactiviteit") |>
      dplyr::mutate(maandelijks_gemiddelde_1000 = n_gebruikers_1000 / 33) |>
      dplyr::arrange(match(vektmszzorgactiviteit, code_order))
    DT::datatable(out, options = list(pageLength = 20, scrollX = TRUE))
  })

  product_top_data <- reactive({
    sheet <- if (input$prod_period == "1000") "top_20_codes_operatie_1000" else "top_20_codes_operatie_30"
    period_value <- if (input$prod_period == "1000") "laatste_1000_dagen" else "laatste_30_dagen"
    df <- get_sheet(sheet) |>
      dplyr::filter(as.character(cohort) == input$prod_year, died == "Overleden", period == period_value)
    df |> dplyr::arrange(dplyr::desc(n_totaal_gebruikers)) |> dplyr::slice_head(n = 20)
  })

  output$plot_prod_top <- renderPlotly({
    df <- product_top_data()
    req(nrow(df) > 0)
    df <- df |>
      dplyr::mutate(
        value_num = numericize(.data[[input$prod_metric]]),
        label = stringr::str_trunc(paste0(vektmszdbczorgproduct, " | ", vektmszdbczorgproduct_naam), 72),
        label = factor(label, levels = rev(label))
      )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = value_num, text = paste0("Product: ", vektmszdbczorgproduct, "<br>Naam: ", wrap_hover(vektmszdbczorgproduct_naam), "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_col(fill = "#477998") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::ggtitle(paste("Type zorgproducten", pretty_code_metric(input$prod_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$tbl_prod_top20 <- renderDT({
    df1000 <- get_sheet("top_20_codes_operatie_1000") |> dplyr::filter(as.character(cohort) == input$prod_year, died == "Overleden")
    df1000 <- df1000 |> dplyr::filter(period == "laatste_1000_dagen")
    df30 <- get_sheet("top_20_codes_operatie_30") |> dplyr::filter(as.character(cohort) == input$prod_year, died == "Overleden", period == "laatste_30_dagen")
    codes <- product_top_data() |> dplyr::pull(vektmszdbczorgproduct)
    out <- df1000 |>
      dplyr::filter(vektmszdbczorgproduct %in% codes) |>
      dplyr::select(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, n_gebruikers_1000 = n_totaal_gebruikers, kosten_1000 = sum_totaal_groep) |>
      dplyr::left_join(df30 |> dplyr::select(vektmszdbczorgproduct, n_gebruikers_30 = n_totaal_gebruikers, kosten_30 = sum_totaal_groep), by = "vektmszdbczorgproduct") |>
      dplyr::mutate(maandelijks_gemiddelde_1000 = n_gebruikers_1000 / 33) |>
      dplyr::arrange(match(vektmszdbczorgproduct, codes))
    DT::datatable(out, options = list(pageLength = 20, scrollX = TRUE))
  })

  care_total_data <- reactive({
    died_keep <- if (isTRUE(input$care_show_control)) c("Overleden", "In leven") else "Overleden"
    base <- dplyr::bind_rows(
      aggregate_metric_data("zvw", "zvwktotaal", input$care_metric, 1000, -1, input$care_year, died_keep) |> dplyr::mutate(domein = "ZVW", versie = "Geobserveerd"),
      aggregate_metric_data("msz_prestaties", "vektmszvergoedbedragzvw", input$care_metric, 1000, -1, input$care_year, died_keep) |> dplyr::mutate(domein = "MSZ", versie = "Geobserveerd"),
      aggregate_metric_data("wlz", "bedragwlzzin", input$care_metric, 1000, -1, input$care_year, died_keep) |> dplyr::mutate(domein = "WLZ", versie = "Geobserveerd")
    )
    if (isTRUE(input$care_corrected) && is_cost_stat(input$care_metric)) {
      corrected <- dplyr::bind_rows(
        aggregate_metric_data("zvw_corrected", "zvwktotaal", input$care_metric, 1000, -1, input$care_year, died_keep) |> dplyr::mutate(domein = "ZVW", versie = "Inflatiecorrectie"),
        aggregate_metric_data("msz_prestaties_corrected", "vektmszvergoedbedragzvw", input$care_metric, 1000, -1, input$care_year, died_keep) |> dplyr::mutate(domein = "MSZ", versie = "Inflatiecorrectie"),
        aggregate_metric_data("wlz_corrected", "bedragwlzzin", input$care_metric, 1000, -1, input$care_year, died_keep) |> dplyr::mutate(domein = "WLZ", versie = "Inflatiecorrectie")
      )
      base <- dplyr::bind_rows(base, corrected)
    }
    base
  })

  output$plot_care_total <- renderPlotly({
    df <- care_total_data()
    req(nrow(df) > 0)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = domein, y = value_num, fill = died, text = paste0("Domein: ", domein, "<br>Versie: ", versie, "<br>Populatie: ", died, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~versie) +
      ggplot2::scale_fill_manual(values = population_palette) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::ggtitle(paste("Zorg totaal", pretty_stat(input$care_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$plot_care_time <- renderPlotly({
    died_keep <- if (isTRUE(input$care_show_control)) c("Overleden", "In leven") else "Overleden"
    df <- dplyr::bind_rows(
      aggregate_metric_data("msz_prestaties", "vektmszvergoedbedragzvw", input$care_metric, 30, NULL, input$care_year, died_keep) |> dplyr::mutate(domein = "MSZ"),
      aggregate_metric_data("wlz", "bedragwlzzin", input$care_metric, 30, NULL, input$care_year, died_keep) |> dplyr::mutate(domein = "WLZ")
    ) |>
      dplyr::mutate(t_num = numericize(t), lijn = paste(domein, died, sep = " | "))
    req(nrow(df) > 0)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = t_num, y = value_num, color = lijn, group = lijn, text = paste0("Domein: ", domein, "<br>Populatie: ", died, "<br>Maand: ", t, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Maand", y = NULL, color = NULL) +
      ggplot2::ggtitle(paste("Zorg over tijd", pretty_stat(input$care_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  observe({
    req(input$addon_sheet)
    df <- get_sheet(input$addon_sheet)
    vars <- unique(df$name)
    updateCheckboxGroupInput(session, "addon_vars", choices = choice_names(vars, pretty_metric_name), selected = head(vars, min(8, length(vars))))
  })

  output$plot_addon <- renderPlotly({
    req(input$addon_vars)
    bin <- if (input$addon_view == "1000") 1000 else 30
    t_filter <- if (input$addon_view == "monthly") NULL else -1
    df <- aggregate_metric_data(input$addon_sheet, input$addon_vars, input$addon_metric, bin, t_filter, input$addon_year, "Overleden") |>
      dplyr::mutate(geneesmiddel = pretty_metric_name(name), t_num = numericize(t))
    req(nrow(df) > 0)
    if (input$addon_view == "monthly" && "t" %in% names(df)) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = t_num, y = value_num, color = geneesmiddel, group = geneesmiddel, text = paste0("Geneesmiddel: ", geneesmiddel, "<br>Maand: ", t, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 1.5) +
        ggplot2::labs(x = "Maand", y = NULL, color = NULL)
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = geneesmiddel, y = value_num, fill = geneesmiddel, text = paste0("Geneesmiddel: ", geneesmiddel, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL)
    }
    p <- p + ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()) + ggplot2::ggtitle(paste(pretty_sheet(input$addon_sheet), pretty_stat(input$addon_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$plot_interventions <- renderPlotly({
    died_keep <- if (isTRUE(input$int_show_control)) c("Overleden", "In leven") else "Overleden"
    names_keep <- intersect(intervention_names, unique(get_sheet("msz_prestaties_diag")$name))
    df <- aggregate_metric_data("msz_prestaties_diag", names_keep, input$int_metric, input$int_period, -1, input$int_year, died_keep) |>
      dplyr::mutate(interventie = pretty_metric_name(name))
    req(nrow(df) > 0)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = interventie, y = value_num, fill = died, text = paste0("Interventie: ", interventie, "<br>Populatie: ", died, "<br>Waarde: ", scales::comma(value_num, big.mark = ",", decimal.mark = ".")))) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = population_palette) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::ggtitle(paste("Interventies", pretty_stat(input$int_metric), sep = " | "))
    plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  top_raw <- reactive({
    req(input$top_sheet)
    get_sheet(input$top_sheet)
  })

  output$top_metric_ui <- renderUI({
    df <- top_raw()
    req(nrow(df) > 0)
    numeric_cols <- top_metric_choices_for(df)
    req(length(numeric_cols) > 0)
    if (length(numeric_cols) <= 1) return(NULL)
    selected <- isolate(input$top_metric)
    if (is.null(selected) || !selected %in% numeric_cols) {
      selected <- first_preferred(c("n_totaal_gebruikers", "sum_totaal_groep", "n_totaal_declaraties"), numeric_cols)
    }
    radioButtons("top_metric", "Maat", choices = choice_names(numeric_cols, pretty_code_metric), selected = selected)
  })

  output$top_cohort_ui <- renderUI({
    df <- top_raw()
    req(nrow(df) > 0)
    if (!"cohort" %in% names(df)) return(NULL)
    values <- ordered_values(df$cohort)
    if (length(values) <= 1) return(NULL)
    selected <- isolate(input$top_cohort)
    if (is.null(selected) || !selected %in% values) selected <- if ("2023" %in% values) "2023" else values[[1]]
    radioButtons("top_cohort", "Cohort", choices = values, selected = selected)
  })

  output$top_category_filter <- renderUI({
    df <- top_raw()
    if (!"beeldvorming_hoofdcategorie" %in% names(df)) return(NULL)
    vals <- ordered_values(df$beeldvorming_hoofdcategorie)
    if (length(vals) <= 1) return(NULL)
    selected <- isolate(input$top_category)
    if (is.null(selected) || !selected %in% vals) selected <- vals[[1]]
    radioButtons("top_category", "Beeldvormingscategorie", choices = vals, selected = selected)
  })

  output$top_population_ui <- renderUI({
    df <- top_raw()
    req(nrow(df) > 0)
    if (!"died" %in% names(df)) return(NULL)
    if (!"In leven" %in% as.character(df$died)) return(NULL)
    checkboxInput("top_show_control", "Controle als tweede grafiek tonen", value = isTRUE(isolate(input$top_show_control)))
  })

  filtered_top <- reactive({
    df <- top_raw()
    req(nrow(df) > 0)
    metric <- input$top_metric
    metric_choices <- top_metric_choices_for(df)
    req(length(metric_choices) > 0)
    if (is.null(metric) || !metric %in% metric_choices) {
      metric <- first_preferred(c("n_totaal_gebruikers", "sum_totaal_groep", "n_totaal_declaraties"), metric_choices)
    }

    if ("cohort" %in% names(df)) {
      cohort_values <- ordered_values(df$cohort)
      selected_cohort <- input$top_cohort
      if (is.null(selected_cohort) || !selected_cohort %in% cohort_values) {
        selected_cohort <- if ("2023" %in% cohort_values) "2023" else cohort_values[[1]]
      }
      df <- df |> dplyr::filter(as.character(cohort) == selected_cohort)
    }
    if ("beeldvorming_hoofdcategorie" %in% names(df) && !is.null(input$top_category) && length(input$top_category) > 0) {
      df <- df |> dplyr::filter(as.character(beeldvorming_hoofdcategorie) == input$top_category)
    }

    keep_pop <- "Overleden"
    if (isTRUE(input$top_show_control)) keep_pop <- c("Overleden", "In leven")
    if ("died" %in% names(df)) df <- df |> dplyr::filter(as.character(died) %in% keep_pop)

    df |>
      dplyr::mutate(
        totaal_personen = if (all(c("cohort", "died") %in% names(df))) top_total_people(cohort, died) else NA_real_,
        gebruikers_per_persoon = if ("n_totaal_gebruikers" %in% names(df)) numericize(n_totaal_gebruikers) / totaal_personen else NA_real_,
        declaraties_per_persoon = if ("n_totaal_declaraties" %in% names(df)) numericize(n_totaal_declaraties) / totaal_personen else NA_real_,
        metric_value = numericize(.data[[metric]]),
        metric_name = metric
      )
  })

  top_butterfly_data <- reactive({
    df <- filtered_top()
    req(nrow(df) > 0)

    code_col <- first_existing(names(df), c("vektmszdbczorgproduct", "vektmszzorgactiviteit"))
    label_col <- first_existing(names(df), c("vektmszdbczorgproduct_naam", "mszzorgactiviteitomschrijving"))
    req(!is.na(code_col), "died" %in% names(df))

    main_period <- if (stringr::str_ends(input$top_sheet, "_30")) "laatste_30_dagen" else "laatste_1000_dagen"
    compare_period <- if (identical(main_period, "laatste_1000_dagen")) "laatste_30_dagen" else "laatste_1000_dagen"
    mode <- input$top_mode %||% "perioden"
    periods_keep <- switch(
      mode,
      alleen_1000 = "laatste_1000_dagen",
      alleen_30 = "laatste_30_dagen",
      c(main_period, compare_period)
    )

    sort_order <- df |>
      dplyr::mutate(
        code = as.character(.data[[code_col]]),
        period = as.character(period),
        sort_users = numericize(.data[["n_totaal_gebruikers"]])
      ) |>
      dplyr::filter(died == "Overleden", period == main_period) |>
      dplyr::group_by(code) |>
      dplyr::summarise(sort_users = sum(sort_users, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(sort_users)) |>
      dplyr::slice_head(n = 20)
    req(nrow(sort_order) > 0)

    labels <- df |>
      dplyr::mutate(
        code = as.character(.data[[code_col]]),
        omschrijving = if (!is.na(label_col)) clean_code_text(.data[[label_col]]) else ""
      ) |>
      dplyr::group_by(code) |>
      dplyr::summarise(omschrijving = dplyr::first(omschrijving[nzchar(omschrijving)] %||% ""), .groups = "drop")

    top_codes <- sort_order |>
      dplyr::left_join(labels, by = "code") |>
      dplyr::mutate(
        omschrijving = omschrijving %||% "",
        omschrijving_hover = wrap_hover(omschrijving),
        code_label = stringr::str_sub(paste0(code, ifelse(nzchar(omschrijving), paste0(" | ", omschrijving), "")), 1, 30),
        code_label = factor(code_label, levels = rev(code_label))
      )

    observed_periods <- df |>
      dplyr::mutate(
        code = as.character(.data[[code_col]]),
        period = as.character(period),
        populatie = population_label(died)
      ) |>
      dplyr::filter(code %in% top_codes$code, period %in% periods_keep) |>
      dplyr::group_by(code, period, populatie) |>
      dplyr::summarise(waarde = sum(metric_value, na.rm = TRUE), .groups = "drop")

    pop_keep <- if (isTRUE(input$top_show_control)) c("Overleden", "Controle") else "Overleden"
    df_periods <- tidyr::expand_grid(
      code = as.character(top_codes$code),
      period = periods_keep,
      populatie = pop_keep
    ) |>
      dplyr::left_join(observed_periods, by = c("code", "period", "populatie")) |>
      dplyr::mutate(waarde = dplyr::coalesce(waarde, 0)) |>
      dplyr::left_join(top_codes |> dplyr::select(code, code_label, omschrijving, omschrijving_hover), by = "code") |>
      dplyr::mutate(code_label = factor(code_label, levels = levels(top_codes$code_label)))

    if (identical(mode, "ratio")) {
      df_ratio <- df_periods |>
        tidyr::pivot_wider(names_from = period, values_from = waarde, values_fill = 0)
      for (col in c("laatste_1000_dagen", "laatste_30_dagen")) {
        if (!col %in% names(df_ratio)) df_ratio[[col]] <- NA_real_
      }
      return(df_ratio |>
        dplyr::mutate(
          waarde = dplyr::if_else(laatste_30_dagen > 0, laatste_1000_dagen / laatste_30_dagen, NA_real_),
          plot_waarde = waarde,
          periode_label = "Ratio 1000 / 30",
          groep_label = populatie,
          metric_name = dplyr::first(df$metric_name),
          metric_label = pretty_code_metric(dplyr::first(df$metric_name)),
          tooltip = paste0(
            "Code: ", code, "<br>",
            ifelse(nzchar(omschrijving), paste0("Omschrijving: ", omschrijving_hover, "<br>"), ""),
            "Populatie: ", populatie, "<br>",
            "Maat: Ratio ", metric_label, "<br>",
            "Waarde: ", format_code_value(waarde, dplyr::first(df$metric_name))
          )
        ))
    }

    df_periods |>
      dplyr::mutate(
        periode_label = period_label(period),
        plot_waarde = ifelse(mode == "perioden" & period == compare_period, -waarde, waarde),
        groep_label = if (isTRUE(input$top_show_control)) paste(periode_label, populatie, sep = " | ") else periode_label,
        metric_name = dplyr::first(df$metric_name),
        metric_label = pretty_code_metric(dplyr::first(df$metric_name)),
        tooltip = paste0(
          "Code: ", code, "<br>",
          ifelse(nzchar(omschrijving), paste0("Omschrijving: ", omschrijving_hover, "<br>"), ""),
          "Populatie: ", populatie, "<br>",
          "Periode: ", periode_label, "<br>",
          "Maat: ", metric_label, "<br>",
          "Waarde: ", format_code_value(waarde, dplyr::first(df$metric_name))
        )
      )
  })

  top_plot_obj <- reactive({
    df_plot <- top_butterfly_data()
    req(nrow(df_plot) > 0)
    metric_label <- dplyr::first(df_plot$metric_label)
    metric_name <- dplyr::first(df_plot$metric_name)

    color_values <- top_period_palette
    missing_groups <- setdiff(unique(df_plot$groep_label), names(color_values))
    if (length(missing_groups) > 0) {
      color_values <- c(color_values, stats::setNames(build_palette(length(missing_groups)), missing_groups))
    }

    code_levels <- levels(df_plot$code_label)
    if (is.null(code_levels)) code_levels <- unique(as.character(df_plot$code_label))
    df_plot <- df_plot |>
      dplyr::mutate(
        code_index = as.numeric(factor(code_label, levels = code_levels)),
        is_control = populatie == "Controle"
      ) |>
      dplyr::arrange(is_control)

    nonzero_df_plot <- df_plot |> dplyr::filter(!is.na(plot_waarde), plot_waarde != 0)
    control_rows <- nonzero_df_plot |>
      dplyr::filter(is_control) |>
      dplyr::mutate(
        xmin = pmin(0, plot_waarde),
        xmax = pmax(0, plot_waarde),
        ymin = code_index - 0.36,
        ymax = code_index + 0.36
      )
    main_rows <- nonzero_df_plot |>
      dplyr::filter(!is_control) |>
      dplyr::mutate(
        xmin = pmin(0, plot_waarde),
        xmax = pmax(0, plot_waarde),
        ymin = code_index - 0.23,
        ymax = code_index + 0.23
      )
    p <- ggplot2::ggplot()
    if (nrow(control_rows) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = control_rows,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = groep_label, text = tooltip),
          alpha = 0.72,
          color = NA
        )
    }
    if (nrow(main_rows) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = main_rows,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = groep_label, text = tooltip),
          alpha = 0.96,
          color = NA
        )
    }
    p <- p +
      ggplot2::geom_vline(xintercept = 0, color = "#4b5563", linewidth = 0.3) +
      ggplot2::scale_x_continuous(labels = function(x) format_code_value(abs(x), metric_name)) +
      ggplot2::scale_y_continuous(
        breaks = seq_along(code_levels),
        labels = function(x) {
          idx <- round(x)
          out <- rep("", length(idx))
          valid <- !is.na(idx) & idx >= 1 & idx <= length(code_levels)
          out[valid] <- code_levels[idx[valid]]
          out
        },
        expand = ggplot2::expansion(mult = c(0.02, 0.02))
      ) +
      ggplot2::scale_fill_manual(values = color_values[unique(df_plot$groep_label)]) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 8),
        plot.margin = ggplot2::margin(8, 18, 8, 8)
      ) +
      ggplot2::guides(fill = "none") +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::ggtitle(paste(pretty_sheet(input$top_sheet), "|", metric_label))

    p
  })

  output$plot_top <- renderPlotly({
    plot_obj <- suppressWarnings(plotly::ggplotly(top_plot_obj(), tooltip = "text"))
    plot_obj$x$data <- lapply(plot_obj$x$data, function(trace) {
      if (identical(trace$fill %||% "", "toself")) {
        trace$hoveron <- "fills"
        trace$hoverinfo <- "text"
        trace$showlegend <- FALSE
      }
      trace
    })
    plot_obj |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        hovermode = "closest"
      ) |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  output$tbl_top <- renderDT({
    table_df <- filtered_top()
    if ("died" %in% names(table_df) && !is.null(input$top_population) && length(input$top_population) > 0) {
      table_df <- table_df |> dplyr::filter(as.character(died) %in% input$top_population)
    }
    DT::datatable(
      table_df |> dplyr::select(-dplyr::any_of("metric_value")),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$dl_top_plot <- downloadHandler(
    filename = function() {
      paste0(
        build_export_name(
          "grafiek_top_codes",
          input$top_sheet %||% "dataset",
          input$top_mode %||% "weergave",
          input$top_metric %||% "maat"
        ),
        ".png"
      )
    },
    content = function(file) {
      save_plot_png(file, top_plot_obj())
    }
  )

  top_export_data <- reactive({
    df <- top_butterfly_data()
    if (nrow(df) == 0) return(df)
    df |>
      dplyr::transmute(
        category = as.character(code_label),
        series = as.character(groep_label),
        export_value = waarde
      )
  })

  chart_data_downloads_server(
    id = "iter2_top_dl",
    data = top_export_data,
    chart_type = "bar",
    category_col = "category",
    series_col = "series",
    value_col = "export_value",
    filename_prefix = "rvs_top_codes",
    agg_fun = NULL
  )
}

# Run the app (Using your existing wrapper)
options(shiny.error = function() {
  err <- geterrmessage()
  message(sprintf("[shiny.error] %s", err))
  writeLines(sprintf("[shiny.error] %s", err), con = "shiny_error.log")
})

if (exists("secure_app", mode = "function") && exists("secure_server", mode = "function")) {
  shinyApp(ui = secure_app(ui), server = server)
} else {
  shinyApp(ui, server)
}
