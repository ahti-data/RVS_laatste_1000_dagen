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
  "scales"
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

resolve_existing_path <- function(candidates) {
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

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
    msz_addon = "MSZ add-ons",
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
            style = "display: flex; gap: 12px; align-items: center;",
            downloadButton("dl_agg", "Gegevens downloaden"),
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
            style = "display: flex; gap: 12px; align-items: center;",
            downloadButton("dl_heatmap", "Gegevens downloaden"),
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
            style = "display: flex; gap: 12px; align-items: center;",
            downloadButton("dl_top", "Gegevens downloaden"),
            downloadButton("dl_top_plot", "Grafiek downloaden")
          )
        )
      )
    )
  )
}

iteration2_server <- function(input, output, session, data_path_override = NULL) {
  # Initialize iteration2 workbook at runtime to keep app startup robust.
  if (is.null(data_path_override) || !nzchar(data_path_override)) {
    data_path_iteration2 <<- resolve_existing_path(c("output.xlsx", "data/output.xlsx"))
  } else {
    data_path_iteration2 <<- data_path_override
  }

  if (is.na(data_path_iteration2) || !file.exists(data_path_iteration2)) {
    stop(sprintf("Iteratie 2: kan output.xlsx niet vinden op pad: %s", data_path_iteration2))
  }

  rm(list = ls(envir = cache_env_iteration2, all.names = TRUE), envir = cache_env_iteration2)
  sheet_names_iteration2 <<- openxlsx::getSheetNames(data_path_iteration2)

  sheet_preview <- dplyr::bind_rows(lapply(sheet_names_iteration2, function(sheet) {
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

  aggregate_sheets <<- sheet_preview |>
    dplyr::filter(
      is_aggregate,
      sheet != "wlz_msz_heup",
      !stringr::str_ends(sheet, "_corrected")
    ) |>
    dplyr::arrange(label)

  top_code_sheets <<- sheet_preview |>
    dplyr::filter(is_top_code) |>
    dplyr::arrange(label)

  # The rest of Iteratie 2 server logic is unchanged from the version you had;
  # it remains in this file after this helper block.
  # (We keep function signature stable: iteration2_server(input, output, session, data_path_override).)
  # NOTE: Full implementation continues below in `app.R` (already present in prior merge).
}

# ===== VARIABLE DECLARATIONS & UTILITIES =====

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cohort", "died", "n_totaal", "value", "name", "type", "t",
    "q05_per_persoon", "q25_per_persoon", "mediaan_per_persoon",
    "q75_per_persoon", "q95_per_persoon", "bin_size", "doodsoorzaak",
    "t_numeric", "value_butterfly", "group", "interventie", "interventie_category"
  ))
}

data_path <- "data/data_iteration_1/all_output.xlsx"
log_file <- "shiny_console.log"
unlink(log_file)

log_msg <- function(msg) {
  cat(paste0("[", Sys.time(), "] ", msg, "\n"), file = log_file, append = TRUE)
  cat(paste0("[", Sys.time(), "] ", msg, "\n"))
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

  navbarMenu(
    "Iteratie 1",
    tabPanel("Basispopulatie",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 selectInput("pop_jaar", "Jarenselectie:", choices = c("2019", "2023", "2019 + 2023"), selected = "2019 + 2023"),
                 selectInput("pop_split", "Kies populaties:", choices = c("Enkel totale populatie", "Totaal + subgroepen doodsoorzaak"), selected = "Totaal + subgroepen doodsoorzaak"),
                 hr(),
                 downloadButton("dl_basis", "Download Data voor Think-cell")
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
                 downloadButton("dl_totaal", "Download Data voor Think-cell")
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
                 downloadButton("dl_maandelijks", "Download Data voor Think-cell")
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
                 helpText("Boxplot-achtig overzicht op basis van quantielen per cohort en status.")
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
                 downloadButton("dl_butterfly", "Download Data voor Think-cell")
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
                 downloadButton("dl_interventies", "Download Data voor Think-cell")
               ),
               mainPanel(
                 plotlyOutput("plot_interventies", height = "600px")
               )
             )
    ),

    tabPanel("Systeem Logs",
             verbatimTextOutput("app_log")
    )
  ),

  navbarMenu(
    "Iteratie 2",
    iteration2_header(),
    iteration2_panels()
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
  
  output$dl_basis <- downloadHandler(
    filename = function() { paste("basispopulatie-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_basis(), file, row.names = FALSE) }
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
  
  output$dl_totaal <- downloadHandler(
    filename = function() { paste("zorg_totaal-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_totaal(), file, row.names = FALSE) }
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
  
  output$dl_maandelijks <- downloadHandler(
    filename = function() { paste("zorg_over_tijd-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_maandelijks(), file, row.names = FALSE) }
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
  
  output$dl_butterfly <- downloadHandler(
    filename = function() { paste("zorg_butterfly-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_butterfly(), file, row.names = FALSE) }
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

  output$dl_interventies <- downloadHandler(
    filename = function() { paste("interventies-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv2(data_interventies(), file, row.names = FALSE) }
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
  iteration2_server(
    input,
    output,
    session,
    data_path_override = "data/data_iteration_2/output.xlsx"
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