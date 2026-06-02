packages <- c(
  "shiny",
  "openxlsx",
  "writexl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "htmlwidgets",
  "DT",
  "stringr",
  "scales"
)

missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages before running the dashboard: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(lapply(packages, library, character.only = TRUE))
options(scipen = 999)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

resolve_existing_path <- function(candidates) {
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

data_path <- resolve_existing_path(c("output.xlsx", "data/output.xlsx"))
if (is.na(data_path)) {
  stop("Kan output.xlsx niet vinden in de projectmap of in data/output.xlsx.")
}

demographic_cols <- c(
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

pretty_default <- function(x) {
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
    .default = pretty_default(x)
  )
}

pretty_type <- function(x) {
  dplyr::recode(
    x,
    gemiddelde_per_persoon = "Gemiddelde per persoon",
    n_totaal_gebruikers = "Aantal gebruikers",
    sum_totaal_groep = "Totale som",
    .default = pretty_default(x)
  )
}

stat_labels <- c(
  sum_totaal_groep = "Totale som",
  n_totaal_gebruikers = "Aantal gebruikers",
  aandeel_gebruikers_berekend = "Aandeel gebruikers",
  gemiddelde_per_gebruiker_berekend = "Gemiddelde per gebruiker",
  gemiddelde_per_persoon_berekend = "Gemiddelde per persoon",
  gemiddelde_per_persoon = "Gemiddelde per persoon (export)"
)

pretty_stat <- function(x) {
  unname(stat_labels[x] %||% pretty_type(x))
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
    .default = pretty_default(x)
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
    stringr::str_replace_all('^"+|"+$', "") |>
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
    .default = pretty_default(x)
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

population_inflation_palette <- c(
  population_palette,
  stats::setNames(lighten_color(population_palette), paste(names(population_palette), "Inflatiecorrectie", sep = ", "))
)

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
    .default = pretty_default(x)
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

  dims <- intersect(demographic_cols, names(df))
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

ordered_values <- function(x) {
  vals <- unique(as.character(x))
  vals <- vals[!is.na(vals)]
  preferred <- c("all", "Overleden", "In leven", "2019", "2023")
  c(intersect(preferred, vals), sort(setdiff(vals, preferred)))
}

numericize <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

value_labeler <- function(metric_name, stat_type) {
  if (startsWith(metric_name %||% "", "heeft_") &&
      !identical(stat_type, "n_totaal_gebruikers")) {
    scales::label_percent(accuracy = 0.1, decimal.mark = ",")
  } else {
    scales::label_number(big.mark = ".", decimal.mark = ",")
  }
}

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

sheet_names <- openxlsx::getSheetNames(data_path)

safe_read_sheet <- function(sheet) {
  tryCatch(
    openxlsx::read.xlsx(data_path, sheet = sheet),
    error = function(e) {
      warning(sprintf("Failed to read sheet %s: %s", sheet, e$message))
      data.frame()
    }
  )
}

sheet_preview <- dplyr::bind_rows(lapply(sheet_names, function(sheet) {
  df <- safe_read_sheet(sheet)
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

aggregate_sheets <- sheet_preview |>
  dplyr::filter(
    is_aggregate,
    sheet != "wlz_msz_heup",
    !stringr::str_ends(sheet, "_corrected")
  ) |>
  dplyr::arrange(label)

corrected_sheet_for <- function(sheet) {
  candidate <- paste0(sheet, "_corrected")
  if (candidate %in% sheet_names) candidate else NA_character_
}

top_code_sheets <- sheet_preview |>
  dplyr::filter(is_top_code) |>
  dplyr::arrange(label)

cache_env <- new.env(parent = emptyenv())

get_sheet <- function(sheet) {
  key <- paste0("sheet__", sheet)
  if (!exists(key, envir = cache_env, inherits = FALSE)) {
    assign(key, safe_read_sheet(sheet), envir = cache_env)
  }
  get(key, envir = cache_env, inherits = FALSE)
}

choice_names <- function(values, labeler = identity) {
  vals <- unique(as.character(values))
  vals <- vals[!is.na(vals)]
  stats::setNames(vals, vapply(vals, labeler, character(1)))
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
  if (!is.null(data_path_override) && nzchar(data_path_override)) {
    data_path <<- data_path_override
    rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env)
    sheet_names <<- openxlsx::getSheetNames(data_path)
  }

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

    dims <- intersect(demographic_cols, names(df))
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

  output$dl_agg <- downloadHandler(
    filename = function() {
      paste0(
        build_export_name("rvs_uitkomsten", active_agg_sheet(), input$agg_stat %||% "maat"),
        ".xlsx"
      )
    },
    content = function(file) writexl::write_xlsx(filtered_agg(), file)
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

  output$dl_heatmap <- downloadHandler(
    filename = function() {
      paste0(
        build_export_name(
          "rvs_heatmap",
          selected_hm_split(),
          selected_hm_stat()
        ),
        ".xlsx"
      )
    },
    content = function(file) {
      writexl::write_xlsx(heatmap_display_data(), file)
    }
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

  output$dl_top <- downloadHandler(
    filename = function() paste0("rvs_top_codes_", input$top_sheet, ".xlsx"),
    content = function(file) writexl::write_xlsx(filtered_top(), file)
  )

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
}
