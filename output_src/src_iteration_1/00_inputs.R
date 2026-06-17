# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Set inputs used in other files
# Output: None
# Last edited: 23 February 2026

library(data.table)
library(glue)
source("H:/utils/backup/20260109/m_functions.R")
source("H:/utils/backup/20260109/demog_functions.R")

years <- 2016:2023

cols_to_select_stapeling_31_12 <- c("burgstaat") # 'geslacht', 'leeftijd'

cols_to_label_stapeling_31_12 <- c("burgstaat") # 'geslacht'

cols_to_select_stapeling_01_01 <- c(
  "hbopl", "hgopl", "huishsamstsocec",
  "belanginkbronpers", "percsm", "inkpers"
)

cols_to_label_stapeling_01_01 <- c(
  "hbopl", "hgopl", "huishsamstsocec",
  "belanginkbronpers"
)

cols_to_select_overlijden <- c("RINPERSOON", "GBADatumOverlijden")

cols_to_select_doodoorz <- c("RINPERSOON", "UCCODE")

cols_to_select_gbapersoon <- c(
  "RINPERSOON", "GBAGESLACHT", "GBAGEBOORTEJAAR",
  "GBAGEBOORTEMAAND"
)

cols_to_select_gbahuishoudensbus <- c("RINPERSOON", "DATUMAANVANGHH", "DATUMEINDEHH")

cols_to_select_zorgkosten <- c(
  "RINPERSOON", "ZVWKTOTAAL", "ZVWKZIEKENHUIS",
  "ZVWKFARMACIE", "ZVWKWYKVERPLEGING", "ZVWKHULPMIDDEL",
  "ZVWKHUISARTS", "NOPZVWKHUISARTSINSCHRIJF",
  "NOPZVWKHUISARTSCONSULT", "NOPZVWKHUISARTSOVERIG"
)

cols_to_select_msz_prestatie <- c('RINPERSOON', 
                                  'VEKTMSZSpecialismeDiagnoseCombinatie', 
                                  'VEKTMSZDBCZorgproduct', 'VEKTMSZBegindatumPrest',
                                  'VEKTMSZDeclaratiecode', 'VEKTMSZDeclaratietype',
                                  'VEKTMSZVergoedbedragZVW')

cols_to_select_msz_activiteiten <- c('RINPERSOON', 
                                     #'VEKTMSZKoppelIDPrestZa', 'VEKTMSZBeginjaarPrest',
                                     'VEKTMSZZorgactiviteit', 
                                     'VEKTMSZZorgactiviteitdatum')

cols_to_select_medicijn <- c('RINPERSOON', 'ATC4')

cols_to_select_huisarts <- c("RINPERSOON", "HADECLPrestatiecode")

# cost columns by dataset
cost_columns_zvw <- c("zvwktotaal", "zvwkziekenhuis", "zvwkfarmacie",
                      "zvwkwykverpleging", "zvwkhulpmiddel", "zvwkhuisarts",
                      "nopzvwkhuisartsinschrijf", "nopzvwkhuisartsconsult",
                      "nopzvwkhuisartsoverig", "zvwkggzzpmtotaal")
cost_columns_msz <- c("vektmszvergoedbedragzvw", "vektmszvergoedbedragav")
cost_columns_huisarts <- c("hadeclvergoedbedrag", "hadeclvergoedbedrag_inschrijving", "hadeclvergoedbedrag_consulatations", "hadeclvergoedbedrag_others")
cost_columns_wlz <- c("bedragwlzzin")
cost_columns_wvp <- c("bedragzvwwvp")


cpi <- data.table(
  year = c(2015:2024),
  cpi = c(
    100.00, 100.32, 101.70, 103.44, 106.16,
    107.51, 110.39, 121.43, 126.09, 130.31
  )
)
base_2023 <- cpi[year == 2023, cpi]
cpi[, cpi := cpi / base_2023]

make_aggregated_data <- function(df, group_var = "leeftijd_cat", columns = NULL) {
  col_names <- if (is.null(columns)) {
    names(df)
  } else {
    columns
  }

  # Calculate means and medians
  cols_binary <- col_names[grepl("^(heeft|gebruikt|referred|has|used)", col_names)]
  if (length(cols_binary) == 0) {
    cols_binary <- col_names[grepl("heeft|gebruikt|referred|has|used", col_names)]
  }
  cols_cont <- col_names[grepl("^(zvw|nopzvw|kosten|n_|vektmszvergoedbedrag|hadeclvergoedbedrag|bedrag)", col_names)]
  cols_other <- setdiff(col_names, c(cols_binary, cols_cont))
  cols_binary <- setdiff(cols_binary, group_var)
  cols_cont <- setdiff(cols_cont, group_var)
  cols_other <- unique(c(cols_other, group_var))

  cat("\n Binary variables: ", paste(cols_binary, collapse = ", "), "\n")
  cat("\n Continuous variables: ", paste(cols_cont, collapse = ", "), "\n")
  cat("\n Other variables: ", paste(cols_other, collapse = ", "), "\n")

  # Binary variables
  if (length(cols_binary) == 0) {
    cat("No binary columns found")
    df_agg_binary <- df[, .(n_totaal = round(.N, -1)),
      by = group_var
    ]
  } else {
    df_agg_binary <- df[, c(
      setNames(
        lapply(.SD, function(x) as.numeric(round(mean(x, na.rm = T), 5))),
        paste0(names(.SD), "_gemiddelde_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) round(sum(x>0, na.rm=T), -1)),
        paste0(names(.SD), "_n_totaal_gebruikers")
      ),
      .(n_totaal = round(.N, -1))
    ),
    by = group_var,
    .SDcols = cols_binary
    ]
  }

  # Continuous variables
  if (length(cols_cont) == 0) {
    cat("No continuous columns found")
    df_agg_cont <- unique(df[, ..group_var])
  } else {
    df_agg_cont <- df[, c(
      setNames(
        lapply(.SD, function(x) as.numeric(mean(x, na.rm = T))),
        paste0(names(.SD), "_gemiddelde_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(mean(x[x != 0], na.rm = T))),
        paste0(names(.SD), "_gemiddelde_per_gebruiker")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(median(x, na.rm = T))),
        paste0(names(.SD), "_mediaan_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(median(x[x != 0], na.rm = T))),
        paste0(names(.SD), "_mediaan_per_gebruiker")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(quantile(x, 0.05, na.rm = T))),
        paste0(names(.SD), "_q05_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(quantile(x, 0.25, na.rm = T))),
        paste0(names(.SD), "_q25_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(quantile(x, 0.75, na.rm = T))),
        paste0(names(.SD), "_q75_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(quantile(x, 0.95, na.rm = T))),
        paste0(names(.SD), "_q95_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) sum(x, na.rm = T)),
        paste0(names(.SD), "_sum_totaal_groep")
      ),
      setNames(
        lapply(.SD, function(x) round(sum(x>0, na.rm=T), -1)),
        paste0(names(.SD), "_n_totaal_gebruikers")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(round(mean(x > 0, na.rm = T), 5))),
        paste0(names(.SD), "_gebruikt_per_persoon")
      )
    ),
    by = group_var,
    .SDcols = cols_cont
    ]
  }

  df_agg <- merge(df_agg_binary, df_agg_cont, by = group_var, all = T)

  df_agg <- drop_if_below_x(df_agg, c(cols_binary, cols_cont), x = 10)

  first_cols <- c(group_var, "n_totaal")
  other_cols <- setdiff(names(df_agg), first_cols)
  other_cols <- sort(other_cols)
  setcolorder(df_agg, c(first_cols, other_cols))

  # Make it to long format
  df_agg <- melt(df_agg,
    id.vars = first_cols,
    measure.vars = other_cols,
    variable.name = "variable",
    value.name = "value"
  )

  # Split a variable name into two parts
  df_agg[, c("name", "type") := {
    parts <- stringr::str_split(variable, "_")
    list(
      sapply(parts, \(x) paste(head(x, -3), collapse = "_")),
      sapply(parts, \(x) paste(tail(x, 3), collapse = "_"))
    )
  }]

  # Drop if below 10 counts
  df_agg <- df_agg[complete.cases(value)]

  return(df_agg)
}


# If the number of people with a given condition < 10, code all measures as NA
drop_if_below_x <- function(df_agg, cols, x = 10) {
  for (col in cols) {
    var_cols <- grep(paste0("^", col, "($|_)"), names(df_agg), value = T)
    df_agg[get(paste0(col, "_n_totaal_gebruikers")) <= x, (var_cols) := NA]
  }
  return(df_agg)
}

calculate_costs_by_bin_size <- function(
    costs_dt,
    overlijden_dt,
    cost_columns,
    cost_date_col,
    bin_size = c("months33", "50", "100", "1000"),
    convert_cost_date_col = FALSE
    ){
  # define bins
  chosen_bin_size <- match.arg(bin_size)
  step_days <- switch(chosen_bin_size,
                      "months33" = 30,
                      "50" = 50,
                      "100" = 100,
                      "1000" = 1000
  )
  
  # copies
  costs_dt_clean <- copy(costs_dt)
  overlijden_dt_clean <- copy(overlijden_dt)
  
  n_bins <- floor(1000 / step_days)
  
  if (convert_cost_date_col) {
  costs_dt_clean[, (cost_date_col) := as.Date(as.character(get(cost_date_col)), format = "%Y%m%d")]
  }
  
  # change costs to numeric
  costs_dt_clean[, (cost_columns) := lapply(.SD, as.numeric), .SDcols = cost_columns]
  
  overlijden_dt_clean <- copy(overlijden_dt)
  overlijden_dt_clean[, row_id := .I]
  dt_grid <- overlijden_dt_clean[, .(
    t = -1:-(n_bins)
  ), by = .(row_id, rinpersoon, gbadatumoverlijden, cohort, died, doodsoorzaak)]
  
  
  dt_grid[, bin_end := gbadatumoverlijden + ((t + 1) * step_days)]
  dt_grid[, bin_start := gbadatumoverlijden + ((t) * step_days) + 1]
  
  # convert to numeric for foverlaps
  dt_grid[, c("bin_start", "bin_end") := lapply(.SD, function(x) {
    year(x) * 10000 + month(x) * 100 + mday(x)
  }), .SDcols = c("bin_start", "bin_end")]
  costs_dt_clean[, ":="(start_temp = get(cost_date_col), end_temp = get(cost_date_col))]
  costs_dt_clean[, c("start_temp", "end_temp") := lapply(.SD, function(x) {
    year(x) * 10000 + month(x) * 100 + mday(x)
  }), .SDcols = c("start_temp", "end_temp")]
  
  setkey(dt_grid, rinpersoon, bin_start, bin_end)
  setkey(costs_dt_clean, rinpersoon, start_temp, end_temp)
  print("overlapping")
  dt_merged <- foverlaps(
    x = dt_grid,
    y = costs_dt_clean,
    mult = "all",
    type = "any",
    nomatch = NA
  )
  
  print("merging")
  dt_merged <- dt_merged[,
                         lapply(.SD, sum, na.rm =TRUE),
                         by = .(rinpersoon, gbadatumoverlijden, cohort, died, t, doodsoorzaak),
                         .SDcols = cost_columns
  ]
  return(dt_merged)
}

