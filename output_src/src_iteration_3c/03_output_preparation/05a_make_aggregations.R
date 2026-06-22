# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Aggregate all datasets 
# Output: Final output for export
# Last edited: 28 April 2026

#### NOTES ####
# note: Checked WLZ figures compared to iteration 1: for cohort 2023, overleden, 
# monthly bin size, the sum_totaal groep and n_totaal_gebruikers are now equal.
# For the In leven population, the n_totaal_gebruikers is now slightly higher (due to sample_id),
# but also the costs are slightly higher? Confirm whether this is intended

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(tictoc)
options(scipen = 999)

# TO DO:
# wlzkosten_monthly_corrected.parquet is not yet updated
# check that all files are updated (best to rerun the whole pipeline and 
# delete all datasets in raw and proccessed folders
# Do internal checks (check internal checks from the second iteration)
# Do external checks of the results of iteration 3 to iteratino 1

dataset_filepaths <- list(
  # WE DO NOT USE huisartsdecltab and wijkverpleging
  ###huisartsdecltab = "./data/processed/huisarts_monthly.parquet",
  ###wijkverpleging = "./data/processed/wvpkosten_monthly.parquet",
  
  msz_prestatie_diagnostiek = "./data/processed/msz_prestatie_monthly.parquet",
  msz_activit_diagnostiek = "./data/processed/msz_activiteiten_monthly.parquet"

  # msz_addon takes a lot of time
  #msz_addon = "./data/processed/msz_addon_monthly.parquet"
  
  # msz_addon_oncology_total = "./data/processed/msz_addon_oncology_total_monthly.parquet",
  # msz_addon_oncology_total_cancer = "./data/processed/msz_addon_oncology_total_cancer_monthly.parquet",
  # msz_addon_oncology_cancer = "./data/processed/msz_addon_oncology_cancer_monthly.parquet",
  # 
  # msz_prestaties = "./data/processed/vektmszkosten_monthly.parquet",
  # msz_prestaties_corrected = "./data/processed/vektmszkosten_monthly_corrected.parquet",
  # wlz = "./data/processed/wlzkosten_monthly.parquet",
  # wlz_corrected = "./data/processed/wlzkosten_monthly_corrected.parquet",
  # zvw = "./data/processed/zvw_1000_dagen.parquet",
  # zvw_corrected = "./data/processed/zvw_1000_dagen_corrected.parquet"
)

#### functions ####

aggregate_for_output <- function(filepath, dataset_name) {
  print(dataset_name)
  ds <- arrow::open_dataset(filepath)
  
  group_cols_always <- c(
    "bin_size",
    "t",
    "cohort",
    "died")
  
  group_cols_demog <- c(
    "doodsoorzaak",
    "age_cat",
    "geslacht",
    "inkomen_klasse",
    "seswoa_cat",
    "migratie_achtergrond",
    'huishoudsamenstelling',
    "stedgem",
    "wlz_start_period",
    "provincie",
    "used_any_acp_2years"
    #"burgstaat",
    #"nndlocationcode"
  )
  
  cost_columns <- setdiff(names(ds), c(
    c(group_cols_always, group_cols_demog),
    "rinpersoon",
    "sample_id",
    "gbadatumoverlijden",
    "t",
    "bin_start", 
    "bin_end",
    "n_days_available",
    "used_acp_31244_2years",
    "used_acp_31381_2years",
    "burgstaat",
    "vektmszdbczorgproduct",
    "vektmszdbczorgproduct_naam"
    #"nndlocationcode"
  ))
  
  
  col_names_other <- setdiff(names(ds), c(
    cost_columns,
    "used_acp_31244_2years",
    "used_acp_31381_2years",
    "burgstaat",
    "vektmszdbczorgproduct",
    "vektmszdbczorgproduct_naam"))
  #}
  if (length(cost_columns) > 45) {
    print(length(cost_columns))
    n_chunks <- ceiling(length(cost_columns) / 30)
    print(n_chunks)
    cost_column_chunks <- split(cost_columns, cut(seq_along(cost_columns), n_chunks, labels = FALSE))
  } else {
    cost_column_chunks <- list(cost_columns)
  }
  
  dt_dataset_total <- rbindlist(
    lapply(cost_column_chunks, apply_all_aggregations, 
           cost_columns, ds, col_names_other, group_cols_always, group_cols_demog, dataset_name)
  )
  
  # intermediate save in case of crash
  fwrite(dt_dataset_total, glue::glue("data/processed/aggregated_costs/agg_{dataset_name}.csv"))
  
  return(dt_dataset_total)
}


apply_all_aggregations <- function(cost_columns_chunk, all_cost_columns, ds, 
                                   col_names = NULL, group_var_always, group_var_demog, dataset_name) {
  if (!is.null(col_names)) {
    dt_dataset <- ds |>
      select(c(dplyr::all_of(c(cost_columns_chunk, col_names)))) |>
      collect()
  } else {
    dt_dataset <- ds |>
      # tail(100000) |>
      collect()
  }
  setDT(dt_dataset)
  
  # aggregate for 1000
  cols_binary <- cost_columns_chunk[grepl("^(heeft|gebruikt|referred|has|used)", cost_columns_chunk)]
  if (length(cols_binary) == 0) {
    cols_binary <- cost_columns_chunk[grepl("heeft|gebruikt|referred|has|used", cost_columns_chunk)]
  }
  group_var_all <- c(group_var_always, group_var_demog)
  
  cols_cont <- cost_columns_chunk[grepl("^(zvw|nopzvw|kosten|n_|vektmszvergoedbedrag|hadeclvergoedbedrag|bedrag)", cost_columns_chunk)]
  cols_other <- setdiff(cost_columns_chunk, c(cols_binary, cols_cont))
  cols_binary <- setdiff(cols_binary, group_var_all)
  cols_cont <- setdiff(cols_cont, group_var_all)
  cols_other <- unique(c(cols_other, group_var_all))
  
  cat("\n Binary variables: ", paste(cols_binary, collapse = ", "), "\n")
  cat("\n Continuous variables: ", paste(cols_cont, collapse = ", "), "\n")
  cat("\n Other variables: ", paste(cols_other, collapse = ", "), "\n")
  
  if(!"t" %in% names(dt_dataset)) dt_dataset[, ':='(t= -1, bin_size = 1000)] # for annual costs
  
  if (length(cols_binary) > 0 & length(cols_cont > 0)) {
    dt_dataset_1000_binary <- dt_dataset[, lapply(.SD, max, na.rm = T),
                                         by = c(setdiff(group_var_all, c("bin_size", "t")), "rinpersoon", "sample_id"),
                                         .SDcols = cols_binary][, ':='(bin_size = 1000, t = -1)]
    dt_dataset_1000_cont <- dt_dataset[, lapply(.SD, sum, na.rm = T),
                                       by = c(setdiff(group_var_all, c("bin_size", "t")), "rinpersoon", "sample_id"),
                                       .SDcols = cols_cont][, ':='(bin_size = 1000, t = -1)]
    dt_dataset_1000 <- merge(dt_dataset_1000_cont, dt_dataset_1000_binary, by = c(group_var_all, "rinpersoon", "sample_id"))
    
  } else if (length(cols_binary > 0)) {
    dt_dataset_1000 <- dt_dataset[, lapply(.SD, max, na.rm = T),
                                  by = c(setdiff(group_var_all, c("bin_size", "t")), "rinpersoon", "sample_id"),
                                  .SDcols = cols_binary][, ':='(bin_size = 1000, t = -1)]
    
  } else {
    dt_dataset_1000 <- dt_dataset[, lapply(.SD, sum, na.rm = T),
                                  by = c(setdiff(group_var_all, c("bin_size", "t")), "rinpersoon", "sample_id"),
                                  .SDcols = cols_cont][, ':='(bin_size = 1000, t = -1)]
  }
  
  rm(dt_dataset_1000_binary, dt_dataset_1000_cont)
  gc()
  
  # ensure same cols
  dt_dataset[, bin_size := 30]
  dt_dataset <- dt_dataset[, .SD, .SDcols = names(dt_dataset_1000)]
  
  dt_dataset <- rbindlist(list(
    dt_dataset_1000,
    dt_dataset
  ), use.names=T)
  
  # First, make aggregations for all
  dt_dataset_agg <- make_aggregated_data(
    dt_dataset,
    group_var <- group_var_always,
    columns = cost_columns_chunk
  )[, (group_var_demog) := "all"]
  
  # Calculate splits only for some datasets
  demog_vars_to_split <- c("wlz_start_period",
                               "provincie",
                               "used_any_acp_2years")
  if (any(cost_columns_chunk %in% c('bedragwlzzin', 'vektmszvergoedbedragzvw', 
                                    'bedrag_13_01_88', 'n_13_01_88',
                                    'heeft_heup_totaal', 'heeft_eerste_mri',
                                    "zvwktotaal", "zvwkziekenhuis", "zvwkfarmacie", 
                                    "zvwkwykverpleging", "zvwkhulpmiddel", "zvwkhuisarts",
                                    "nopzvwkhuisartsinschrijf", "nopzvwkhuisartsconsult",
                                    "nopzvwkhuisartsoverig" , "zvwkggzzpmtotaal")) | dataset_name %in% c(
                                      "msz_prestatie_diagnostiek", "msz_activit_diagnostiek"
                                    )){
    
    # Second, make aggregations split by doodsoorzaak and demographics 
    dt_dataset_agg_split_demog <- rbindlist(lapply(demog_vars_to_split, function(demog_col) {
      print(glue("splitting for {demog_col}"))
      dt <- make_aggregated_data(
        dt_dataset,
        group_var <- c(group_var_always, demog_col),
        columns = cost_columns_chunk
      )
      
      rename_cols <- c(setdiff(group_var_demog, demog_col))
      dt[, (rename_cols) := "all"]
      return(dt)
    }), use.names=T)
    
    dt_dataset_total <- rbindlist(list(
      dt_dataset_agg,
      dt_dataset_agg_split_demog
    ), use.names=T)
    
  } else {
    dt_dataset_total <- dt_dataset_agg
  }
  return(dt_dataset_total)
}

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
  
  df[, (group_var) := lapply(.SD, as.factor), .SDcols = group_var]
  setkeyv(df, c(group_var))
  
  
  # Binary variables
  if (length(cols_binary) == 0) {
    cat("No binary columns found")
    df_agg_binary <- df[, .(n_totaal = round(fndistinct(sample_id), -1)),
                        by = group_var
    ]
  } else {
    df_agg_binary <- df[, c(
      setNames(
        lapply(.SD, function(x) as.numeric(round(fmean(x, na.rm = T), 5))),
        paste0(names(.SD), "_gemiddelde_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) round(fndistinct(sample_id[x > 0]), -1)),
        paste0(names(.SD), "_n_totaal_gebruikers")
      ),
      .(n_totaal = round(fndistinct(sample_id), -1))
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
    tic("cont aggregations", quiet = F)
    #expr_gemiddelde_per_persoon <- paste0(cols_cont, "_gemiddelde_per_persoon = fmean(", cols_cont, ", na.rm = TRUE)")
    #expr_gemiddelde_per_gebruiker <- paste0(cols_cont, "_gemiddelde_per_gebruiker = fmean(", cols_cont, "[", cols_cont,  "!= 0], na.rm = T)")
    expr_sum_totaal_groep <- paste0(cols_cont, "_sum_totaal_groep = sum(", cols_cont, ", na.rm = TRUE)")
    expr_n_totaal_gebruikers <- paste0(cols_cont, "_n_totaal_gebruikers = round(sum(", cols_cont, "> 0, na.rm = TRUE), -1)")
    #expr_gebruikt_per_persoon <- paste0(cols_cont, "_gebruikt_per_persoon = round(fmean(", cols_cont, "> 0, na.rm = TRUE), 5)")
    
    all_expr <- c(
      #expr_gemiddelde_per_persoon, expr_gemiddelde_per_gebruiker, 
      expr_sum_totaal_groep, expr_n_totaal_gebruikers 
      #expr_gebruikt_per_persoon
    )
    j_code <- paste0(".(", paste(all_expr, collapse = ", "), ")")
    
    # print(j_code)
    
    df_agg_cont <- df[, eval(parse(text = j_code)), by = group_var]
    toc()
  }
  
  tic("rest of func", quiet = F)
  
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
  df_agg[, ':='(
    type = sub(".*_([^_]+_[^_]+_[^_]+)$", "\\1", variable),
    name = sub("(_[^_]+_[^_]+_[^_]+)$", "", variable)
  )]
  
  # Drop if below 10 counts
  df_agg <- df_agg[complete.cases(value)]
  
  toc()
  
  return(df_agg)
}

#### apply and write ####
output_aggregations <- mapply(aggregate_for_output, dataset_filepaths, 
                              names(dataset_filepaths), SIMPLIFY = FALSE)
