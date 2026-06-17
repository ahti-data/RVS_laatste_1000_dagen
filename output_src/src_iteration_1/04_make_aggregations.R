#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(ggplot2)
library(dplyr)
options(scipen = 999)

path <- glue("./output/iteration_1/")
dir.create(path, recursive = T, showWarnings = F)

dataset_filepaths <- list(
  zvw = "./data/processed/zvw_1000_dagen.parquet",
  medicijn = "./data/processed/medicijn.parquet",
  msz_eerstelijns_diagnostiek = "./data/processed/msz_prestatie_1000_dagen.parquet",
  msz_tweedelijns_diagnostiek = "./data/processed/msz_activiteiten_1000_dagen.parquet",
  msz_prestaties = "./data/processed/vektmszkosten_monthly.parquet",
  # #  "./data/processed/huisarts_monthly.parquet",
  wlz = "./data/processed/wlzkosten_monthly.parquet",
  wijkverpleging = "./data/processed/wvpkosten_monthly.parquet"
)

#### functions ####

aggregate_for_output <- function(filepath, dataset_name) {
  print(dataset_name)
  ds <- arrow::open_dataset(filepath)
  
   if (dataset_name == 'msz_eerstelijns_diagnostiek') {
     cost_columns <- names(ds)[grepl("^(kosten_)", names(ds))]
     col_names_other <- c('rinpersoon', 'gbadatumoverlijden', 't', 
                          'cohort', 'died', 'doodsoorzaak')
   } else {
    cost_columns <- setdiff(names(ds), c(
      "rinpersoon",
      "gbadatumoverlijden",
      "t",
      "bin_start", 
      "bin_end",
      "cohort",
      "died",
      "doodsoorzaak"
    ))
    col_names_other <- NULL
  }
  
  if (length(cost_columns) > 5) {
    print(length(cost_columns))
    n_chunks <- ceiling(length(cost_columns) / 5)
    print(n_chunks)
    cost_column_chunks <- split(cost_columns, cut(seq_along(cost_columns), n_chunks, labels = FALSE))
  } else {
    cost_column_chunks <- list(cost_columns)
  }
  
  dt_dataset_total <- rbindlist(
    lapply(cost_column_chunks, apply_all_aggregations, cost_columns, ds, col_names_other)
  )
  # intermediate save in case of crash
  openxlsx2::write_xlsx(dt_dataset_total, glue::glue("./output/iteration_1/all_output{dataset_name}.xlsx"))
  
  
  return(dt_dataset_total)
}

apply_all_aggregations <- function(cost_columns_chunk, all_cost_columns, ds, col_names = NULL) {
  
  if (!is.null(col_names)) {
    dt_dataset <- ds |>
      select(c(dplyr::all_of(c(cost_columns_chunk, col_names)))) |>
      collect()
  } else {
    dt_dataset <- ds |>
      collect()  
  }
  setDT(dt_dataset)
  
  # aggregate for 1000
  cols_binary <- cost_columns_chunk[grepl("^(heeft|gebruikt|referred|has|used)", cost_columns_chunk)]
  if (length(cols_binary) == 0) {
    cols_binary <- cost_columns_chunk[grepl("heeft|gebruikt|referred|has|used", cost_columns_chunk)]
  }
  group_var <- setdiff(names(dt_dataset), c(all_cost_columns, "t"))
  
  cols_cont <- cost_columns_chunk[grepl("^(zvw|nopzvw|kosten|n_|vektmszvergoedbedrag|hadeclvergoedbedrag|bedrag)", cost_columns_chunk)]
  cols_other <- setdiff(cost_columns_chunk, c(cols_binary, cols_cont))
  cols_binary <- setdiff(cols_binary, group_var)
  cols_cont <- setdiff(cols_cont, group_var)
  cols_other <- unique(c(cols_other, group_var))
  
  if (length(cols_binary) > 0 & length(cols_cont > 0)) {
    dt_dataset_1000_binary <- dt_dataset[, lapply(.SD, max, na.rm=T),
                                         by = group_var,
                                         .SDcols = cols_binary]
    dt_dataset_1000_cont <- dt_dataset[, lapply(.SD, sum, na.rm=T),
                                       by = group_var,
                                       .SDcols = cols_cont]
    dt_dataset_1000 <- merge(dt_dataset_1000_cont, dt_dataset_1000_binary, by = group_var)
  } else if (length(cols_binary > 0)) {
    dt_dataset_1000 <- dt_dataset[, lapply(.SD, max, na.rm=T),
                                         by = group_var,
                                         .SDcols = cols_binary]
  } else {
    dt_dataset_1000 <- dt_dataset[, lapply(.SD, sum, na.rm=T),
                                       by = group_var,
                                       .SDcols = cols_cont]
  }
  
  rm(dt_dataset_1000_binary, dt_dataset_1000_cont)
  gc()

  dt_dataset_aggregated_1000 <- make_aggregated_data(
    dt_dataset_1000,
    group_var = c("cohort", "died"),
    columns = cost_columns_chunk
  )[, ':='(
    t = -1,
    bin_size = "1000days",
    doodsoorzaak = "all"
  )]
  
  dt_dataset_aggregated_1000_doodsoorzaak <- make_aggregated_data(
    dt_dataset_1000,
    group_var = c("cohort", "died", "doodsoorzaak"),
    columns = cost_columns_chunk
  )[, ':='(
    t = -1,
    bin_size = "1000days"
  )]
  
  rm(dt_dataset_1000)
  gc()
  
  if ("t" %in% names(dt_dataset)) {
    dt_dataset_aggregated_monthly <- make_aggregated_data(
      dt_dataset,
      group_var = c("cohort", "died", "t"),
      columns = cost_columns_chunk
    )[, ':='(
      bin_size = "monthly",
      doodsoorzaak = "all"
    )]
    
    dt_dataset_aggregated_monthly_doodsoorzaak <- make_aggregated_data(
      dt_dataset,
      group_var = c("cohort", "died", "t", "doodsoorzaak"),
      columns = cost_columns_chunk
    )[, ':='(
      bin_size = "monthly"
    )]
    
    dt_dataset_total <- rbindlist(list(
      dt_dataset_aggregated_1000,
      dt_dataset_aggregated_monthly,
      dt_dataset_aggregated_1000_doodsoorzaak,
      dt_dataset_aggregated_monthly_doodsoorzaak
    ), use.names=TRUE)
  } else {
    dt_dataset_total <- rbindlist(list(
      dt_dataset_aggregated_1000,
      dt_dataset_aggregated_1000_doodsoorzaak
    ), use.names=TRUE)
  }
  

  return(dt_dataset_total)
}

#### apply and write ####
output_aggregations <- mapply(aggregate_for_output, dataset_filepaths, names(dataset_filepaths), SIMPLIFY = FALSE)
openxlsx2::write_xlsx(output_aggregations, "./output/iteration_1/all_output.xlsx")
