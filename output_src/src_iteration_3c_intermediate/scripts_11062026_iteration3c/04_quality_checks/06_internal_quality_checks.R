# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Do internal checks of the aggregated data
# Output: None
# Last edited: 28 April 2026

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(ggplot2)
library(dplyr)
options(scipen = 999)

path <- glue("./output/iteration_2/")

agg_files <- list.files(
  path = path,
  pattern = '^agg_.*\\.csv$',
  full.names = T
)

names <- tools::file_path_sans_ext(basename(agg_files))

dt_sample <- dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet", 
  columns = c('rinpersoon', 'cohort', 'died', 'doodsoorzaak'))
n_totaal <- dt_sample[, .(n_totaal_raw = round(.N, -1)), by = .(cohort, died)]
n_totaal_cancer <- dt_sample[doodsoorzaak == 'Palliatief kanker', 
                             .(n_totaal_raw = round(.N, -1)), by = .(cohort, died)]

for (i in seq_along(agg_files)) {
  print(glue::glue('Checking: {names[i]}'))
  dt <- as.data.table(rio::import(agg_files[[i]]))
  
  # First, check that the total number of people match
  if (names[i] %in% c('agg_msz_addon_oncology_cancer', 'agg_msz_addon_oncology_total_cancer')) {
    dt <- merge(dt, n_totaal_cancer, by = c('cohort', 'died'))
    
  } else {
    dt <- merge(dt, n_totaal, by = c('cohort', 'died'))
  }
  
  assertthat::assert_that(any(dt$n_totaal == dt$n_totaal_raw))
  
  # Second, check that the total number match the sum of groups
  for (cat in c("doodsoorzaak", "age_cat", "geslacht", "inkomen_klasse",
                 "seswoa_cat", "migratie_achtergrond", "huishoudsamenstelling", 
                 "stedgem", "wlz_start_period")) {
    print(glue::glue('Checking: {cat}'))
    dt_cat <- dt[get(cat) != 'all']
    n_totaal_cat <- dt_cat[, .(n_totaal_cat = sum(n_totaal, na.rm = T)), 
                               by = .(variable, bin_size, cohort, died)]
    n_totaal_cat <- merge(n_totaal_cat, n_totaal, by = c('cohort', 'died'))
    assertthat::assert_that(any(dt$n_totaal == dt$n_totaal_raw))
  }
  
  rm(dt)
}
