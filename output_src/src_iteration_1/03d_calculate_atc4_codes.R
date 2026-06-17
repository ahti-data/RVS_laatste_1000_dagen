# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Count incidents of the usage of 5 or more ATC-4 codes in the last 1000 days
# Output: A list of people with their usage 
# Last edited: 10 March 2026

rm(list = ls())
gc()
source("./src/00_inputs.R")
library(dplyr)
dt_overlijden_with_matched <- r_parquet_get_dt("data/raw/overlijden_with_matched.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)
n_total <- nrow(dt_overlijden_with_matched)

years <- 2016:2023

dt_medicijn <- data.table()
for (yr in years) {
  print(yr)
  
  filepath <- get_newest_parquet_check(
    folder_h_parquet = "H:/data/Parquet_files_G_drive/MEDICIJNTAB/parquet_files",
    folder_g_parquet = "G:/GezondheidWelzijn/MEDICIJNTAB/Geconverteerde bestanden/",
    folder_g_sav = glue("G:/GezondheidWelzijn/MEDICIJNTAB/{yr}"),
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
  )
  
  ds <- arrow::open_dataset(filepath)
  dt <- ds |>
    filter(RINPERSOON %in% rinpersoon_set) |>
    select(all_of(cols_to_select_medicijn)) |>
    collect()
  dt <- format_data(dt)
  
  # Keep ATC-4 codes only in the last 1000 days 
  dt <- merge(dt_overlijden_with_matched, 
              dt, 
              by = 'rinpersoon', all.x = T, 
              allow.cartesian = T)
  dt[, end_date := as.Date(glue('{yr}-12-31'))]
  dt <- dt[end_date <= gbadatumoverlijden & 
              end_date >= gbadatumoverlijden - 1000]
  dt <- dt[, end_date := NULL]
  
  dt <- dt[, .(gebruikt_minstens5_atc4 = as.integer(uniqueN(atc4) >= 5)), 
           by = .(rinpersoon, gbadatumoverlijden, cohort, died, doodsoorzaak)]
  
  # If usage is NA, code them as 0
  dt <- merge(dt_overlijden_with_matched, 
              dt, all.x = T, 
              by = c("rinpersoon", "gbadatumoverlijden", "cohort", "died", 'doodsoorzaak'))
  cols_to_fill <- setdiff(names(dt), c(
    "rinpersoon", "gbadatumoverlijden", "cohort", "died", 'doodsoorzaak'))
  setnafill(dt, fill = 0, cols = cols_to_fill)
  
  stopifnot(nrow(dt) == n_total)
  
  dt_medicijn <- rbindlist(list(dt_medicijn, dt), use.names = T)
  rm(dt)
  gc()
}

dt_medicijn <- dt_medicijn[, .(gebruikt_minstens5_atc4 = as.integer(gebruikt_minstens5_atc4 > 0)), 
                           by = .(rinpersoon, gbadatumoverlijden, cohort, died, doodsoorzaak)]

setindex(dt_medicijn, NULL)
arrow::write_parquet(dt_medicijn, "./data/processed/medicijn.parquet")
rm(dt_medicijn, dt_overlijden_with_matched)
gc()

