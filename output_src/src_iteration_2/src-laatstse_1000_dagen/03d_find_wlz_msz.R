# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Find whether a person has used Wlz before being diagnosed with a condition
# Output: A sample of rinpersoon with dummy variables
# Last edited: 22 April 2026

rm(list = ls())
gc()
source("./src/00_inputs.R")

dt_rin <- r_parquet_get_dt("./data/processed/msz_prestatie_first_rin.parquet")
dt_wlz <- list()
for (yr in years) {
  print(yr)
  
  file_path <- get_path_newest(
    path = "G:/GezondheidWelzijn/WLZZINTAB/",
    string_pattern = yr,
    extension = ".sav",
    method = "max_version"
  )
  
  dt <- haven::read_sav(file_path, col_select = all_of(cols_to_select_wlz))
  dt <- format_data(dt)
  dt <- dt[rinpersoon %in% dt_rin$rinpersoon]
  
  dt_wlz[[yr]] <- dt
  rm(dt)
  gc()
}

dt_wlz <- rbindlist(dt_wlz, use.names = T)
rm(dt_rin)

for (outcome in c('heeft_aaa_totaal', 'heeft_heup_totaal')){
  print(outcome)
  
  dt <- r_parquet_get_dt(glue::glue("./data/raw/msz_prestatie_first_{outcome}.parquet"))
  
  # Merge
  dt <- merge(dt, dt_wlz, by = 'rinpersoon', all.x = T, allow.cartesian = T)
  setorder(dt, rinpersoon, beginwlzzin)
  
  # Create a dummy if a person used Wlz a month before being diagnosed
  dt[, beginwlzzin := as.Date(beginwlzzin, format = "%Y%m%d")]
  wlz_outcome <- glue::glue('wlz_before_{outcome}')
  dt[, (wlz_outcome) := fifelse(
    !is.na(beginwlzzin) & !is.na(vektmszbegindatumprest),
    as.integer(beginwlzzin - 30 < vektmszbegindatumprest), 
    0L)]
  dt <- dt[, setNames(list(as.integer(any(get(wlz_outcome) == 1L, na.rm = T))),
                       wlz_outcome),
            by = .(rinpersoon, gbadatumoverlijden)]
  
  setindex(dt, NULL)
  arrow::write_parquet(dt, glue::glue("./data/processed/msz_wlz_{outcome}.parquet"))
  rm(dt)
  gc()
}


