# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Calculate the share of people with and without Wlz who got diagnosed and
# had an operation after
# Output: Aggregated data
# Last edited: 28 April 2026

# WE NOT LONGER HAVE IT
# rm(list = ls())
# gc()
# source("./src/00_inputs.R")
# 
# wb <- openxlsx::createWorkbook()
# 
# for (outcome in c('heeft_aaa_totaal', 'heeft_heup_totaal')){
#   print(outcome)
#   
#   dt <- r_parquet_get_dt(glue::glue("./data/processed/msz_wlz_{outcome}.parquet"))
#   
#   ds <- arrow::open_dataset("./data/processed/msz_prestatie_monthly.parquet")
#   msz <- ds |>
#     filter(as.numeric(rinpersoon) %in% dt$rinpersoon) |>
#     collect()
#   
#   # Merge to msz_wlz because I do not need everyone, but only those who have a condition
#   dt <- merge(dt, msz, by = c('rinpersoon', 'gbadatumoverlijden'), all.x = T)
#   
#   # Make aggregation for the full 1000 days, not monthly
#   cols_binary <- names(dt)[grepl("^(n_aaa_|n_heup_)",  names(dt))]
#   setnames(dt, cols_binary, sub('^n_', 'heeft_', cols_binary))
#   cols_binary <- names(dt)[grepl("^(heeft_aaa_|heeft_heup_)",  names(dt))]
#   dt[, (cols_binary) := lapply(.SD, function(x) 
#       as.integer(x > 0)),
#       .SDcols = cols_binary
#   ]
#   
#   wlz_outcome <- glue::glue('wlz_before_{outcome}')
#   dt <- dt[, c('rinpersoon', 'cohort', 'died', wlz_outcome, cols_binary), with = F]
#   dt <- dt[, lapply(.SD, max, na.rm=T), by = 
#              c('rinpersoon', 'cohort', 'died', wlz_outcome),
#                                 .SDcols = cols_binary]
#   dt_agg <- make_aggregated_data(dt, group_var = c('cohort', 'died', wlz_outcome))
#   
#   # Save
#   openxlsx::addWorksheet(wb, glue::glue('{outcome}'))
#   openxlsx::writeData(wb, glue::glue('{outcome}'), dt_agg)
# }
# 
# openxlsx::saveWorkbook(wb, glue::glue('./output/iteration_2/wlz_msz.xlsx'), overwrite = T)
