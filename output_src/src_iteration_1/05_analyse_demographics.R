# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Look at demographic characteristics and causes of death
# Output: Average age, sex, migration background
# Last edited: 18 March 2025

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(ggplot2)
options(scipen = 999)

make_agg <- function(file){
  df <- file
  n_total <- nrow(df)
  dem <- r_parquet_get_dt('./data/raw/gbapersoon.parquet')
  df <- merge(df, dem, by = 'rinpersoon', all.x = T)
  rm(dem)
  
  migratie_achtergrond <- r_parquet_get_dt("./data/raw/migratie_achtergrond.parquet")
  df <- merge(df, migratie_achtergrond, by = 'rinpersoon', all.x = T)
  rm(migratie_achtergrond)
  
  stopifnot(nrow(df) == n_total)
  
  ###
  # DELETE AFTER RERUNNING 02_find_random_mathces.R
  df[, date_of_birth := as.Date(ISOdate(year_of_birth, month_of_birth, 1))]
  df[, age_at_death := as.numeric((gbadatumoverlijden - date_of_birth) / 365.25)]
  df[, vrouw := as.integer(geslacht == 'Vrouwen')]
  ###
  
  df_agg <- df[, .(age_at_death = round(mean(age_at_death), 3),
                   vrouw = round(mean(vrouw), 3),
                   migratie_achtergrond = round(mean(migratie_achtergrond), 3),
                   n_total = round(.N, -1)),
               by = .(cohort, died)]
  
  # 159 people have missing cause of death, assign Overig
  df[is.na(doodsoorzaak), doodsoorzaak := 'Onbekend']
  df_doodoorz <- df[died == 'Overleden', .(age_at_death = round(mean(age_at_death), 3),
                                           vrouw = round(mean(vrouw), 3),
                                           migratie_achtergrond = round(mean(migratie_achtergrond), 3),
                                           n_total = round(.N, -1)), 
                    by = .(doodsoorzaak, cohort)]
  df_doodoorz_total <- df[died == 'Overleden', .(age_at_death = round(mean(age_at_death), 3),
                                                 vrouw = round(mean(vrouw), 3),
                                                 migratie_achtergrond = round(mean(migratie_achtergrond), 3),
                                                 n_total = round(.N, -1)), 
                          by = .(cohort)]
  df_doodoorz_total[, doodsoorzaak := 'Alle']
  df_doodoorz <- rbindlist(list(df_doodoorz, df_doodoorz_total), use.names = T)
  setorder(df_doodoorz, cohort, -n_total)
  return(list(df_agg = df_agg, 
              df_doodoorz = df_doodoorz))
}

# Counts for all deceased
overliden <- r_parquet_get_dt("./data/raw/overlijden.parquet")
overliden <- overliden[year_of_death %in% c(2019, 2023)]
overliden[, died := 'Overleden']
setnames(overliden, 'year_of_death', 'cohort')

doodoorz <- r_parquet_get_dt("./data/raw/doodoorz.parquet")
overliden <- merge(overliden, doodoorz, by = 'rinpersoon', all.x = T)
rm(doodoorz)
gc()

overliden_agg <- make_agg(overliden)
overliden_agg$df_agg[, dataset := 'Alle overleden']

# Counts for deceased after restrictions
overlijden_with_matched <- r_parquet_get_dt('./data/raw/overlijden_with_matched.parquet')
overlijden_with_matched_agg <- make_agg(overlijden_with_matched)
overlijden_with_matched_agg$df_agg[, dataset := 'Geselecteerde overleden']

df_agg <- rbindlist(list(overliden_agg$df_agg,
                    overlijden_with_matched_agg$df_agg))
setorder(df_agg, cohort, dataset, -died)

setnames(overliden_agg$df_doodoorz, 'n_total', 'n_total_alle_overleden')
setnames(overliden_agg$df_doodoorz, 'age_at_death', 'age_at_death_alle_overleden')
setnames(overliden_agg$df_doodoorz, 'vrouw', 'vrouw_alle_overleden')
setnames(overliden_agg$df_doodoorz, 'migratie_achtergrond', 'migratie_achtergrond_alle_overleden')

setnames(overlijden_with_matched_agg$df_doodoorz, 'n_total', 'n_total_geselecteerde_overleden')
setnames(overlijden_with_matched_agg$df_doodoorz, 'age_at_death', 'age_at_death_geselecteerde_overleden')
setnames(overlijden_with_matched_agg$df_doodoorz, 'vrouw', 'vrouw_geselecteerde_overleden')
setnames(overlijden_with_matched_agg$df_doodoorz, 'migratie_achtergrond', 'migratie_achtergrond_geselecteerde_overleden')

df_doodoorz <- merge(overliden_agg$df_doodoorz, overlijden_with_matched_agg$df_doodoorz,
                    by = c('doodsoorzaak', 'cohort'), all = T)
#df_doodoorz[, diff := n_total_alle_overleden - n_total_geselecteerde_overleden]
#df_doodoorz[, share := round(diff / n_total_alle_overleden, 2)]

# Save
path <- glue('./output/iteration_1/')
wb <- openxlsx::loadWorkbook(glue('{path}all_output.xlsx'))
openxlsx::addWorksheet(wb, 'df_agg')
openxlsx::writeData(wb, 'df_agg', df_agg)

openxlsx::addWorksheet(wb, 'df_doodoorz')
openxlsx::writeData(wb, 'df_doodoorz', df_doodoorz)

openxlsx::saveWorkbook(wb, glue('{path}all_output.xlsx'), overwrite = T)
