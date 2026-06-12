# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Find 10 random people based on sex and birth year
# Output: A sample of rinpersoon
# Last edited: 3 March 2025

#### initialize ####
rm(list = ls())
gc()
source('./src/00_inputs.R')

# Load death registry
overlijden <- r_parquet_get_dt('./data/raw/overlijden.parquet')

# Load sex and year of birth 
df_gbapersoon <- r_parquet_get_dt('./data/raw/gbapersoon.parquet')
df_overlijden <- merge(overlijden, df_gbapersoon, by = 'rinpersoon', all.x = T)
gc()

# Load cause of death
df_doodoorz <- r_parquet_get_dt("./data/raw/doodoorz.parquet")
df_overlijden <- merge(df_overlijden, df_doodoorz, by = 'rinpersoon', all.x = T)
df_overlijden[is.na(doodsoorzaak), doodsoorzaak := 'Onbekend']
rm(df_doodoorz)
gc()

# Keep only people 18+ at death
df_overlijden[, date_of_birth := as.Date(ISOdate(year_of_birth, month_of_birth, 1))]
df_overlijden[, age_at_death := as.numeric((gbadatumoverlijden - date_of_birth) / 365.25)]
df_overlijden <- df_overlijden[age_at_death >= 18]
df_overlijden <- df_overlijden[, .(rinpersoon, gbadatumoverlijden, year_of_death, 
                                   geslacht, year_of_birth, age_at_death, doodsoorzaak)]

# Load people registered 
df_registered <- r_parquet_get_dt('./data/raw/stapeling_31_12.parquet',
                       columns = c('rinpersoon', 'year'))
df_registered <- merge(df_registered, overlijden, by = 'rinpersoon', all = T)
df_registered <- merge(df_registered, df_gbapersoon, by = 'rinpersoon', all.x = T)
setorder(df_registered, rinpersoon, year)
rm(overlijden, df_gbapersoon)

df_cohorts <- data.table()
for (cohort in c(2019, 2023)) {
  print(cohort)
  
  df_fully_covered <- r_parquet_get_dt(glue(
    './data/raw/fully_covered_{cohort}.parquet'))
  
  # Match pool for the 2023 cohort consists of people alive in 2026 
  # (not yet available, so take everyone alive) and for the 2019, by 2022
  alive <- unique(df_registered[is.na(year_of_death) | year_of_death > cohort + 3], by = 'rinpersoon')
  deceased <- unique(df_overlijden[year_of_death == cohort], by = 'rinpersoon')
  
  # Keep only people who have been continuously registered from 1 January 2020 (2016) to 
  # 31 December 2022 (2019) in NL
  alive <- merge(alive, df_fully_covered, by = 'rinpersoon', all = F)
  deceased <- merge(deceased, df_fully_covered, by = 'rinpersoon', all = F)
  rm(df_fully_covered)
  gc()
  
  # Number of matches
  deceased <- deceased[rep(seq_len(.N), 10)]
  
  pool <- alive[, .(IDs = list(rinpersoon)), by = .(year_of_birth, geslacht)]
  matching_sample <- merge(deceased, pool, all.x = T,
                           by = c('year_of_birth', 'geslacht'))
  rm(alive, pool)
  gc()
  
  set.seed(89345)
  
  # Matching with replacement 
  matching_sample[, ID := {
    x <- IDs[[1]]
    if (length(x) == 0) NA_character_
    else sample(as.character(x), 1L)
  }, by = .I]
  
  cat(glue('\n Share of profiles with no match: 
                 {mean(ifelse(is.na(matching_sample$ID), 1, 0))} \n'))
  cat(glue('\n Number of profiles with no match: 
                 {nrow(matching_sample[is.na(ID)])} \n'))
  
  random_alive <- matching_sample[, .(rinpersoon = as.numeric(ID), 
                                      gbadatumoverlijden,
                                      doodsoorzaak)]
  random_alive <- random_alive[complete.cases(rinpersoon)]
  
  all_ids <- rbindlist(list(
    unique(deceased[, .(rinpersoon, 
                        gbadatumoverlijden, 
                        cohort = cohort,
                        died = 'Overleden',
                        doodsoorzaak)]), 
    random_alive[, .(rinpersoon, gbadatumoverlijden, 
                     cohort = cohort,
                     died = 'In leven',
                     doodsoorzaak)]), 
    use.names = T)
  
  # Drop people with no matches
  #diagnosed_drop <- unique(matching_sample[is.na(ID), .(rinpersoon)])
  #all_ids <- all_ids[!(rinpersoon %in% diagnosed_drop$rinpersoon)]
  
  df_cohorts <- rbindlist(list(df_cohorts, all_ids), use.names = T)
  rm(matching_sample, deceased, random_alive, all_ids)
  gc()
}

setindex(df_cohorts, NULL)
arrow::write_parquet(df_cohorts, './data/raw/overlijden_with_matched.parquet')
rm(df_overlijden, df_registered, df_cohorts)
gc()

