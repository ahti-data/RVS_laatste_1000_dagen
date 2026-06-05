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

#### Add other demographic characteristics ####
df_cohorts <- r_parquet_get_dt('./data/raw/overlijden_with_matched.parquet')
df_cohorts[, sample_id := .I]
unique_rins <- unique(df_cohorts$rinpersoon)
n_total <- nrow(df_cohorts)

# Add seswoa
dt_seswoa <- r_parquet_get_dt("data/raw/seswoa.parquet")

df_cohorts <- merge(
  df_cohorts,
  dt_seswoa,
  by.x = c("rinpersoon", "cohort"),
  by.y = c("rinpersoon", "year"),
  all.x = T
)

rm(dt_seswoa)
gc()

# Add migratie
dt_migratie <- r_parquet_get_dt("data/raw/migratie_achtergrond.parquet")

df_cohorts <- merge(
  df_cohorts,
  dt_migratie,
  by.x = c("rinpersoon"),
  by.y = c("rinpersoon"),
  all.x = T
)

rm(dt_migratie)
gc()

# Add leeftijd & geslacht
dt_gbapersoon <- r_parquet_get_dt("data/raw/gbapersoon.parquet")

df_cohorts <- merge(
  df_cohorts,
  dt_gbapersoon,
  by.x = c("rinpersoon"),
  by.y = c("rinpersoon"),
  all.x = T
)

df_cohorts[, month_of_birth := as.character(month_of_birth)]
df_cohorts[nchar(month_of_birth) == "1", month_of_birth := paste0("0", as.character(month_of_birth))]
df_cohorts[, birthdate := as.Date(paste0(year_of_birth, month_of_birth, "01"), format = "%Y%m%d")]
df_cohorts[, age_cat := floor(round(as.numeric((gbadatumoverlijden - birthdate) / 365.25)) / 10)]
df_cohorts[age_cat == 1, age_cat := 2] # 18-29
df_cohorts[age_cat >= 9, age_cat := 9] # 90+

df_cohorts[, ':='(
  year_of_birth = NULL, month_of_birth = NULL, birthdate = NULL
)]

rm(dt_gbapersoon)
gc()

# Add inkomen, huishoudsamenstelling, burgerlijke staat, and hbopl
dt_stapeling_31_12 <- r_parquet_get_dt("data/raw/stapeling_31_12.parquet")

df_cohorts <- merge(
  df_cohorts,
  dt_stapeling_31_12,
  by.x = c("rinpersoon", "cohort"),
  by.y = c("rinpersoon", "year"),
  all.x = T
)

rm(dt_stapeling_31_12)
gc()

dt_stapeling_01_01 <- r_parquet_get_dt("data/raw/stapeling_01_01.parquet")

df_cohorts <- merge(
  df_cohorts,
  dt_stapeling_01_01,
  by.x = c("rinpersoon", "cohort"),
  by.y = c("rinpersoon", "year"),
  all.x = T
)

rm(dt_stapeling_01_01)
gc()

# Add WLZ categorisation
dt_wlz <- load_dataset(2015:2023, "WLZZINTAB", 
                       cols = c("rinpersoon", "beginwlzzin", "bedragwlzzin"),
                       rinpersoon_chunk = unique(df_cohorts$rinpersoon))

df_cohorts <- calculate_costs_by_bin_size(
  dt_wlz,
  df_cohorts,
  cost_columns = "bedragwlzzin",
  cost_date_col = "beginwlzzin",
  bin_size = "wlz_categorisation",
  aggregate_groupby_cols = names(df_cohorts)
)

# create categories
df_cohorts <- df_cohorts[, .(wlz_start_period = fcase(
  any(t == -1 & bedragwlzzin > 0) & !any(t < -1 & bedragwlzzin > 0), "WLZ start 0-3 maanden voor overlijden",
  any(t == -2 & bedragwlzzin > 0) & !any(t < -2 & bedragwlzzin > 0), "WLZ start 3-12 maanden voor overlijden",
  any(t == -3 & bedragwlzzin > 0) & !any(t < -3 & bedragwlzzin > 0), "WLZ start 12 - 33 maanden voor overlijden",
  any(t == -4 & bedragwlzzin > 0) , "WLZ 1000 dagen",
  !any(bedragwlzzin > 0) , "Nooit WLZ")),
  by = c(setdiff(names(df_cohorts), c("t", "bedragwlzzin")))]

rm(dt_wlz)
gc()

# Location of overlijden
# dt_doodoorz <- r_parquet_get_dt("data/raw/doodoorz.parquet")
# 
# df_cohorts <- merge(
#   df_cohorts,
#   dt_doodoorz[, .SD, .SDcols = c("rinpersoon", "nndlocationcode")],
#   by.x = c("rinpersoon"),
#   by.y = c("rinpersoon"),
#   all.x=T
# )
# df_cohorts[died == "In leven", nndlocationcode := "In leven"]

df_cohorts[is.na(seswoa_cat), seswoa_cat := 'Onbekend']
df_cohorts[is.na(inkomen_klasse), inkomen_klasse := 'Onbekend']

df_cohorts[, burgstaat := as.character(burgstaat)]
df_cohorts[, stedgem := as.character(stedgem)]
df_cohorts[, huishoudsamenstelling := as.character(huishoudsamenstelling)]
df_cohorts[is.na(burgstaat), burgstaat := 'Onbekend']
df_cohorts[is.na(stedgem), stedgem := 'Onbekend']
df_cohorts[is.na(huishoudsamenstelling), huishoudsamenstelling := 'overig_onbekend_hh']

stopifnot(nrow(df_cohorts) == n_total)
arrow::write_parquet(df_cohorts, './data/raw/overlijden_with_matched_add_demog.parquet')
rm(df_cohorts)
gc()
