# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Find 10 random people based on sex and birth year
# Output: A sample of rinpersoon
# Last edited: 3 March 2025

#### initialize ####
rm(list = ls())
gc()
source('./src/00_inputs.R')

#### Load death registry ####
df_cohorts <- r_parquet_get_dt('./data/raw/overlijden_with_matched.parquet')
df_cohorts[, sample_id := .I]
unique_rins <- unique(df_cohorts$rinpersoon)
n_total <- nrow(df_cohorts)

#### Add seswoa ####
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

#### Add migratie ####
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

##### Add leeftijd & geslacht ####
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

#### Add inkomen, huishoudsamenstelling, burgerlijke staat, hbopl, and province ####
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

#### Add WLZ categorisation ####
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

##### add huisarts binary var, "used_ACP" for cohort 2023 ####
acp_codes <- c(31244, 31381)

dt_huisarts <- load_dataset(2021:2023, "huisartsdecltab", cols = c("rinpersoon", "hadeclprestatiecode", "hadeclbegindatumprest"), rinpersoon_chunk = unique(df_cohorts$rinpersoon))
dt_huisarts[, hadeclprestatiecode := as.numeric(hadeclprestatiecode)]
dt_huisarts <- dt_huisarts[hadeclprestatiecode %in% c(31244, 31381)]

df_cohorts <- calculate_costs_by_bin_size(
  dt_huisarts,
  df_cohorts,
  cost_columns = "hadeclprestatiecode",
  cost_date_col = "hadeclbegindatumprest",
  bin_size = "2years"
)

# drop overlap cols & huisartsdate col
df_cohorts[, c("bin_end", "bin_start", "t", "hadeclbegindatumprest", "start_temp", "end_temp") := NULL]

# define used acp codes per sample id
df_cohorts <- df_cohorts[, .(
  used_acp_31244_2years = as.integer(31244 %in% unique(hadeclprestatiecode)),
  used_acp_31381_2years = as.integer(31381 %in% unique(hadeclprestatiecode))
), by = setdiff(names(df_cohorts), "hadeclprestatiecode")]

df_cohorts <- df_cohorts[, used_any_acp_2years := as.integer(used_acp_31244_2years | used_acp_31381_2years)]

# set NA for cohort 2019
df_cohorts[cohort == "2019", c("used_acp_31244_2years", "used_acp_31381_2years", "used_any_acp_2years") := NA]

rm(dt_huisarts)
gc()

#### clean cols & write ####
df_cohorts[is.na(seswoa_cat), seswoa_cat := 'Onbekend']
df_cohorts[is.na(inkomen_klasse), inkomen_klasse := 'Onbekend']

df_cohorts[, burgstaat := as.character(burgstaat)]
df_cohorts[, stedgem := as.character(stedgem)]
df_cohorts[, huishoudsamenstelling := as.character(huishoudsamenstelling)]
df_cohorts[is.na(burgstaat), burgstaat := 'Onbekend']
df_cohorts[is.na(stedgem), stedgem := 'Onbekend']
df_cohorts[is.na(huishoudsamenstelling), huishoudsamenstelling := 'overig_onbekend_hh']
df_cohorts[provincie %in% c(NA, "--"), provincie := "onbekend"]


stopifnot(nrow(df_cohorts) == n_total)
arrow::write_parquet(df_cohorts, './data/raw/overlijden_with_matched_add_demog.parquet')
rm(df_cohorts)
gc()
