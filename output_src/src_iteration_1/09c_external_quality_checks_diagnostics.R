# Project: Laatste 1000 dagen
# Author: Marco Griep & Stanislav Avdeev
# Goal: Run external quality checks
# Output: filled in quality checks excel in data/quality_checks
# Last edited: 17 March 2026

rm(list = ls())
gc()
source("src/00_inputs.R")
options(scipen = 999)


library(openxlsx2)


#### load and clean data ####
load_clean_quality_checks_dt <- function() {
  quality_checks_dt <- format_data(openxlsx::read.xlsx(
    "H:/laatste_1000_dagen/data/quality_checks/Kwaliteitschecks_iteratie1_20260311.xlsx", 
    sheet = "Input_outside_environment"), rin_num = F)
  
  
  cols_quality_checks <- c("categorie", "subject", "source.(link)", "region", 
                           "year", "group", "measure", "external.value", 
                           "lower.bound", "upper.bound", "internal.value", 
                           "difference", "relative.difference", "note")
  
  cols_to_drop <- c("source.(link)", "categorie")
  
  # drop unnecessary cols 
  quality_checks_dt[, (cols_to_drop) := NULL]
  
  # NUmeric
  quality_checks_dt[, ':='(
    external.value = as.numeric(external.value),
    internal.value = as.numeric(internal.value)
  )]
  
  quality_checks_dt[grepl("WLZ", subject), dataset := "WLZ"]
  quality_checks_dt[grepl("WIJKVERPLEGING", subject), dataset := "WVP"]
  quality_checks_dt[grepl("Total overleden personen", subject), dataset := "dt_overlijden"]
  quality_checks_dt[grepl("FARMACIE", subject), dataset := "ZVW"]
  quality_checks_dt[grepl("HUISARTS|huisarts", subject), dataset := "HUISARTSDECLTAB"]
  quality_checks_dt[grepl("heup|eerstelijn|IC|AAA", subject), dataset := "MSZ_PRESTATIES_EERSTELIJNS"]
  quality_checks_dt[grepl("tweedelijn", subject), dataset := "MSZ_PRESTATIES_TWEEDELIJNS"]
  quality_checks_dt[grepl("ADD-ON", subject), dataset := "ADD-ON"]
  quality_checks_dt[grepl("MSZ", subject), dataset := "MSZ_TOTAAL"]
}

###
quality_checks_dt <- load_clean_quality_checks_dt()
quality_checks_dt_msz <- quality_checks_dt[dataset %in% c(
  'MSZ_PRESTATIES_TWEEDELIJNS', 'MSZ_PRESTATIES_EERSTELIJNS' 
)]


## calculate from doodsoorzaak
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

df_overlijden[, date_of_birth := as.Date(ISOdate(year_of_birth, month_of_birth, 1))]
df_overlijden[, age_at_death := as.numeric((gbadatumoverlijden - date_of_birth) / 365.25)]
df_overlijden <- df_overlijden[age_at_death >= 18]
df_overlijden <- df_overlijden[, .(rinpersoon, gbadatumoverlijden, year_of_death, 
                                   geslacht, year_of_birth, age_at_death, doodsoorzaak)]

df_overlijden <- df_overlijden[year_of_death == 2023]
df_overlijden[,':='(cohort = 2023)]


msz_eerste_2017 <- r_parquet_get_dt("./data/processed/msz_prestatie_2017.parquet")
# msz_eerste_2017 <- merge(
#   msz_eerste_2017,
#   df_overlijden,
#   all.x = T,
#   by = "rinpersoon"
# )

msz_tweede <- r_parquet_get_dt("./data/processed/msz_activiteiten_2017.parquet")

quality_checks_dt_msz[
  subject == "Gemiddeld aantal AAA-operaties per jaar, in periode 2013-2017", ':='(
  internal.value = msz_eerste_2017[,
    .(sum(heeft_aaa_operatie))]
)]

quality_checks_dt_msz[
  subject == "Totaal heupfracturen per jaar, ongeacht overlijden en leeftijd", ':='(
    internal.value = msz_eerste_2017[
      heeft_heup_totaal == 1, 
      .(uniqueN(rinpersoon))]
  )]

quality_checks_dt_msz[
  subject == "Totaal heupprotheses per jaar, ongeacht overlijden en leeftijd", ':='(
    internal.value = msz_eerste_2017[
      heeft_heup_prothese == 1, 
      .(uniqueN(rinpersoon))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor eerstelijn diagnostische activiteiten (ZPK4) in 2017", ':='(
    internal.value = msz_eerste_2017[,
      .(sum(heeft_eerstelijn_zpk_4))]
  )]

quality_checks_dt_msz[
  subject == "Kosten voor eerstelijn diagnostische activiteiten (ZPK4) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
      .(sum(kosten_eerstelijn_zpk_4))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor eerstelijn Beeldvormende diagnostiek (ZPK7) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
      .(sum(heeft_eerstelijn_zpk_7))]
  )]

quality_checks_dt_msz[
  subject == "Kosten voor eerstelijn Beeldvormende diagnostiek (ZPK7) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
      .(sum(kosten_eerstelijn_zpk_7))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor eerstelijn Klinische chemie & haematologie (ZPK8) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
      .(sum(heeft_eerstelijn_zpk_8))]
  )]

quality_checks_dt_msz[
  subject == "Kosten voor eerstelijn Klinische chemie & haematologie (ZPK8) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
                                .(sum(kosten_eerstelijn_zpk_8))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor eerstelijn Microbiologie & parasitologie (ZPK9) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
                                .(sum(heeft_eerstelijn_zpk_9))]
  )]

quality_checks_dt_msz[
  subject == "Kosten voor eerstelijn Microbiologie & parasitologie (ZPK9) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
                                .(sum(kosten_eerstelijn_zpk_9))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor eerstelijn Pathologie (ZPK10) ) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
                                .(sum(heeft_eerstelijn_zpk_10))]
  )]

quality_checks_dt_msz[
  subject == "Kosten voor eerstelijn Pathologie (ZPK10)  in 2017", ':='(
    internal.value = msz_eerste_2017[, 
                                .(sum(kosten_eerstelijn_zpk_10))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor tweedelijn overige diagnostiek (ZPK8+10+11) in 2017", ':='(
    external.value = 100000,
    internal.value = msz_eerste_2017[, 
                                .(sum(heeft_overig_tweedelijn_zpk_8_10_11))]
  )]

quality_checks_dt_msz[
  subject == "Kosten voor tweedelijn overige diagnostiek (ZPK8+10+11) in 2017", ':='(
    internal.value = msz_eerste_2017[, 
                                .(sum(kosten_overig_tweedelijn_zpk_8_10_11))]
  )]

# 2019
msz_eerste_2019 <- r_parquet_get_dt("./data/processed/msz_prestatie_2019.parquet")
quality_checks_dt_msz[
  subject == "Totaal aantal IC-opnamen in 2019, ongeacht overlijden en leeftijd ", ':='(
    internal.value = msz_eerste_2019[heeft_add_on_ic > 0, 
                                     .(uniqueN(rinpersoon))]
  )]

msz_eerste_2023 <- r_parquet_get_dt("./data/processed/msz_prestatie_2023.parquet")
quality_checks_dt_msz[
  subject == "Totaal aantal IC-opnamen in 2023, ongeacht overlijden en leeftijd ", ':='(
    internal.value = msz_eerste_2023[heeft_add_on_ic > 0, 
                                     .(uniqueN(rinpersoon))]
  )]

# Tweedelijn
quality_checks_dt_msz[
  subject == "Aantal prestaties voor tweedelijn diagnostische activiteiten (ZPK4) in 2017", ':='(
    internal.value = msz_tweede[, 
                                .(sum(heeft_tweedelijn_zpk_4))]
  )]

quality_checks_dt_msz[
  subject == "Aantal prestaties voor tweedelijn Beeldvormende diagnostiek (ZPK7) in 2017", ':='(
    internal.value = msz_tweede[, 
                                .(sum(heeft_tweedelijn_zpk_7))]
  )]

msz_tweede[, heeft_tweedelijn_zpk_8_10_11 := rowSums(.SD, na.rm = T), .SDcols = c(
  'heeft_tweedelijn_zpk_8', 'heeft_tweedelijn_zpk_10', 'heeft_tweedelijn_zpk_11'
)]
quality_checks_dt_msz[
  subject == "Aantal prestaties voor tweedelijn overige diagnostiek (ZPK8+10+11) in 2017", ':='(
    internal.value = msz_tweede[, 
                                .(sum(heeft_tweedelijn_zpk_8_10_11))]
  )]


# By month of death
msz_eerste_monthly <- r_parquet_get_dt(
  "./data/processed/msz_prestatie_1000_dagen.parquet",
                                       columns = c(
                                         'rinpersoon', 't', 'cohort', 'died', 'doodsoorzaak', 'heeft_add_on_ic'))
msz_eerste_monthly <- merge(msz_eerste_monthly, 
                            df_overlijden[, .(rinpersoon, age_at_death)],
                            by = 'rinpersoon',
                            all.x = T)
quality_checks_dt_msz[
  subject == "Percentage palliatieve patiënten in 2023 met IC-opname30 dagen voor overlijden", ':='(
    internal.value = msz_eerste_monthly[cohort == 2023 & 
                                          died == 'Overleden' &
                                          t == -1 &
                                          age_at_death >= 18 &
                                          grepl("Palliatief", doodsoorzaak), 
                                .(mean(heeft_add_on_ic))]
  )]

bound_percentage = 0.05

quality_checks_dt_msz[, ':='(
  lower.bound = (1-bound_percentage) * external.value,
  upper.bound = (1+bound_percentage) * external.value
)
]

quality_checks_dt_msz[,':='(
  difference = internal.value - external.value,
  relative.difference = internal.value / external.value - 1,
  check_passed = ifelse(internal.value > lower.bound & 
                          internal.value < upper.bound, TRUE, FALSE)
) 
]
View(quality_checks_dt_msz[is.na(check_passed)])
openxlsx2::write_xlsx(quality_checks_dt_msz, "./output/checks.xlsx")
###
