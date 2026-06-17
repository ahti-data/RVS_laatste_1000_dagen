# Project: Laatste 1000 dagen
# Author: Marco Griep
# Goal: Run external quality checks
# Output: filled in quality checks excel in data/quality_checks
# Last edited: 12 March 2026

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

quality_checks_dt <- load_clean_quality_checks_dt()

dt_overlijden_with_matched <- as.data.table(arrow::read_parquet("data/raw/overlijden_with_matched.parquet"))[died == "Overleden"]

print(quality_checks_dt[, .N, by = dataset])

#### functions ####
find_2023_avgcosts_and_npatients <- function(filepath_output, filepath_binned, cost_col, cost_col_gebruikt, dataset_name) {
  dt_wvp_output <- as.data.table(read_xlsx("output/iteration_1/all_output.xlsx", sheet = "wijkverpleging"))
  dt_wvp_binned <- as.data.table(arrow::read_parquet("data/processed/wvpkosten_monthly.parquet"))
  
  ## calculate
  total_n_patients_internal <- sum(dt_wvp_output[
    cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "gebruikt_bedragzvwwvp_n_totaal_gebruikers" & grepl("Palliatief", doodsoorzaak)
  ]$value)
  
  total_costs_internal <- sum(dt_wvp_output[
    cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "bedragzvwwvp_sum_totaal_groep" & grepl("Palliatief", doodsoorzaak)
  ]$value)
  
  
  quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WIJKVERPLEGING", ':='(
    internal.value = total_n_patients_internal,
    note.internal = "berekend voor totaal 1000 dagen"
  )
  ]
  
  quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WIJKVERPLEGING", ':='(
    internal.value = total_costs_internal / total_n_patients_internal,
    note.internal = "berekend voor totaal 1000 dagen"
  )
  ]
  total_n_patients_internal_from_binned <- uniqueN(dt_wvp_binned[
    t >= -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_bedragzvwwvp == 1
  ]$rinpersoon)
  
  total_costs_internal_from_binned <- sum(dt_wvp_binned[
    t >= -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_bedragzvwwvp == 1
  ]$bedragzvwwvp)
  
  row_copy <- copy(quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WIJKVERPLEGING"])
  row_copy_2 <- copy(quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WIJKVERPLEGING"])
  
  row_copy[, ':='(
    subject = "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WIJKVERPLEGING (from intermediate step)",
    internal.value = total_n_patients_internal_from_binned,
    note.internal = "calculated from intermediate step"
  )]
  
  row_copy_2[, ':='(
    subject = "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WIJKVERPLEGING (from intermediate step)",
    internal.value = total_costs_internal_from_binned / total_n_patients_internal_from_binned,
    note.internal = "calculated from intermediate step"
  )]
  
  quality_checks_dt <- rbind(quality_checks_dt, row_copy, row_copy_2)
  
  rm(dt_wvp_binned, dt_wvp_output)
  gc()
}

#### dt_overlijden ####
# clean for total overleden personen
quality_checks_dt[grepl("Total overleden personen", subject), type := "n_deaths"]
quality_checks_dt[type == "n_deaths", doodsoorzaak := "all"]
quality_checks_dt[grepl("palliatief dementie", subject) & type == "n_deaths", doodsoorzaak := "Palliatief dementie"]
quality_checks_dt[grepl("overig/uitwendig", subject) & type == "n_deaths", doodsoorzaak := "Overig/Uitwendige oorzaken"]
quality_checks_dt[grepl("kanker", subject) & type == "n_deaths", doodsoorzaak := "Palliatief kanker"]
quality_checks_dt[grepl("luchtweg", subject) & type == "n_deaths", doodsoorzaak := "Palliatief luchtwegaandoening"]
quality_checks_dt[grepl("hart & vaatziekten", subject) & type == "n_deaths", doodsoorzaak := "Palliatief hart & vaatziekten"]
quality_checks_dt[grepl("overig ", subject) & type == "n_deaths", doodsoorzaak := "Palliatief overig"]


find_n_deaths <- function(yr, grp, doodsoorz, overlijden_dt) {
  if (yr == 2020) {
    yr <- 2019
  }
  
  if (doodsoorz == "all"){
    sum <- nrow(overlijden_dt[
      cohort == yr,
    ])
  } else if (doodsoorz == "Overig/Uitwendige oorzaken") {
    sum <- nrow(overlijden_dt[
      cohort == yr &
        doodsoorzaak %in% c("Overig", "Uitwendige oorzaken")
    ])
  }else {
    sum <- nrow(overlijden_dt[
      cohort == yr &
      doodsoorzaak %in% doodsoorz
    ])
  }
  return(sum)
}

quality_checks_dt[type == "n_deaths", internal.value := mapply(find_n_deaths, year, group, doodsoorzaak, MoreArgs = list(overlijden_dt = dt_overlijden_with_matched))]
quality_checks_dt[type == "n_deaths" & year == 2020, note.internal := "derived from 2019 instead of 2020"]

#### WLZ ####
dt_wlz_output <- as.data.table(read_xlsx("output/iteration_1/all_output.xlsx", sheet = "wlz"))
dt_wlz_binned <- as.data.table(arrow::read_parquet("data/processed/wlzkosten_monthly.parquet"))

## aggregate subjects to usable versions
wlz_aantal_patienten_subjects <- c(
  "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ MPT",
  "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ VPT",
  "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ ZZP"
)

wlz_gemiddelde_kosten_subjects <- c(
  "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WLZ MPT",
  "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WLZ VPT",
  "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WLZ ZZP"
)

pakketen <- c("MPT", "VPT", "ZZP")

total_sum_costs_external <- sum(sapply(pakketen, function(p){
  avg <- quality_checks_dt[subject == paste0("Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WLZ ", p)]$external.value
  n <- quality_checks_dt[subject == paste0("Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ ", p)]$external.value
  return(avg * n)
}))

total_sum_n_external <- sum(sapply(pakketen, function(p){
  n <- quality_checks_dt[subject == paste0("Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ ", p)]$external.value
  return(n)
}))

quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WLZ MPT", ':='(
  subject = "gemiddelde kosten palliatieve patienten in 2023 binnen de wlz",
  external.value = total_sum_costs_external / total_sum_n_external
)]

quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ MPT", ':='(
  subject = "Totaal Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ",
external.value = total_sum_n_external)]

quality_checks_dt <- quality_checks_dt[!subject %in% c(wlz_aantal_patienten_subjects, wlz_gemiddelde_kosten_subjects)]

## calculate
total_n_patients_internal <- sum(dt_wlz_output[
  cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "gebruikt_bedragwlzzin_n_totaal_gebruikers" & grepl("Palliatief", doodsoorzaak)
]$value)

total_costs_internal <- sum(dt_wlz_output[
  cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "bedragwlzzin_sum_totaal_groep" & grepl("Palliatief", doodsoorzaak)
]$value)


quality_checks_dt[subject == "Totaal Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ", ':='(
                      internal.value = total_n_patients_internal,
                      note.internal = "berekend voor totaal 1000 dagen"
                      )]

quality_checks_dt[subject == "gemiddelde kosten palliatieve patienten in 2023 binnen de wlz", ':='(
                      internal.value = total_costs_internal / total_n_patients_internal,
                      note.internal = "berekend voor totaal 1000 dagen"
                      )]
row_copy <- copy(quality_checks_dt[subject == "Totaal Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ"])
row_copy_2 <- copy(quality_checks_dt[subject == "gemiddelde kosten palliatieve patienten in 2023 binnen de wlz"])

total_n_patients_internal_from_binned <- uniqueN(dt_wlz_binned[
  t > -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_bedragwlzzin == 1
  ]$rinpersoon)

total_costs_internal_from_binned <- sum(dt_wlz_binned[
  t > -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_bedragwlzzin == 1
]$bedragwlzzin)

row_copy[, ':='(
  subject = "Totaal Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WLZ (from intermediate step)",
  internal.value = total_n_patients_internal_from_binned,
  note.internal = "calculated from intermediate step"
)]

row_copy_2[, ':='(
  subject = "gemiddelde kosten palliatieve patienten in 2023 binnen de wlz (from intermediate step)",
  internal.value = total_costs_internal_from_binned / total_n_patients_internal_from_binned,
  note.internal = "calculated from intermediate step"
)]

quality_checks_dt <- rbind(quality_checks_dt, row_copy, row_copy_2)


## compare to 2017 van Dijk
dt_wlz_binned_lastyear <- dt_wlz_binned[cohort == 2019 & died == "Overleden" & t>=-1, .(sum(bedragwlzzin, na.rm=T)), by = rinpersoon]
mean(dt_wlz_binned_lastyear$V1)


rm(dt_wlz_binned, dt_wlz_output)
gc()

#### WVP ####
dt_wvp_output <- as.data.table(read_xlsx("output/iteration_1/all_output.xlsx", sheet = "wijkverpleging"))
dt_wvp_binned <- as.data.table(arrow::read_parquet("data/processed/wvpkosten_monthly.parquet"))


## calculate
total_n_patients_internal <- sum(dt_wvp_output[
  cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "gebruikt_bedragzvwwvp_n_totaal_gebruikers" & grepl("Palliatief", doodsoorzaak)
]$value)

total_costs_internal <- sum(dt_wvp_output[
  cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "bedragzvwwvp_sum_totaal_groep" & grepl("Palliatief", doodsoorzaak)
]$value)


quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WIJKVERPLEGING", ':='(
  internal.value = total_n_patients_internal,
  note.internal = "berekend voor totaal 1000 dagen"
)
]

quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WIJKVERPLEGING", ':='(
                      internal.value = total_costs_internal / total_n_patients_internal,
                      note.internal = "berekend voor totaal 1000 dagen"
                      )
                    ]
total_n_patients_internal_from_binned <- uniqueN(dt_wvp_binned[
  t >= -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_bedragzvwwvp == 1
]$rinpersoon)

total_costs_internal_from_binned <- sum(dt_wvp_binned[
  t >= -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_bedragzvwwvp == 1
]$bedragzvwwvp)

row_copy <- copy(quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WIJKVERPLEGING"])
row_copy_2 <- copy(quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WIJKVERPLEGING"])

row_copy[, ':='(
  subject = "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen WIJKVERPLEGING (from intermediate step)",
  internal.value = total_n_patients_internal_from_binned,
  note.internal = "calculated from intermediate step"
)]

row_copy_2[, ':='(
  subject = "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen WIJKVERPLEGING (from intermediate step)",
  internal.value = total_costs_internal_from_binned / total_n_patients_internal_from_binned,
  note.internal = "calculated from intermediate step"
)]

quality_checks_dt <- rbind(quality_checks_dt, row_copy, row_copy_2)

rm(dt_wvp_binned, dt_wvp_output)
gc()
#### MSZ eerstelijns ####
dt_MSZ_1 <- as.data.table(r_parquet_get_dt("H:/laatste_1000_dagen/data/processed/msz_prestatie_1000_dagen_1.parquet"))

quality_checks_dt[subject == "Percentage palliatieve patiënten in 2023 met IC-opname30 dagen voor overlijden", ':='(
                       internal.value = mean(dt_MSZ_1[died == "Overleden" & grepl("Palliatief", doodsoorzaak) & t == -1 & cohort == 2023]$heeft_add_on_ic)
                       )]

quality_checks_dt[subject == "Aantal palliatieve patiënten 18+ die in 2017 de maand voor overlijden op IC werden opgenomen", ':='(
  internal.value = uniqueN(dt_MSZ_1[died == "Overleden" & grepl("Palliatief", doodsoorzaak) & t == -1 & cohort == 2019 & heeft_add_on_ic == 1]$rinpersoon),
  note.internal = "based from 2019"
)]

#### MSZ tweedelijns ####



#### MSZ Totaal ####
dt_mszT_output <- as.data.table(read_xlsx("output/iteration_1/all_output.xlsx", sheet = "msz_prestaties"))
dt_mszT_binned <- as.data.table(arrow::read_parquet("data/processed/vektmszkosten_monthly.parquet"))


## calculate
total_n_patients_internal <- sum(dt_mszT_output[
  cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "gebruikt_vektmszvergoedbedragzvw_n_totaal_gebruikers" & grepl("Palliatief", doodsoorzaak)
]$value)

total_costs_internal <- sum(dt_mszT_output[
  cohort == 2023 & bin_size == "1000days" & died == "Overleden" & variable == "vektmszvergoedbedragzvw_sum_totaal_groep" & grepl("Palliatief", doodsoorzaak)
]$value)


quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen MSZ", ':='(
  internal.value = total_n_patients_internal,
  note.internal = "berekend voor totaal 1000 dagen"
)]

quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen MSZ", ':='(
  internal.value = total_costs_internal / total_n_patients_internal,
  note.internal = "berekend voor totaal 1000 dagen"
)]

total_n_patients_internal_from_binned <- uniqueN(dt_mszT_binned[
  t >= -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_vektmszvergoedbedragzvw == 1
]$rinpersoon)

total_costs_internal_from_binned <- sum(dt_mszT_binned[
  t >= -month(gbadatumoverlijden) & cohort == 2023 & died == "Overleden" & grepl("Palliatief", doodsoorzaak) & gebruikt_vektmszvergoedbedragzvw == 1
]$vektmszvergoedbedragzvw)

row_copy <- copy(quality_checks_dt[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen MSZ"])
row_copy_2 <- copy(quality_checks_dt[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen MSZ"])

row_copy[, ':='(
  subject = "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen MSZ (from intermediate step)",
  internal.value = total_n_patients_internal_from_binned,
  note.internal = "calculated from intermediate step"
)]

row_copy_2[, ':='(
  subject = "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen MSZ (from intermediate step)",
  internal.value = total_costs_internal_from_binned / total_n_patients_internal_from_binned,
  note.internal = "calculated from intermediate step"
)]

quality_checks_dt <- rbind(quality_checks_dt, row_copy, row_copy_2)

## compare to 2017 van Dijk
dt_mszT_binned_lastyear <- dt_mszT_binned[cohort == 2019 & died == "Overleden" & t>=-1, .(sum(vektmszvergoedbedragzvw + vektmszvergoedbedragav, na.rm=T)), by = rinpersoon]
mean(dt_mszT_binned_lastyear$V1)

rm(dt_mszT_binned, dt_mszT_output)
gc()

#### ZVW ####
quality_checks_dt <- load_clean_quality_checks_dt()
quality_checks_dt_zvw_overlijden <- copy(quality_checks_dt)[dataset %in% c("WIJKVERPLEGING", "ZVW", "HUISARTSDECLTAB", "MSZ_TOTAAL")]
quality_checks_dt_zvw_doodsoorzaak <- copy(quality_checks_dt)[dataset %in% c("WIJKVERPLEGING", "ZVW", "HUISARTSDECLTAB", "MSZ_TOTAAL")]
# 2023
{
dt_zvw_2023 <- as.data.table(r_parquet_get_dt("H:/laatste_1000_dagen/data/processed/zvw_2023.parquet"))
dt_zvw_merged_overlijden <- merge(
  dt_overlijden_with_matched,
  dt_zvw_2023,
  all.x = T,
  by = "rinpersoon"
)


quality_checks_dt_zvw_overlijden[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen FARMACIE", ':='(
                      internal.value = dt_zvw_merged_overlijden[
                        gebruikt_zvwkfarmacie == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
                        .(uniqueN(rinpersoon))],
                      note.internal = "calculated from 2023 zvw"
                      )]

quality_checks_dt_zvw_overlijden[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen FARMACIE", ':='(
  internal.value = dt_zvw_merged_overlijden[
    gebruikt_zvwkfarmacie == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(mean(zvwkfarmacie))],
  note.internal = "calculated from 2023 zvw"
)]

quality_checks_dt_zvw_overlijden[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen MSZ", ':='(
  internal.value = dt_zvw_merged_overlijden[
    gebruikt_zvwkziekenhuis == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(uniqueN(rinpersoon))],
  note.internal = "calculated from 2023 zvw"
)]

quality_checks_dt_zvw_overlijden[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen MSZ", ':='(
  internal.value = dt_zvw_merged_overlijden[
    gebruikt_zvwkziekenhuis == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(mean(zvwkziekenhuis))],
  note.internal = "calculated from 2023 zvw"
)]

quality_checks_dt_zvw_overlijden[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen HUISARTS", ':='(
  internal.value = dt_zvw_merged_overlijden[
    gebruikt_zvwkhuisarts == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(uniqueN(rinpersoon))],
  note.internal = "calculated from 2023 zvw"
)]

quality_checks_dt_zvw_overlijden[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen HUISARTS", ':='(
  internal.value = dt_zvw_merged_overlijden[
    gebruikt_zvwkhuisarts == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(mean(zvwkhuisarts))],
  note.internal = "calculated from 2023 zvw"
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

dt_zvw_merged_doodsoorzaak <- merge(
  df_overlijden,
  dt_zvw_2023,
  all.x = T,
  by = "rinpersoon"
)

quality_checks_dt_zvw_doodsoorzaak[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen FARMACIE", ':='(
  internal.value = dt_zvw_merged_doodsoorzaak[
    gebruikt_zvwkfarmacie == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(uniqueN(rinpersoon))],
  note.internal = "calculated from 2023 zvw (all deaths)"
)]

quality_checks_dt_zvw_doodsoorzaak[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen FARMACIE", ':='(
  internal.value = dt_zvw_merged_doodsoorzaak[
    gebruikt_zvwkfarmacie == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(mean(zvwkfarmacie))],
  note.internal = "calculated from 2023 zvw (all deaths)"
)]

quality_checks_dt_zvw_doodsoorzaak[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen MSZ", ':='(
  internal.value = dt_zvw_merged_doodsoorzaak[
    gebruikt_zvwkziekenhuis == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(uniqueN(rinpersoon))],
  note.internal = "calculated from 2023 zvw (all deaths)"
)]

quality_checks_dt_zvw_doodsoorzaak[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen MSZ", ':='(
  internal.value = dt_zvw_merged_doodsoorzaak[
    gebruikt_zvwkziekenhuis == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(mean(zvwkziekenhuis))],
  note.internal = "calculated from 2023 zvw (all deaths)"
)]

quality_checks_dt_zvw_doodsoorzaak[subject == "Aantal palliatieve patienten dat in 2023 zorg gebruikten binnen HUISARTS", ':='(
  internal.value = dt_zvw_merged_doodsoorzaak[
    gebruikt_zvwkhuisarts == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(uniqueN(rinpersoon))],
  note.internal = "calculated from 2023 zvw (all deaths)"
)]

quality_checks_dt_zvw_doodsoorzaak[subject == "Gemiddelde kosten per gebruiker van palliatieve patienten in 2023 binnen HUISARTS", ':='(
  internal.value = dt_zvw_merged_doodsoorzaak[
    gebruikt_zvwkhuisarts == 1 & cohort == 2023 & grepl("Palliatief", doodsoorzaak), 
    .(mean(zvwkhuisarts))],
  note.internal = "calculated from 2023 zvw (all deaths)"
)]

quality_checks_dt_zvw <- rbindlist(list(quality_checks_dt_zvw_overlijden, quality_checks_dt_zvw_doodsoorzaak))
}

# 2019
{
  dt_zvw_2019 <- as.data.table(r_parquet_get_dt("H:/laatste_1000_dagen/data/processed/zvw_2019.parquet"))
  
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
  
  df_overlijden <- df_overlijden[year_of_death == 2019]
  df_overlijden[,':='(cohort = 2019)]
  
  dt_zvw_merged_doodsoorzaak <- merge(
    df_overlijden,
    dt_zvw_2019,
    all.x = T,
    by = "rinpersoon"
  )
  
  mean(dt_zvw_merged_doodsoorzaak$zvwkfarmacie, na.rm = T)
  
  # compare to vandijk 2017
  dt_zvw_binned_lastyear <- dt_wlz_binned[cohort == 2019 & died == "Overleden" & t>=-1, .(sum(bedragwlzzin, na.rm=T)), by = rinpersoon]
  mean(dt_wlz_binned_lastyear$V1)
  
}
#### add difference cols and save ####
bound_percentage = 0.05

quality_checks_dt[, ':='(
  lower.bound = (1-bound_percentage) * external.value,
  upper.bound = (1+bound_percentage) * external.value
  )
]
quality_checks_dt[,':='(
  difference = internal.value - external.value,
  relative.difference = internal.value / external.value - 1,
  check_passed = ifelse(internal.value > lower.bound & internal.value < upper.bound, TRUE, FALSE)
  ) 
]

openxlsx2::write_xlsx(quality_checks_dt, "data/quality_checks/Filled_external.xlsx")




