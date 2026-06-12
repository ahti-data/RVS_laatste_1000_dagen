#### TODO ####
# fix duplication error
# n_totaal_activiteiten is not compared properly earlier in pipeline

#### Initialize ####
rm(list=ls())
gc()

source("src/00_inputs.R")

#### zpk categories distribution ####
dt_mszprest_DBC_agg <- as.data.table(openxlsx2::read_xlsx("data/processed/msz_prestaties_DBC_agg.xlsx"))
dt_mszprest_OZP_agg <- as.data.table(openxlsx2::read_xlsx("data/processed/msz_prestaties_OZP_agg.xlsx"))

# remove problematic categories
dt_mszprest_DBC_agg <- dt_mszprest_DBC_agg[!(vektmszsettingzpk == "9" & bin_size == "monthly")]

OZP_invalid_n_threshold <- 5
OZP_invalid_counts <- as.data.table(openxlsx2::read_xlsx("data/processed/msz_prestaties_OZP_agg.xlsx", sheet = "invalid_counts"))
invalid_groups <- OZP_invalid_counts[n_invalid >= OZP_invalid_n_threshold]
invalid_groups[, id := .I]

for (group_id in unique(invalid_groups$id)) {
  group <- invalid_groups[id == group_id]
  
  dt_mszprest_OZP_agg <- dt_mszprest_OZP_agg[!(
    died == group$died & 
      cohort == group$cohort & 
      zpk_category == group$zpk_category &
      vektmszsettingzpk == group$vektmszsettingzpk &
      bin_size == group$bin_size
    )
  ]
}


# Filter out the rows that do not match the inclusion criteria
dt_mszprest_DBC_agg <- dt_mszprest_DBC_agg[n_instellingen >=3 & share_main_instelling < 0.5]
dt_mszprest_OZP_agg <- dt_mszprest_OZP_agg[n_instellingen >=3 & share_main_instelling < 0.5]

# stack and make output-ready
dt_mszprest_agg <- rbindlist(list(
  dt_mszprest_DBC_agg[, prestatie_type := "DBC"],
  dt_mszprest_OZP_agg[, prestatie_type := "OZP"]
))

# Round to 10
cols_to_round <- c("n_totaal_gebruikers", "n_totaal_declaraties", "n_totaal_population")
dt_mszprest_agg <- dt_mszprest_agg[, (cols_to_round) := lapply(.SD, round, digits=-1), .SDcols = cols_to_round]

# assert no duplicates
assert_that(nrow(dt_mszprest_agg) == nrow(unique(dt_mszprest_agg)))

# save output-ready version
output_cols <- setdiff(names(dt_mszprest_agg), grep("share|n_instelling", names(dt_mszprest_agg), value=T))
openxlsx2::write_xlsx(dt_mszprest_agg, glue("{output_folder}GEENOUTPUT_Achtergrondinformatie/zpk_categorieen_tellingen.xlsx"))
openxlsx2::write_xlsx(dt_mszprest_agg[, .SD, .SDcols = output_cols], glue("{output_folder}zpk_categorieen_tellingen.xlsx"))

#### top50 codes ####
dt_top50codes <- as.data.table(openxlsx2::read_xlsx("data/processed/top50_mszact_codes_by_category.xlsx"))

# filter out rows that don't meet inclusion criteria
dt_top50codes_filtered <- dt_top50codes[
  n_instellingen >= 3 & 
    (n_instellingen_30d >=3 | is.na(n_instellingen_30d)) & 
    (n_instellingen_1000d >=3 | is.na(n_instellingen_1000d)) &
    (n_instellingen_In_leven >=3 | is.na(n_instellingen_In_leven)) &
    (n_instellingen_Overleden >=3 | is.na(n_instellingen_Overleden))
  ]

# round to 10
cols_to_round <- grep("^n_totaal", names(dt_top50codes_filtered), value=T)
dt_top50codes_filtered <- dt_top50codes_filtered[, (cols_to_round) := lapply(.SD, round, digits=-1), .SDcols = cols_to_round]

# fill the columns that should be equal
dt_top50codes_filtered[died == "In_leven", ':='(
  n_totaal_gebruikers_In_leven = n_totaal_gebruikers,
  n_instellingen_In_leven = n_instellingen
)]

dt_top50codes_filtered[died == "Overleden", ':='(
  n_totaal_gebruikers_Overleden = n_totaal_gebruikers,
  n_instellingen_Overleden = n_instellingen
)]

dt_top50codes_filtered[bin_size == "1000d", ':='(
  n_totaal_gebruikers_1000d = n_totaal_gebruikers,
  n_instellingen_1000d = n_instellingen
)]

dt_top50codes_filtered[bin_size == "30d", ':='(
  n_totaal_gebruikers_30d = n_totaal_gebruikers,
  n_instellingen_30d = n_instellingen
)]

# set all cols where n_totaal_gebruikers or n_totaal_declarates = 0, to 0 as well
suffixes <- c("_1000d", "_30d", "_In_leven", "_Overleden")
n_cols <- c("n_totaal_gebruikers")
cols_to_mask <- c("n_instellingen", "n_totaal_gebruikers", "n_totaal_activiteiten")

for (suffix in suffixes) {
  n_cols_suffix <- paste0(n_cols, suffix)
  cols_to_mask_suffix <- paste0(cols_to_mask, suffix)
  
  rows_to_zero <- rowSums(dt_top50codes_filtered[, ..n_cols_suffix] == 0 | is.na(dt_top50codes_filtered[, ..n_cols_suffix])) > 0
  dt_top50codes_filtered[rows_to_zero, (cols_to_mask_suffix) := 0]
}

# save output-ready version
output_cols <- setdiff(names(dt_top50codes_filtered), grep("share|n_instelling", names(dt_top50codes_filtered), value=T))
openxlsx2::write_xlsx(dt_top50codes_filtered, glue("{output_folder}GEENOUTPUT_Achtergrondinformatie/top50_activiteiten.xlsx"))
openxlsx2::write_xlsx(dt_top50codes_filtered[ , .SD, .SDcols = output_cols], glue("{output_folder}top50_activiteiten.xlsx"))


