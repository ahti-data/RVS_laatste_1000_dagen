#### TODO ####
# For top 50 prestaties, drop rows where n_totaal_gebruikers < 10 or NA

#### Initialize ####
rm(list=ls())
gc()

source("src/00_inputs.R")

#### zpk categories distribution ####
dt_mszprest_DBC_agg <- as.data.table(openxlsx2::read_xlsx("data/processed/msz_prestaties_DBC_agg.xlsx"))
dt_mszprest_OZP_agg <- as.data.table(openxlsx2::read_xlsx("data/processed/msz_prestaties_OZP_agg.xlsx"))

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

# for rows where n_users == 0, set costs to 0 as well
dt_mszprest_agg[n_totaal_gebruikers == 0, c("n_totaal_declaraties", "sum_totaal_groep", "median_cost_per_declaratie") := NA]

#NEW: remove rows where sum_totaal_groep == 0
dt_mszprest_agg <- dt_mszprest_agg[sum_totaal_groep != 0 | is.na(sum_totaal_groep)]

# save output-ready version
output_cols <- setdiff(names(dt_mszprest_agg), grep("share|n_instelling", names(dt_mszprest_agg), value=T))
openxlsx2::write_xlsx(dt_mszprest_agg, glue("{output_folder}GEENOUTPUT_Achtergrondinformatie/zpk_categorieen_tellingen.xlsx"))
openxlsx2::write_xlsx(dt_mszprest_agg[, .SD, .SDcols = output_cols], glue("{output_folder}zpk_categorieen_tellingen.xlsx"))

#### top50 activiteiten codes ####
dt_top50codes <- as.data.table(openxlsx2::read_xlsx("data/processed/top50_mszact_codes_by_category.xlsx"))

# filter out rows that don't meet inclusion criteria
dt_top50codes_filtered <- dt_top50codes[
  n_instellingen >= 3 & 
    (n_instellingen_30d >=3 | is.na(n_instellingen_30d)) & 
    (n_instellingen_1000d >=3 | is.na(n_instellingen_1000d)) &
    (n_instellingen_In_leven >=3 | is.na(n_instellingen_In_leven)) &
    (n_instellingen_Overleden >=3 | is.na(n_instellingen_Overleden))
  ]

# fill the columns that should be equal
dt_top50codes_filtered[died == "In_leven", ':='(
  n_totaal_gebruikers_In_leven = n_totaal_gebruikers,
  n_instellingen_In_leven = n_instellingen,
  n_totaal_activiteiten_In_leven = n_totaal_activiteiten
)]

dt_top50codes_filtered[died == "Overleden", ':='(
  n_totaal_gebruikers_Overleden = n_totaal_gebruikers,
  n_instellingen_Overleden = n_instellingen,
  n_totaal_activiteiten_Overleden = n_totaal_activiteiten
  
)]

dt_top50codes_filtered[bin_size == "1000d", ':='(
  n_totaal_gebruikers_1000d = n_totaal_gebruikers,
  n_instellingen_1000d = n_instellingen,
  n_totaal_activiteiten_1000d = n_totaal_activiteiten
  
)]

dt_top50codes_filtered[bin_size == "30d", ':='(
  n_totaal_gebruikers_30d = n_totaal_gebruikers,
  n_instellingen_30d = n_instellingen,
  n_totaal_activiteiten_30d = n_totaal_activiteiten
)]

# round to 10
cols_to_round <- grep("^n_totaal", names(dt_top50codes_filtered), value=T)
dt_top50codes_filtered <- dt_top50codes_filtered[, (cols_to_round) := lapply(.SD, round, digits=-1), .SDcols = cols_to_round]
for (col_name in cols_to_round) {
  dt_top50codes_filtered[get(col_name) == "0", (col_name) := NA]
}

# set all cols where n_totaal_gebruikers or n_totaal_declarates = 0, to 0 as well
suffixes <- c("_1000d", "_30d", "_In_leven", "_Overleden")
n_cols <- c("n_totaal_gebruikers")
cols_to_mask <- c("n_instellingen", "n_totaal_gebruikers", "n_totaal_activiteiten")

for (suffix in suffixes) {
  n_cols_suffix <- paste0(n_cols, suffix)
  cols_to_mask_suffix <- paste0(cols_to_mask, suffix)
  
  rows_to_zero <- rowSums(dt_top50codes_filtered[, ..n_cols_suffix] == 0 | is.na(dt_top50codes_filtered[, ..n_cols_suffix])) > 0
  dt_top50codes_filtered[rows_to_zero, (cols_to_mask_suffix) := NA]
}

# save output-ready version
output_cols <- setdiff(names(dt_top50codes_filtered), grep("share|n_instelling", names(dt_top50codes_filtered), value=T))
openxlsx2::write_xlsx(dt_top50codes_filtered, glue("{output_folder}GEENOUTPUT_Achtergrondinformatie/top50_activiteiten.xlsx"))
openxlsx2::write_xlsx(dt_top50codes_filtered[ , .SD, .SDcols = output_cols], glue("{output_folder}top50_activiteiten.xlsx"))



#### top50 prestaties codes ####
dt_top50codes <- as.data.table(openxlsx2::read_xlsx("data/processed/top50_mszprest_DBC_codes_by_category.xlsx"))

# filter out rows that don't meet inclusion criteria
dt_top50codes_filtered <- dt_top50codes[
  n_instellingen >= 3 & share_main_instelling <= 0.5 &
    (n_instellingen_30d >=3 | is.na(n_instellingen_30d)) & (share_main_instelling_30d <= 0.5 | is.na(share_main_instelling_30d)) & 
    (n_instellingen_1000d >=3 | is.na(n_instellingen_1000d)) & (share_main_instelling_1000d <= 0.5 | is.na(share_main_instelling_1000d)) &
    (n_instellingen_In_leven >=3 | is.na(n_instellingen_In_leven)) & (share_main_instelling_In_leven <= 0.5 | is.na(share_main_instelling_In_leven)) &
    (n_instellingen_Overleden >=3 | is.na(n_instellingen_Overleden)) & (share_main_instelling_Overleden <= 0.5 | is.na(share_main_instelling_Overleden))
]

# fill the columns that should be equal
dt_top50codes_filtered[died == "In_leven", ':='(
  n_totaal_gebruikers_In_leven = n_totaal_gebruikers,
  n_instellingen_In_leven = n_instellingen,
  n_totaal_declaraties_In_leven = n_totaal_declaraties
)]

dt_top50codes_filtered[died == "Overleden", ':='(
  n_totaal_gebruikers_Overleden = n_totaal_gebruikers,
  n_instellingen_Overleden = n_instellingen,
  n_totaal_declaraties_Overleden = n_totaal_declaraties
  
)]

dt_top50codes_filtered[bin_size == "1000d", ':='(
  n_totaal_gebruikers_1000d = n_totaal_gebruikers,
  n_instellingen_1000d = n_instellingen,
  n_totaal_declaraties_1000d = n_totaal_declaraties
)]

dt_top50codes_filtered[bin_size == "30d", ':='(
  n_totaal_gebruikers_30d = n_totaal_gebruikers,
  n_instellingen_30d = n_instellingen,
  n_totaal_declaraties_30d = n_totaal_declaraties
)]

# round to 10
cols_to_round <- grep("^(n_totaal|sum_totaal|median_cost)", names(dt_top50codes_filtered), value=T)
dt_top50codes_filtered <- dt_top50codes_filtered[, (cols_to_round) := lapply(.SD, round, digits=-1), .SDcols = cols_to_round]
for (col_name in cols_to_round) {
  dt_top50codes_filtered[get(col_name) == "0", (col_name) := NA]
}

# set all cols where n_totaal_gebruikers or n_totaal_declarates = 0, to 0 as well
suffixes <- c("_1000d", "_30d", "_In_leven", "_Overleden", "")
n_cols <- c("n_totaal_gebruikers")
cols_to_mask <- c("n_instellingen", "n_totaal_gebruikers", "n_totaal_declaraties", "sum_totaal_groep", "median_cost_per_declaratie")

for (suffix in suffixes) {
  n_cols_suffix <- paste0(n_cols, suffix)
  cols_to_mask_suffix <- intersect(paste0(cols_to_mask, suffix), names(dt_top50codes_filtered))
  # browser()
  
  dt_top50codes_filtered[get(n_cols_suffix) == 0 | is.na(get(n_cols_suffix)), (cols_to_mask_suffix) := NA]
}

# save output-ready version
output_cols <- setdiff(names(dt_top50codes_filtered), grep("share|n_instelling", names(dt_top50codes_filtered), value=T))
openxlsx2::write_xlsx(dt_top50codes_filtered, glue("{output_folder}GEENOUTPUT_Achtergrondinformatie/top50_prestaties.xlsx"))
openxlsx2::write_xlsx(dt_top50codes_filtered[ , .SD, .SDcols = output_cols], glue("{output_folder}top50_prestaties.xlsx"))

