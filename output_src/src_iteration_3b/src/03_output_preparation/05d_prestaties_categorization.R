#### TODO ####

#### Initialize ####
rm(list=ls())
gc()

source("src/00_inputs.R")

#### zpk categories distribution ####
dt_mszprest_agg <- as.data.table(openxlsx2::read_xlsx("data/processed/msz_prestaties_agg_zpkcategories.xlsx"))

# remove problematic categories
dt_mszprest_agg <- dt_mszprest_agg[!(zpk_category == "oper_verr" & vektmszsettingzpk == "9")]

# Filter out the rows that do not match the inclusion criteria
dt_mszprest_agg <- dt_mszprest_agg[n_instellingen >=3 & share_main_instelling < 0.5]

# Round to 10
cols_to_round <- c("n_totaal_gebruikers", "n_totaal_declaraties", "n_totaal_population")
dt_mszprest_agg <- dt_mszprest_agg[, (cols_to_round) := lapply(.SD, round, digits=-1), .SDcols = cols_to_round]
dt_mszprest_agg <- unique(dt_mszprest_agg) # temporary duplicate fix


# save output-ready version
output_cols <- setdiff(names(dt_mszprest_agg), grep("share|n_instelling", names(dt_mszprest_agg), value=T))
openxlsx2::write_xlsx(dt_mszprest_agg, "output/iteration_3a/GEENOUTPUT_Achtergrondinformatie/zpk_categorieen_tellingen.xlsx")
openxlsx2::write_xlsx(dt_mszprest_agg[, .SD, .SDcols = output_cols], "output/iteration_3a/zpk_categorieen_tellingen.xlsx")

#### top50 codes ####
dt_top50codes <- as.data.table(openxlsx2::read_xlsx("data/processed/top20_codes_by_category.xlsx"))

# filter out rows that don't meet inclusion criteria
dt_top50codes_filtered <- dt_top50codes[
  n_instellingen >= 3 & 
    (n_instellingen_30d >=3 | is.na(n_instellingen_30d)) & 
    (n_instellingen_1000d >=3 | is.na(n_instellingen_1000d)) &
    (n_instellingen_In_leven >=3 | is.na(n_instellingen_In_leven)) &
    (n_instellingen_Overleden >=3 | is.na(n_instellingen_Overleden)) &
    share_main_instelling < 0.5 &
    (share_main_instelling_1000d < 0.5 | is.na(share_main_instelling_1000d)) &
    (share_main_instelling_30d < 0.5 | is.na(share_main_instelling_30d)) &
    (share_main_instelling_In_leven < 0.5 | is.na(share_main_instelling_In_leven)) &
    (share_main_instelling_Overleden < 0.5 | is.na(share_main_instelling_Overleden))
  ]

# round to 10
cols_to_round <- grep("^n_totaal", names(dt_top50codes_filtered), value=T)
dt_top50codes_filtered <- dt_top50codes_filtered[, (cols_to_round) := lapply(.SD, round, digits=-1), .SDcols = cols_to_round]

# fill the columns that should be equal
dt_top50codes_filtered[died == "In_leven", ':='(
  n_totaal_gebruikers_In_leven = n_totaal_gebruikers,
  sum_totaal_groep_In_leven = sum_totaal_groep,
  median_cost_per_declaratie_In_leven = median_cost_per_declaratie,
  share_main_instelling_In_leven = share_main_instelling,
  n_instellingen_In_leven = n_instellingen
)]

dt_top50codes_filtered[died == "Overleden", ':='(
  n_totaal_gebruikers_Overleden = n_totaal_gebruikers,
  sum_totaal_groep_Overleden = sum_totaal_groep,
  median_cost_per_declaratie_Overleden = median_cost_per_declaratie,
  share_main_instelling_Overleden = share_main_instelling,
  n_instellingen_Overleden = n_instellingen
)]

dt_top50codes_filtered[bin_size == "1000d", ':='(
  n_totaal_gebruikers_1000d = n_totaal_gebruikers,
  sum_totaal_groep_1000d = sum_totaal_groep,
  median_cost_per_declaratie_1000d = median_cost_per_declaratie,
  share_main_instelling_1000d = share_main_instelling,
  n_instellingen_1000d = n_instellingen
)]

dt_top50codes_filtered[bin_size == "30d", ':='(
  n_totaal_gebruikers_30d = n_totaal_gebruikers,
  sum_totaal_groep_30d = sum_totaal_groep,
  median_cost_per_declaratie_30d = median_cost_per_declaratie,
  share_main_instelling_30d = share_main_instelling,
  n_instellingen_30d = n_instellingen
)]

# set all cols where n_totaal_gebruikers or n_totaal_declarates = 0, to 0 as well
suffixes <- c("_1000d", "_30d", "_In_leven", "_Overleden")
n_cols <- c("n_totaal_gebruikers")
cols_to_mask <- c("n_instellingen", "share_main_instelling", "sum_totaal_groep", "median_cost_per_declaratie", "n_totaal_gebruikers")

for (suffix in suffixes) {
  n_cols_suffix <- paste0(n_cols, suffix)
  cols_to_mask_suffix <- paste0(cols_to_mask, suffix)
  
  rows_to_zero <- rowSums(dt_top50codes_filtered[, ..n_cols_suffix] == 0 | is.na(dt_top50codes_filtered[, ..n_cols_suffix])) > 0
  dt_top50codes_filtered[rows_to_zero, (cols_to_mask_suffix) := 0]
}



# save output-ready version
output_cols <- setdiff(names(dt_top50codes_filtered), grep("share|n_instelling", names(dt_top50codes_filtered), value=T))
openxlsx2::write_xlsx(dt_top50codes_filtered, "output/iteration_3a/GEENOUTPUT_Achtergrondinformatie/top50_activiteiten.xlsx")
openxlsx2::write_xlsx(dt_top50codes_filtered[ , .SD, .SDcols = output_cols], "output/iteration_3a/top50_activiteiten.xlsx")


