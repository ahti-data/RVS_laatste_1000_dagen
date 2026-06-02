source("src/setup.R")

#### EXTERNAL: load and clean the quality checks excel file ####
load_format_quality_checks_dt <- function() {
  dt_quality_checks <- fread("data/raw/datacheck_iter3.csv")[, c("Onderwerp", "V7") := NULL]
  setnames(dt_quality_checks,
           c("Regios", "Leeftijd", "Perioden", "Geslacht", "Totaal opnamen", "Klinische opnamen en observaties", "Dagopnamen"),
           c("level", "leeftijd_cat", "year", "geslacht", "n_all_check", "n_K_O_check", "n_D_check"))
  
  # Regios
  dt_quality_checks[level == "Nederland", ':='(level = "nl", code = "NL")]
  dt_quality_checks[level == "GGD Groningen (GG)", ':='(level = "ggd", code = "GG0111")]
  dt_quality_checks[level == "GGD Twente (GG)", ':='(level = "ggd", code = "GG1106")]
  
  dt_quality_checks[year == "2023*", year := 2023]
  dt_quality_checks[, row_id := .I]
  
  return(dt_quality_checks)
}
dt_quality_checks <- load_format_quality_checks_dt()



# Load appropriate datasets
all_possible_codes <- c(
  c("icd10_disease_J09", "icd10_disease_J10", "icd10_disease_J11"),
  c("icd10_disease_K35", "icd10_disease_K36", "icd10_disease_K37"),
  c("icd10_disease_U07_1_U07_2"),
  c("icd10_disease_U09")
)

counts_LG <- fread("output/output_3/univariate_counts_profile_LG.csv")[variable %in% all_possible_codes]
counts_leeftijd_cat <- fread("output/output_3/univariate_counts_leeftijd_cat.csv")[variable %in% all_possible_codes]
counts_geslacht <- fread("output/output_3/univariate_counts_geslacht.csv")[variable %in% all_possible_codes]
counts_all <- fread("output/output_3/univariate_counts_all.csv")[variable %in% all_possible_codes]


for (idx in 1:nrow(dt_quality_checks)) {
  row <- dt_quality_checks[row_id == idx]
  
  disease_codes <- switch(row$Diagnose,
                          "10.3 Influenza" = c("icd10_disease_J09", "icd10_disease_J10", "icd10_disease_J11"),
                          "11.10 Appendicitis" = c("icd10_disease_K35", "icd10_disease_K36", "icd10_disease_K37"),
                          "21.1 COVID-19 infectie" = c("icd10_disease_U07_1_U07_2"),
                          "21.3 Post-COVID-19 aandoening" = c("icd10_disease_U09")
                          )
  
  
  
  # first, split choices by underlying dataset needed
  if (row$leeftijd_cat == "Totaal leeftijd" & row$geslacht == "Totaal mannen en vrouwen") {
    matched_rows <- counts_all[year == row$year & level == row$level & code == row$code & variable %in% disease_codes]
  } else if (row$leeftijd_cat == "Totaal leeftijd" & row$geslacht != "Totaal mannen en vrouwen") {
    matched_rows <- counts_geslacht[year == row$year & level == row$level & code == row$code & variable %in% disease_codes & geslacht == row$geslacht]
  } else if (row$leeftijd_cat != "Totaal leeftijd" & row$geslacht == "Totaal mannen en vrouwen") {
    
    leeftijd_cats <- switch(row$leeftijd_cat,
                            "80 jaar of ouder" = c(8, 9, 10, 11))
    
    matched_rows <- counts_leeftijd_cat[year == row$year & level == row$level & code == row$code & variable %in% disease_codes & leeftijd_cat %in% leeftijd_cats]
    
  } else if (row$leeftijd_cat != "Totaal leeftijd" & row$geslacht != "Totaal mannen en vrouwen") {
    LG_cats = switch(row$geslacht,
                     "Mannen" = c("8_Mannen", "9_Mannen", "10_Mannen", "11_Mannen"),
                     "Vrouwen" =  c("8_Vrouwen", "9_Vrouwen", "10_Vrouwen", "11_Vrouwen"))
    
    matched_rows <- counts_LG[year == row$year & level == row$level & code == row$code & variable %in% disease_codes & profile_LG %in% LG_cats]
    }
    
  
  dt_quality_checks[row_id == idx, ':='(
    n_all = sum(matched_rows$n_all, na.rm=T),
    n_K_O = sum(matched_rows$n_K, na.rm=T) + sum(matched_rows$n_O, na.rm=T),
    n_D = sum(matched_rows$n_D, na.rm=T)
  )]
  
  print(glue("processed row {idx} / {nrow(dt_quality_checks)}"))
}


dt_quality_checks[, ':=' (
  delta_n_all = n_all_check - n_all,
  delta_n_K_O = n_K_O_check - n_K_O,
  delta_N_D = n_D_check - n_D
)]

#### INTERNAL: check some cases where icd10 codes have no matches whatsoever ####
icd10_cw_source <- readxl::read_xlsx(
  "data/raw/icd10_cw_010526.xlsx"
)
icd10_cw_K <- fread("data/processed/00_cleaned/icd10_cw_K.csv")[swab != ""]
icd10_cw_D <- fread("data/processed/00_cleaned/icd10_cw_D.csv")[swab != ""]
icd10_cw_O <- fread("data/processed/00_cleaned/icd10_cw_O.csv")[swab != ""]

all_unique_codes_found <- unique(c(
  unique(icd10_cw_O$icd10_disease),
  unique(icd10_cw_K$icd10_disease),
  unique(icd10_cw_D$icd10_disease)))

codes_no_matches <- setdiff(
  unique(icd10_cw_source$`ICD-10`), 
  all_unique_codes_found
  )

lbz_data_comb <- load_dataset(years, "lbzbasistab", cols = rel_cols_lbz, create_year_col = TRUE)
lbz_data_comb[lbzicd10hoofddiagnose == "", lbzicd10hoofddiagnose := NA]
lbz_data_comb <- lbz_data_comb[!is.na(lbzicd10hoofddiagnose)]

unique_codes_lbz <- as.data.table(unique(lbz_data_comb$lbzicd10hoofddiagnose))

# Now manually walk through the codes_no_matches, and check this with the unique codes found in lbz.

#### INTERNAL: check swab categories line up ####

icd10_cw_K <- fread("data/processed/00_cleaned/icd10_cw_K.csv")[swab != ""]
icd10_cw_K[, icd10_disease := gsub("[[:punct:]]", "_", icd10_disease)]
icd10_cw_K[, swab := gsub("[[:punct:]]", "_", swab)]

counts_all_icd10 <- fread("data/processed/03_formatted/univariate_counts_n_icd10_all_all_output.csv")
counts_all_swab <- fread("data/processed/03_formatted/univariate_counts_n_swab_all_all_output.csv")



for (swab_group in unique(icd10_cw_K$swab)) {
  
  codes <- paste0("icd10_disease_", unique(icd10_cw_K[swab == swab_group]$icd10_disease))
  
  sum_n_icd10s <- sum(counts_all[variable %in% codes & year == 2020 & level == "nl"]$value, na.rm=T)
}


#### INTERNAL: check if comp and aggregates line up ####

full_output_inc_comp_LG_all <- as.data.table(openxlsx2::read_xlsx("output/output_3/full_output_inc_comp_LG_all_output.xlsx"))
univariate_counts <- fread("output/output_3/univariate_counts_all.csv")

agg_comp <- na.omit(full_output_inc_comp_LG_all[, .(
  n_comp = sum(n, na.rm=T)),
  by = .(year, variable, code, level)
])

agg_counts <- na.omit(univariate_counts_profile_LG[year == 2018, .(
  n_counts = sum(n_all, na.rm=T)),
  by = .(year, variable, code, level)
])

agg_merged <- merge(
  agg_counts,
  agg_comp,
  by = c("year", "variable", "code", "level"),
  all.x=T
)[!is.na(n_comp)][, diff := n_counts - n_comp]

View(agg_merged)

