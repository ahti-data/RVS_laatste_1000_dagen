source("./src/setup.R")

## Read relevant LBZ files
## NOTE: we will want to include 2023 in the future. When available
## edit in ./src/setup.R.
lbz_data_comb <- load_dataset(years, "lbzbasistab", cols = rel_cols_lbz, create_year_col = TRUE)
lbz_data_comb[lbzicd10hoofddiagnose == "", lbzicd10hoofddiagnose := NA]
lbz_data_comb <- lbz_data_comb[!is.na(lbzicd10hoofddiagnose)]


#' Function to map ICD10 codes to a crosswalk
#'
#' @param dt Data.table with a column indicating an ICD10 code
#' @description Function that maps known ICD10 codes to swab groups
#'
map_icd10 <- function(dt) {
  cw_icd10 <- readxl::read_xlsx(
    "data/raw/icd10_cw_010526.xlsx"
  )

  setDT(cw_icd10)

  ## NOTE: subject to change. They should provide a crosswalk with a single row
  ## containing ICD-10 and SWAB. Current crosswalk contains:
  ## - duplicate mappings (multiple swabs for the same ICD10)
  ## - ICD-10 code that contains a later code (e.g. A123 and A1234)
  ## - ranges (e.g. A1-A14)
  ## We need a unique ICD10 to swab groep mapping and use first occurrences
  ## in case of duplicate mappings.

  ## Remove NA ICD-10 codes
  cw_icd10 <- cw_icd10[!is.na(cw_icd10$`ICD-10`), ]

  ## Remove dots
  cw_icd10[, icd10 := gsub("\\.", "", `ICD-10`)]
  cw_icd10[, Excluding := gsub("\\.", "", `Excluding`)]

  # Handle double codes
  cw_icd10[grepl(",", icd10), Inclusief := tstrsplit(icd10, ",", fixed = TRUE)[[2]]]
  cw_icd10[grepl(",", icd10), icd10 := tstrsplit(icd10, ",", fixed = TRUE)[[1]]]
  
  cw_icd10[, Inclusief := gsub("\\(.*\\)", "", Inclusief)]
  cw_icd10[, Inclusief := trimws(Inclusief)]

  ## Remove everything between brackets
  cw_icd10[, icd10 := gsub("\\(.*\\)", "", icd10)]
  cw_icd10[, icd10 := trimws(icd10)]

  ## Remove duplicates: keep first appearance
  cw_icd10 <- cw_icd10[!duplicated(cw_icd10$icd10), ]

  ## Make placeholders for swab, icd10
  unique_lbz_icd10_codes <- as.data.table(unique(dt$lbzicd10hoofddiagnose))
  setnames(unique_lbz_icd10_codes, "V1", "icd10_lbz")
  
  ## temporarily split codes by their letter and numbers, to deal with ranged diseases later
  unique_lbz_icd10_codes[, icd10_letter := substr(icd10_lbz, 1, 1)]
  unique_lbz_icd10_codes[, icd10_numbers := substr(icd10_lbz, 2, 9999)]

  icd10_codes <- unique(cw_icd10$icd10)
  icd10_codes <- sort(icd10_codes)
  
  # map the icd10 codes from lbz
  for (icd10_code in icd10_codes) {
    # derive swab group from crosswalk
    swab_group <- cw_icd10$`Categorie`[cw_icd10$icd10 == icd10_code]
    swab_group <- swab_group[1]
    icd10_code_orig <- cw_icd10$`ICD-10`[cw_icd10$icd10 == icd10_code]

    # retrieve the "excluding" and "including" codes, if appropriate
    excluding <- as.character(cw_icd10[icd10 == icd10_code, .SD, .SDcols = "Excluding"])
    excluding <- strsplit(excluding, ",")[[1]]
    excluding_pattern <- paste0("^(", paste(excluding, collapse = "|"), ")")
    including <- as.character(cw_icd10[icd10 == icd10_code, .SD, .SDcols = "Inclusief"])

    # first, handle ranged cases
    if (grepl("-", icd10_code)) {
      # dissect the icd10codes
      icd_letter <- substring(icd10_code, 1, 1)
      parts <- strsplit(icd10_code, "-")[[1]] # split by "-"
      code_min <- parts[1] # get the lower end code
      code_min_number <- substr(code_min, 2, 9999) # get the number part of the lower end code
      code_min_number_length <- nchar(code_min_number)

      code_max <- parts[2] # get the higher end code
      code_max_number <- substr(code_max, 2, 9999) # get the number part of the higher end code
      code_max_number_length <- nchar(code_max_number)

      unique_lbz_icd10_codes[
        icd10_letter == icd_letter &
          substr(icd10_numbers, 1, code_max_number_length) <= code_max_number &
          substr(icd10_numbers, 1, code_min_number_length) >= code_min_number &
          !grepl(excluding_pattern, icd10_lbz),
        ":="(swab = swab_group, icd10_disease = icd10_code_orig)
      ]

    } else {
      unique_lbz_icd10_codes[
        (grepl(glue("^{icd10_code}"), icd10_lbz) |
          grepl(glue("^{including}"), icd10_lbz)) &
          !grepl(excluding_pattern, icd10_lbz),
        ":="(swab = swab_group, icd10_disease = icd10_code_orig)
      ]
    }

    print(paste("icd10_code", icd10_code))
  }
  
  # save intermittent, to check later
  zorgtype <- dt$lbzzorgtype[[1]]
  fwrite(unique_lbz_icd10_codes, glue("data/processed/00_cleaned/icd10_cw_{zorgtype}.csv"))
  
  # finally, merge the full codes back
  dt <- merge(
    dt,
    unique_lbz_icd10_codes[, .SD, .SDcols = c("icd10_lbz", "icd10_disease", "swab")],
    by.x = "lbzicd10hoofddiagnose",
    by.y = "icd10_lbz",
    all.x=T
  )
  
  # filter out NA's
  dt <- dt[!is.na(icd10_disease)]

  return(dt)
}

## Map icd10 codes to swab groups
for (zorgtype in unique_zorgtypes) {
  if (zorgtype == "all") {
    lbz_data_comb_zorgtype <- lbz_data_comb
  } else {
  lbz_data_comb_zorgtype <- lbz_data_comb[lbzzorgtype == zorgtype]
  }
  lbz_data_comb_mapped <- map_icd10(lbz_data_comb_zorgtype)
  
  arrow::write_parquet(
    lbz_data_comb_mapped, 
    glue("./data/processed/00_cleaned/lbz_data_{zorgtype}_18_23.parquet")
  )
}


