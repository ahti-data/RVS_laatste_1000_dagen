# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Find demographics characteristics, date and cause of death
# Output: Individual level processed data
# Last edited: 20 April 2026

rm(list = ls())
gc()
source("./src/00_inputs.R")

#### Find sex and birth year ####
file_path <- get_path_newest(
  path = "G:/Bevolking/GBAPERSOONTAB/2024/",
  string_pattern = "GBAPERSOONTAB",
  extension = ".csv",
  method = "max_version"
)
df_gbapersoon <- fread(file_path, select = cols_to_select_gbapersoon)
df_gbapersoon <- format_data(df_gbapersoon)
df_gbapersoon[, ":="(geslacht = ifelse(gbageslacht == 2, "Vrouwen", "Mannen"))]

setindex(df_gbapersoon, NULL)
arrow::write_parquet(
  df_gbapersoon[, .(rinpersoon, geslacht,
                    year_of_birth = gbageboortejaar,
                    month_of_birth = gbageboortemaand
  )],
  "./data/raw/gbapersoon.parquet"
)
rm(df_gbapersoon)
gc()


#### Find migration background ####
df_rin <- r_parquet_get_dt("./data/raw/gbapersoon.parquet", columns = "rinpersoon")
df <- add_herkomst(df_rin, remove_orig = T)

df[, migratie_achtergrond := fcase(
  herkomst3 == "Nederlandse herkomst", 'NL',
  herkomst3 == "Kind van migrant", 'Kind van migrant',
  herkomst3 == "Migrant", 'Migrant',
  default = "Onbekend"
)]

setindex(df, NULL)
arrow::write_parquet(
  df[, .(rinpersoon, migratie_achtergrond)],
  "./data/raw/migratie_achtergrond.parquet"
)
rm(df, df_rin)
gc()


#### Find date of death ####
file_path <- get_path_newest(
  path = "G:/Bevolking/GBAOVERLIJDENTAB/2024/",
  string_pattern = "GBAOVERLIJDEN",
  extension = ".sav",
  method = "max_version"
)
df_overlijden <- haven::read_sav(file_path, col_select = all_of(cols_to_select_overlijden))
df_overlijden <- format_data(df_overlijden)
df_overlijden[, year_of_death := as.integer(substr(gbadatumoverlijden, 1, 4))]

# Keep only people who died in 2016-2025
df_overlijden <- df_overlijden[year_of_death %between% c(2016, 2025)]
df_overlijden[, gbadatumoverlijden := as.Date(gbadatumoverlijden, format = "%Y%m%d")]

# Save
setindex(df_overlijden, NULL)
arrow::write_parquet(df_overlijden, "./data/raw/overlijden.parquet")
rm(df_overlijden)
gc()


#### Find causes of death ####
df_doodoorz <- data.table()
for (cohort in c(2013:2024)){
  
  file_path <- get_path_newest(
    path = glue("G:/GezondheidWelzijn/DOODOORZTAB/{cohort}/"),
    string_pattern = "DOODOORZ",
    extension = ".sav",
    method = "max_version"
  )
  df <- haven::read_sav(file_path, col_select = all_of(cols_to_select_doodoorz))
  df <- format_data(df)
  df[, uccode := toupper(substr(uccode, 1, 3))]
  
  df[, doodsoorzaak := fcase(
    uccode %in% sprintf('C%02d', 0:97),
    'Palliatief kanker',
    uccode %in% sprintf('I%02d', c(0:11, 14:52, 60:69)),
    'Palliatief hart & vaatziekten',
    uccode %in% c('F01', 'F03', 'G30', 'R54'),
    'Palliatief dementie',
    uccode %in% sprintf('J%02d', c(40:47, 96)),
    'Palliatief luchtwegaandoening',
    uccode %in% c('N17', 'N18', 'N28', 'I12', 'I13',
                  sprintf('K%02d', 70:77), 
                  'G10', 'G12.2', 'G20', 'G23.1', 'G35', 'G90.3',
                  sprintf('B%02d', 20:24)),
    'Palliatief overig',
    uccode %in% c(sprintf('V%02d', 0:98), sprintf('Y%02d', 0:98)),
    'Uitwendige oorzaken',
    default = 'Overig'
  )]
  
  df_doodoorz <- rbindlist(list(df_doodoorz, df), use.names = T, ignore.attr = T)
  
  rm(df)
}

# replace_values_by_haven_labels(df_doodoorz, get_path_newest(
#   path = glue("G:/GezondheidWelzijn/DOODOORZTAB/2024/"),
#   string_pattern = "DOODOORZ",
#   extension = ".sav",
#   method = "max_version"
# ), cols = "nndlocationcode", format=T)

# 3 people have duplicated records (2 different causes), keep the first one
df_doodoorz <- df_doodoorz[, .SD[1], by = rinpersoon]

setindex(df_doodoorz, NULL)
arrow::write_parquet(df_doodoorz, "./data/raw/doodoorz.parquet")
rm(df_doodoorz)
gc()


#### Find registration in NL ####
file_path <- get_path_newest(
  path = "G:/Bevolking/GBAHUISHOUDENSBUS/Geconverteerde bestanden",
  string_pattern = "2024",
  extension = ".parquet",
  method = "max_version"
)

df_registration <- arrow::read_parquet(
  file_path,
  col_select = all_of(cols_to_select_gbahuishoudensbus)
)
df_registration <- format_data(df_registration)

# Keep only people who have been continuously registered from 1 January 2020 (2016) to
# 31 December 2022 (2019) in NL
for (cohort in c(2019, 2023)) {
  print(cohort)
  
  start_date <- glue("{cohort-3}0101")
  end_date <- glue("{cohort-1}1231")
  df_fully_covered <- df_registration[datumeindehh >= start_date &
                                        datumaanvanghh <= end_date]
  df_fully_covered <- df_fully_covered[, .(
    min_start = min(datumaanvanghh),
    max_end = max(datumeindehh)
  ),
  by = rinpersoon
  ]
  df_fully_covered <- df_fully_covered[min_start <= start_date &
                                         max_end >= end_date]
  df_fully_covered <- unique(df_fully_covered[, .(rinpersoon)])
  
  # Save
  setindex(df_fully_covered, NULL)
  arrow::write_parquet(
    df_fully_covered,
    glue("./data/raw/fully_covered_{cohort}.parquet")
  )
  rm(df_fully_covered)
  gc()
}
rm(df_registration)
gc()


#### Find demographic characteristics and income ####
# Note that demographics are measured on 31 December YEAR,
# education on 1 October YEAR, and labor market outcomes on 1 January YEAR
var_sets <- list(
  list(
    vars_select = cols_to_select_stapeling_01_01,
    vars_label = cols_to_label_stapeling_01_01,
    period = "01_01",
    offset = 0
  ),
  list(
    vars_select = cols_to_select_stapeling_31_12,
    vars_label = cols_to_label_stapeling_31_12,
    period = "31_12",
    offset = -1
  )
)

for (var_set in var_sets) {
  df_stapeling <- data.table()
  
  cols_to_select <- var_set$vars_select
  cols_to_label <- var_set$vars_label
  period <- var_set$period
  offset <- var_set$offset
  
  # Look only for years that we use for cohorts (2019 and 2023) plus a year before 
  # to impute missing values
  for (yr in c(2018, 2019, 2022, 2023)) {
    print(yr)
    
    # For outcomes measured on 31 December move forward the year,
    # For outcomes measured on 1 January, keep the year as it is
    year_offset <- yr + offset
    
    file_path <- get_newest_parquet_check(
        folder_g_parquet = NULL,
        folder_h_parquet = "H:/data/Parquet_files_G_drive/Stapeling/parquet_files/",
        folder_g_sav = glue::glue("G:/Maatwerk/STAPELINGSMONITOR/{year_offset}/"),
        string_pattern_parquet = year_offset,
        string_pattern_sav = year_offset,
        stop_on_mismatch = F)
    file_path = tools::file_path_sans_ext(file_path)
    
    file_path_sav <- get_path_newest(
      path = glue("G:/Maatwerk/STAPELINGSMONITOR/{year_offset}/"),
      string_pattern = year_offset,
      extension = ".sav",
      method = "max_version"
    )
    
    # Get names of all columns
    schema <- arrow::ParquetFileReader$create(
      glue::glue('{file_path}/partition=1/data_0.parquet'))
    col_names <- names(schema$GetSchema())
    
    # Select available columns among the ones I want to load
    cols_available <- intersect(
      col_names, c("RINPERSOON", paste0(cols_to_select, "_", year_offset))
    )
    
    cols_not_available <- setdiff(
      paste0(cols_to_select, "_", year_offset), col_names
    )
    
    cat(
      "\n For available variables:", cols_available,
      "\n Measured on", period, "in", year_offset,
      "\n Load parquet file:\n", file_path,
      "\n Get labels from sav file:\n", file_path_sav,
      "\n Assign to year", yr
    )
    cat("\n Not available columns:\n", cols_not_available)
    
    df <- arrow::open_dataset(file_path) |>
      select(all_of(cols_available)) |>
      collect()
    df <- format_data(df)
    
    # Get labels from SPSS files
    replace_values_by_haven_labels(
      df,
      sav_path = file_path_sav,
      cols = cols_to_label,
      format = T
    )
    
    # CBS changed labels for education from 2023
    if ("hbopl" %in% names(df) & year_offset >= 2023) {
      df[, hbopl := fcase(
        hbopl == "Basisonderwijs, vmbo, mbo1", "Lager",
        hbopl == "Havo, vwo, mbo2-4", "Middelbaar",
        hbopl == "Hbo, wo", "Hoger",
        hbopl == "Onbekend", "Onbekend",
        default = "Onbekend"
      )]
      
      df[, hgopl := fcase(
        hgopl == "Basisonderwijs, vmbo, mbo1", "Lager",
        hgopl == "Havo, vwo, mbo2-4", "Middelbaar",
        hgopl == "Hbo, wo", "Hoger",
        hgopl == "Onbekend", "Onbekend",
        default = "Onbekend"
      )]
    }
    
    # Make income classes
    if ("percsm" %in% names(df)) {
      df <- make_inkomen_klasse(df,
                                remove_orig = c("inkpers", "percsm"),
                                imp_inst = ("inkpers" %in% cols_to_select)
      )
    }
    
    # Make the household composition
    if ("huishsamstsocec" %in% names(df)) {
      df <- make_huishoudsamenstelling(df)
    }
    
    df <- df[, year := yr]
    df_stapeling <- rbindlist(list(df_stapeling, df), use.names = T, fill = T)
    
    # Save rinpersoon for later use
    setindex(df, NULL)
    arrow::write_parquet(df[, .(rinpersoon)], glue("./data/raw/rinpersoon_{yr}.parquet"))
    rm(df)
    gc()
  }
  
  setindex(df_stapeling, NULL)
  arrow::write_parquet(df_stapeling, glue("./data/raw/stapeling_{period}.parquet"))
  rm(df_stapeling)
  gc()
}

# Recode variables measured on 1 January
df_sample <- r_parquet_get_dt("./data/raw/stapeling_31_12.parquet",
                              columns = c("rinpersoon", "year")
)
df <- r_parquet_get_dt("./data/raw/stapeling_01_01.parquet")

df <- merge(df_sample, df, by = c("rinpersoon", "year"), all.x = T)
rm(df_sample)
gc()

#df <- df[hbopl == "hoger", hbopl := "Hoger"]

df <- df[, .(rinpersoon, year, inkomen_klasse, huishoudsamenstelling, provincie)]
gc()

# For missing labor market outcomes, find the previous one
setorder(df, rinpersoon, year)
cols_to_change <- setdiff(names(df), c("rinpersoon", "year"))
prev_cols <- paste0(cols_to_change, "_previous")
df[, (prev_cols) := lapply(.SD, shift, type = "lag"),
   by = rinpersoon, .SDcols = cols_to_change
]

for (i in seq_along(cols_to_change)) {
  v <- cols_to_change[i]
  p <- prev_cols[i]
  df[is.na(get(v)), (v) := get(p)]
}
df[, (prev_cols) := NULL]
gc()

#df[is.na(hbopl), hbopl := "Onbekend"]
df[is.na(inkomen_klasse), inkomen_klasse := "Onbekend"]
df[is.na(huishoudsamenstelling), huishoudsamenstelling := "overig_onbekend_hh"]

setindex(df, NULL)
arrow::write_parquet(df, "./data/raw/stapeling_01_01.parquet")
rm(df)
gc()


#### Find SESWOA score ####
# Note that SESWOA score is measured on 1 January, so use YEAR
df_seswoa <- data.table()
df_rin <- data.table()
for (yr in c(2019, 2023)) {
  path <- glue("./data/raw/rinpersoon_{yr}.parquet")
  
  # Take score from the previous year (year - 1),
  # as 'rinpersoon' is already shifted (year - 1) when worked with Stapelings data above
  rin <- r_parquet_get_dt(path)
  df <- add_seswoa(rin, yr - 1)
  df[, year := yr]
  
  df_seswoa <- rbindlist(list(df_seswoa, df), use.names = T)
  df_rin <- rbindlist(list(df_rin, rin), use.names = T)
  df_rin <- unique(df_rin, by = "rinpersoon")
  
  rm(rin, df)
  gc()
  unlink(path)
}

setindex(df_seswoa, NULL)
setindex(df_rin, NULL)
arrow::write_parquet(df_seswoa, "./data/raw/seswoa.parquet")
arrow::write_parquet(df_rin, "./data/raw/rinpersoon.parquet")
rm(df_seswoa, df_rin)
gc()


#### create lookup table etkind ####
# lookup_icd10 <- rbindlist(list(
#   data.table(icd10 = sprintf('C%02d', 0:97), doodsoorzaak = 'Palliatief kanker'),
#   data.table(icd10 = sprintf('I%02d', c(0:11, 14:52, 60:69)), doodsoorzaak = 'Palliatief hart & vaatziekten'),
#   data.table(icd10 = c('F01', 'F03', 'G30', 'R54'), doodsoorzaak = 'Palliatief dementie'),
#   data.table(icd10 = sprintf('J%02d', c(40:47, 96)), doodsoorzaak = 'Palliatief luchtwegaandoening'),
#   data.table(icd10 = c('N17', 'N18', 'N28', 'I12', 'I13',
#                        sprintf('K%02d', 70:77), 
#                        'G10', 'G122', 'G20', 'G231', 'G35', 'G903',
#                        sprintf('B%02d', 20:24)), doodsoorzaak = 'Palliatief overig'),
#   data.table(icd10 = c(sprintf('V%02d', 0:98), sprintf('Y%02d', 0:98)), doodsoorzaak = 'Uitwendige oorzaken')
# ))
# 
# # create a main and suffix cols
# lookup_icd10[, ':='(
#   hoofd_icd10 = substr(icd10, 1, 3),
#   suffix_icd10 = substr(icd10, 4, 999)
# )][suffix_icd10 == "", suffix_icd10:=NA]
# 
# 
# arrow::write_parquet(lookup_icd10, "data/raw/etkind_crosswalk_icd10.parquet")
