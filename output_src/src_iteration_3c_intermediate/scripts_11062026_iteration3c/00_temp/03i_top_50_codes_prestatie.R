# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Merge ZPK codes with activities
# Output: Merge MSZPRESTATIESVEKTTAB and MSZZORGACTIVITEITENVEKTTAB with ReflijstZorgactiviteiten
# Last edited: 4 June 2026

source("src/00_inputs.R")
options(scipen = 999)

# Load a sample
dt_sample <- r_parquet_get_dt(
  "H:/_Current_projects/laatste_1000_dagen/data/raw/overlijden_with_matched_add_demog.parquet")
dt_sample <- dt_sample[cohort == 2023 & died == 'Overleden']
dt_sample <- unique(dt_sample[, .(rinpersoon, gbadatumoverlijden)])

# Load prestaties
dt_prestatie <- data.table()
for (yr in c(2020:2023)) {
  print(yr)
  
  file_path <- get_newest_parquet_check(
    folder_g_parquet = NULL,
    folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files/",
    folder_g_sav = 'G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/',
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
    stop_on_mismatch = F)
  file_path = tools::file_path_sans_ext(file_path)
  
  ds <- arrow::open_dataset(file_path)
  dt_temp <- ds |>
    filter(as.numeric(RINPERSOON) %in% dt_sample$rinpersoon) |>
    select(all_of(names(ds)[
      tolower(names(ds)) %in% tolower(c('RINPERSOON', 'VEKTMSZKoppelIDPrestZa', 'VEKTMSZBeginjaarPrest',
                    'VEKTMSZBegindatumPrest', 
                    'VEKTMSZDBCZorgproduct', 'VEKTMSZDeclaratiecode'))])) |>
    collect()
  dt_temp <- format_data(dt_temp)
  dt_prestatie <- rbindlist(list(dt_prestatie, dt_temp), use.names = T)
  rm(dt_temp)
}
setorder(dt_prestatie, "rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest")

# Load activities
dt_activit <- data.table()
for (yr in c(2020:2023)) {
  print(yr)
  
  file_path <- get_newest_parquet_check(
    folder_g_parquet = NULL,
    folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZActiviteiten/parquet_files/",
    folder_g_sav = 'G:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/',
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
    stop_on_mismatch = F)
  file_path = tools::file_path_sans_ext(file_path)
  
  ds <- arrow::open_dataset(file_path)
  dt_temp <- ds |>
    filter(as.numeric(RINPERSOON) %in% dt_sample$rinpersoon) |>
    select(all_of(names(ds)[
      tolower(names(ds)) %in% tolower(c('RINPERSOON', 'VEKTMSZKoppelIDPrestZa', 'VEKTMSZBeginjaarPrest',
                                        'VEKTMSZZorgactiviteitdatum',
                                        'VEKTMSZZorgactiviteit'))])) |>
    collect()
  dt_temp <- format_data(dt_temp)
  dt_activit <- rbindlist(list(dt_activit, dt_temp), use.names = T)
  rm(dt_temp)
}
setorder(dt_activit, "rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest")

# MSZPRESTATIESVEKTTAB contains DBCs (that can be merged with MSZZORGACTIVITEITENVEKTTAB)
# and OZPs (that cannot be merged with MSZZORGACTIVITEITENVEKTTAB)
# First, look at the joint dataset between both -> DBCs
dt_joint <- merge(dt_prestatie, dt_activit, 
                  by = c("rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest"),
                  all = F)
setorder(dt_joint, "rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest")

# Second, look at the rest -> OZPs
dt_prestatie_ozp <- dt_prestatie[
  !dt_joint,
  on = c("rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest")
]
setorder(dt_prestatie_ozp, "rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest")

# Notice that (almost) all codes are 999999999, indicating that are OZPs
table(dt_prestatie_ozp$vektmszdbczorgproduct, useNA = 'ifany')

# Merge with ZPKs
zpk_codes <- rio::import("K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods")
zpk_codes <- format_data(zpk_codes, rin_num = F)

zpk_codes <- zpk_codes[, .(mszzorgactiviteit, 
                           zpk_5 = as.integer(zpkcode == 5),
                           zpk_6 = as.integer(zpkcode == 6),
                           zpk_other = as.integer(zpkcode %in% c(5, 6)))]

# dt_joint contains DBCs. To merge them with ZPK codes from ReflijstZorgactiviteiten, use "vektmszzorgactiviteit"
dt_joint <- merge(dt_joint, zpk_codes, all.x = T,
                  by.x = 'vektmszzorgactiviteit',
                  by.y = 'mszzorgactiviteit')

# Check that the merge is correct, i.e. all codes have been merged with ZPKs
# If a code is missing a ZPK, might assing it to zpk_other
mean(ifelse(is.na(dt_joint$zpk_5), 1, 0)) * 100

# dt_prestatie_ozp contain OZPs. To merge them with ZPK codes from ReflijstZorgactiviteiten, use "vektmszdeclaratiecode"
dt_prestatie_ozp <- merge(dt_prestatie_ozp, zpk_codes, all.x = T,
                          by.x = 'vektmszdeclaratiecode',
                          by.y = 'mszzorgactiviteit')

# Check that the merge is correct, i.e. all codes have been merged with ZPKs
# If a code is missing a ZPK, might assing it to zpk_other
mean(ifelse(is.na(dt_prestatie_ozp$zpk_5), 1, 0)) * 100

# Then based on both files, dt_joint and dt_prestatie_ozp, it is possible to assign
# all prestaties into categories as Simone specified

# Check top 50 activity codes within DBC by ZPKs 5 and 6
dt_joint <- merge(dt_joint, dt_sample, all.x = T, by = 'rinpersoon')
dt_joint[, vektmszbegindatumprest := as.Date(vektmszbegindatumprest, format = "%Y%m%d")]
dt_joint_1000 <- dt_joint[vektmszbegindatumprest <= gbadatumoverlijden & 
                            vektmszbegindatumprest >= gbadatumoverlijden - 1000]
dt_joint_1000_zpk_5 <- dt_joint_1000[zpk_5 == 1, 
                                     .(n_total = uniqueN(rinpersoon)), 
                                     by = .(vektmszzorgactiviteit)][order(-n_total)][1:50]
dt_joint_1000_zpk_6 <- dt_joint_1000[zpk_6 == 1, 
                                     .(n_total = uniqueN(rinpersoon)), 
                                     by = .(vektmszzorgactiviteit)][order(-n_total)][1:50]
dt_joint_1000_zpk_overig <- dt_joint_1000[zpk_5 != 1 & zpk_6 != 1, 
                                          .(n_total = uniqueN(rinpersoon)), 
                                     by = .(vektmszzorgactiviteit)][order(-n_total)][1:50]
# Save top 20 codes
openxlsx::write.xlsx(list(
  dt_joint_1000_zpk_5 = dt_joint_1000_zpk_5,
  dt_joint_1000_zpk_6 = dt_joint_1000_zpk_6,
  dt_joint_1000_zpk_overig = dt_joint_1000_zpk_overig
), file = './output/top_50_codes_activit_in_dbc.xlsx')
