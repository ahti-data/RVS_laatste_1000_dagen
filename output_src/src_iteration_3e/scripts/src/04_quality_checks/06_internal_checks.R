# Project: Laatste 1000 dagen
# Author: Marco Griep

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
options(scipen = 999)


#### assert equality among iterations ####

#### check top50 prestaties ####
dt_top50_prestaties <- openxlsx2::read_xlsx(glue("{output_folder}top50_prestaties.xlsx"))
setDT(dt_top50_prestaties)

# we manually recalculate some codes, to ensure this is going well
dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")

codes_to_recalc <- c(70601009, 70401008, 99899107)
cols_to_load <- c("RINPERSOON", "VEKTMSZBegindatumPrest", 
"VEKTMSZVergoedbedragZVW", "VEKTMSZSettingZPK", 
"VEKTMSZDBCZorgproduct", "VEKTMSZSpecialismeDiagnoseCombinatie") 

dt_mszprest_codes <- rbindlist(lapply(2016:2023, function(yr) {
  dir <- find_dataset_directory("mszprestatiesvekttab", "G")
  filepath <- get_path_newest(dir, yr, recursive=T, extension = ".parquet")
  
  ds <- arrow::open_dataset(filepath)
  dt <- ds |>
    mutate(vektmszdbczorgproduct_num = cast(VEKTMSZDBCZorgproduct, arrow::int64())) |>
    filter(vektmszdbczorgproduct_num %in% codes_to_recalc) |>
    select(all_of(cols_to_load)) |>
    collect()
  
  setDT(dt)
  
  return(dt)
}))

dt_mszprest_codes <- format_data(dt_mszprest_codes)

# overlap 
dt_mszprest_codes_overlapped_1000 <- calculate_costs_by_bin_size(
  dt_mszprest_codes,
  dt_overlijden_with_matched,
  cost_columns = NULL,
  bin_size = "1000",
  cost_date_col = "vektmszbegindatumprest"
)[, bin_size := "1000d"]

dt_mszprest_codes_overlapped_30 <- calculate_costs_by_bin_size(
  dt_mszprest_codes,
  dt_overlijden_with_matched,
  cost_columns = NULL,
  bin_size = "last_month",
  cost_date_col = "vektmszbegindatumprest"
)[, bin_size := "30d"]

dt_mszprest_codes_overlapped <- rbindlist(list(dt_mszprest_codes_overlapped_1000, dt_mszprest_codes_overlapped_30))
dt_mszprest_codes_overlapped <- dt_mszprest_codes_overlapped[died == "In leven", died := "In_leven"]
dt_mszprest_codes_overlapped <- dt_mszprest_codes_overlapped[, vektmszdbczorgproduct := as.numeric(vektmszdbczorgproduct)]
dt_mszprest_codes_overlapped <- dt_mszprest_codes_overlapped[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]


for (died_setting in c("Overleden")) { # we only 
  for (cohort_setting in c("2019", "2023")) {
    for (bin_size_setting in c("30d")){
      for (code in codes_to_recalc) {
        
        print(paste(died_setting, cohort_setting, bin_size_setting, code))
        
        dt_top50codes_filtered <- dt_top50_prestaties[vektmszdbczorgproduct == code & died == died_setting & cohort == cohort_setting & bin_size == bin_size_setting]
        
        if (nrow(dt_top50codes_filtered) > 0) {
          print("testing")
          dt_mszprest_codes_overlapped_filtered <- dt_mszprest_codes_overlapped[vektmszdbczorgproduct == code & died == died_setting & cohort == cohort_setting & bin_size == bin_size_setting]
          # assert equal users
          assert_that(abs(
            dt_top50codes_filtered$n_totaal_gebruikers[1] - 
              uniqueN(dt_mszprest_codes_overlapped_filtered$sample_id)
          ) < 20)
          
          
          # assert equal costs
          assert_that(abs(
            (dt_top50codes_filtered$sum_totaal_groep[1] - 
               sum(dt_mszprest_codes_overlapped_filtered$vektmszvergoedbedragzvw, na.rm=T))/ dt_top50codes_filtered$sum_totaal_groep[1]
          ) < 0.1)
          
          # assert equal declaraties
          assert_that(abs(
            dt_top50codes_filtered$n_totaal_declaraties[1] - 
              nrow(dt_mszprest_codes_overlapped_filtered)
          ) < 20)
          
          # assert equal medians
          assert_that(abs(
            dt_top50codes_filtered$median_cost_per_declaratie[1] - 
              median(dt_mszprest_codes_overlapped_filtered$vektmszvergoedbedragzvw, na.rm=T)
          ) < 20)
        }
        
      }
    }
  }
}


#### check equality among iterations: msz_addon ####
dt_mszaddon <- openxlsx2::read_xlsx(glue("{output_folder}cost_aggregations.xlsx"), sheet = "msz_addon")
setDT(dt_mszaddon)
dt_mszaddon[, age_cat := NULL]

dt_mszaddon_previous <- openxlsx2::read_xlsx(glue("output/iteration_3c/cost_aggregations.xlsx"), sheet = "msz_addon")
setDT(dt_mszaddon_previous)
cols_to_set_all <- c("doodsoorzaak", "age_cat", "geslacht", "inkomen_klasse",
                     "seswoa_cat", "migratie_achtergrond", "huishoudsamenstelling",
                     "stedgem", "wlz_start_period", "provincie", "used_any_acp_2years")
for (col in cols_to_set_all) {
  dt_mszaddon_previous <- dt_mszaddon_previous[get(col) == "all"]
  dt_mszaddon_previous[, (col) := NULL]
}

# dt_mszaddon <- dt_mszaddon[doodsoorzaak == "all"]

# NOTE: for msz addon, we changed the data to two years - i.e., the monthly figures should line up

check_grid <- as.data.table(unique(dt_mszaddon[, .SD, .SDcols = c("cohort", "died", "name", "type", "t", "bin_size")]))
check_grid <- check_grid[
  cohort == "2023" & t %in% -1 & bin_size == "30"
]

for (row_i in 1:nrow(check_grid)) {
  row <- check_grid[row_i]
  
  cohort_setting <- row$cohort
  died_setting <- row$died
  name_setting <- row$name
  type_setting <- row$type
  t_setting <- row$t
  bin_size_setting <- row$bin_size
  
  # browser()
  
  # assert equality
  # print(row)
  
  assert_that(abs(
    dt_mszaddon[
      cohort == cohort_setting & died == died_setting & name == name_setting & type == type_setting & t == t_setting & bin_size == bin_size_setting
        ]$value - 
      dt_mszaddon_previous[
        cohort == cohort_setting & died == died_setting & name == name_setting & type == type_setting & t == t_setting & bin_size == bin_size_setting
      ]$value
  ) < 10)
}



#### check equality among iterations: msz_prestaties ####
dt_mszprest <- openxlsx2::read_xlsx(glue("{output_folder}cost_aggregations.xlsx"), sheet = "msz_prestaties")
setDT(dt_mszprest)

dt_mszprest_previous <- openxlsx2::read_xlsx(glue("output/iteration_3c/cost_aggregations.xlsx"), sheet = "msz_prestaties")
setDT(dt_mszprest_previous)
cols_to_set_all <- c("doodsoorzaak", "age_cat", "geslacht", "inkomen_klasse",
                     "seswoa_cat", "migratie_achtergrond", "huishoudsamenstelling",
                     "stedgem", "wlz_start_period", "provincie", "used_any_acp_2years", 
                     "huisarts_consults_cat", "age_cat")
for (col in cols_to_set_all) {
  if (col %in% names(dt_mszprest_previous)) {
    
  dt_mszprest_previous <- dt_mszprest_previous[get(col) == "all"]
  dt_mszprest_previous[, (col) := NULL]
  }
  if (col %in% names(dt_mszprest)) {
    
    dt_mszprest <- dt_mszprest[get(col) == "all"]
    dt_mszprest[, (col) := NULL]
  }
}

check_grid <- as.data.table(unique(dt_mszprest[, .SD, .SDcols = c("cohort", "died", "name", "type", "t", "bin_size")]))
check_grid <- check_grid[
  name == "vektmszvergoedbedragzvw"
]

for (row_i in 1:nrow(check_grid)) {
  row <- check_grid[row_i]
  
  cohort_setting <- row$cohort
  died_setting <- row$died
  name_setting <- row$name
  type_setting <- row$type
  t_setting <- row$t
  bin_size_setting <- row$bin_size
  
  # browser()
  
  # assert equality
  # print(row)
  
  assert_that(abs(
    dt_mszprest[
      cohort == cohort_setting & died == died_setting & name == name_setting & type == type_setting & t == t_setting & bin_size == bin_size_setting
    ]$value - 
      dt_mszprest_previous[
        cohort == cohort_setting & died == died_setting & name == name_setting & type == type_setting & t == t_setting & bin_size == bin_size_setting
      ]$value
  ) < 10)
}



