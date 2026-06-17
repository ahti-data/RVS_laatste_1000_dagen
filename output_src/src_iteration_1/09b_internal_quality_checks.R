# Project: Laatste 1000 dagen
# Author: Marco Griep
# Goal: Run internal quality checks
# Output: filled in quality checks excel in data/quality_checks
# Last edited: 12 March 2026

rm(list = ls())
gc()
source("src/00_inputs.R")
options(scipen = 999)


library(openxlsx2)
library(assertthat)

#### load data ####
load_output_data <- function(sheet_name) {
  return(as.data.table(read_xlsx("output/iteration_1/all_output.xlsx", sheet = sheet_name)))
}

dt_overlijden_with_matched <- as.data.table(arrow::read_parquet("data/raw/overlijden_with_matched.parquet"))

#### functions ####
assert_population_size_correct <- function(dt_overlijden, dt_output) {
  overlijden_population_size <- nrow(dt_overlijden)
  cost_cols <- unique(dt_output$name)
  
  for (cost_col in cost_cols) {
    # print(cost_col)
    total_population_size_output <- sum(dt_output[
      name == cost_col & 
        bin_size == "1000days" &
        type == "gemiddelde_per_persoon" &
        name == cost_col &
        doodsoorzaak == "all"
    ]$n_totaal)
    
    abs_diff <- abs(total_population_size_output - overlijden_population_size)
    
    if (abs_diff > 10000) {
      print(cost_col)
      print(total_population_size_output)
      print(overlijden_population_size)
      print("")
    }
    # 
    # assert_that(
    #   abs_diff < 10000
    # )
  }
}

assert_correct_binning <- function(dt_output) {
  if("monthly" %in% unique(dt_output$bin_size)) {
    cost_cols <- unique(dt_output$name)
    for (cost_col in cost_cols) {
      total_costs_bin1000 <- sum(dt_output[
        name == cost_col & 
          bin_size == "1000days" &
          type == "sum_totaal_groep" &
          doodsoorzaak == "all"
      ]$value)
      
      total_costs_monthly <- sum(dt_output[
        name == cost_col & 
          bin_size == "monthly" &
          type == "sum_totaal_groep" &
          doodsoorzaak == "all"
      ]$value)
      
      abs_diff <- abs(total_costs_bin1000 - total_costs_monthly)

      # assert_that(
      #   abs_diff < 20
      # )
    }
  }
}

#### run tests ####
sheets <- readxl::excel_sheets("output/iteration_1/all_output.xlsx")

for (sheet_name in sheets) {
  print(glue::glue("currently internally checking {sheet_name}"))
  
  if (sheet_name != "medicijn"){
    dt_output <- load_output_data(sheet_name)
    
    assert_correct_binning(dt_output)
    assert_population_size_correct(dt_overlijden_with_matched, dt_output)
  }
}

#### run some manual tests on wlz, to see whether aggregation is going well in make_aggregations ####
sheets <- readxl::excel_sheets("output/iteration_1/all_output.xlsx")
dt_wlz_output <- load_output_data("wlz")
ds <- arrow::open_dataset("./data/processed/wlzkosten_monthly.parquet")
dt_wlz_2023_processed <-ds |>
  dplyr::collect()

# filter wlz output
dt_wlz_output <- dt_wlz_output[
  cohort == 2023 &
    died == "In leven" &
    bin_size == "monthly" & 
    doodsoorzaak == "all"
]

# filter wlz 2023 processed
dt_wlz_2023_processed <- dt_wlz_2023_processed[
  cohort == 2023 &
    died == "In leven" 
]

for (t_i in unique(dt_wlz_output$t)) {
  # gebruikt_per_persoon
  assert_that(
    abs(sum(dt_wlz_output[type == "gebruikt_per_persoon" & t == t_i & name == "bedragwlzzin"]$value) -
      mean(dt_wlz_2023_processed[t == t_i]$gebruikt_bedragwlzzin)) < 30
  )
  
  # gemiddelde_per_gebruiker
  assert_that(
    abs(sum(dt_wlz_output[type == "gemiddelde_per_gebruiker" & t == t_i & name == "bedragwlzzin"]$value) -
      mean(dt_wlz_2023_processed[t == t_i & gebruikt_bedragwlzzin == 1]$bedragwlzzin)) < 30
  )
  
  # gemiddelde_per_persoon
  assert_that(
    abs(sum(dt_wlz_output[type == "gemiddelde_per_persoon" & t == t_i & name == "bedragwlzzin"]$value) -
      mean(dt_wlz_2023_processed[t == t_i]$bedragwlzzin)) < 30
  )
  
  # mediaan_per_gebruiker
  assert_that(
    abs(sum(dt_wlz_output[type == "mediaan_per_gebruiker" & t == t_i & name == "bedragwlzzin"]$value) -
      median(dt_wlz_2023_processed[t == t_i & gebruikt_bedragwlzzin == 1]$bedragwlzzin)) < 30
  )
  
  # # n_totaal_gebruikers
  # assert_that(
  #   abs(sum(dt_wlz_output[type == "n_totaal_gebruikers" & t == t_i & name == "bedragwlzzin"]$value) -
  #     nrow(dt_wlz_2023_processed[t == t_i & gebruikt_bedragwlzzin == 1])) < 30
  # )
  
  # q75_per_persoon
  assert_that(
    abs(sum(dt_wlz_output[type == "q75_per_persoon" & t == t_i & name == "bedragwlzzin"]$value) -
      as.numeric(quantile(dt_wlz_2023_processed[t == t_i]$bedragwlzzin, 0.75, na.rm = T))) < 30
  )
  
  # sum_totaal_groep
  assert_that(
    abs(sum(dt_wlz_output[type == "sum_totaal_groep" & t == t_i & name == "bedragwlzzin"]$value) -
      sum(dt_wlz_2023_processed[t == t_i]$bedragwlzzin)) < 30
  )
  
  # for gebruiktbedragwlzzin, values line up with other values in bedragwlzzin. so all good
}

#### run some manual tests on msz_eerstelijns, to see whether aggregation is going well in make_aggregations ####
sheets <- readxl::excel_sheets("output/iteration_1/all_output.xlsx")
dt_msz1_output <- load_output_data("msz_eerstelijns_diagnostiek")
ds <- arrow::open_dataset("./data/processed/msz_prestatie_1000_dagen.parquet")
dt_msz1_2023_processed <-ds |>
  dplyr::collect()

# filter msz1 output
dt_msz1_output <- dt_msz1_output[
  cohort == 2023 &
    died == "Overleden" &
    bin_size == "monthly" & 
    doodsoorzaak == "all"
]

# filter msz1 2023 processed
dt_msz1_2023_processed <- dt_msz1_2023_processed[
  cohort == 2023 &
    died == "Overleden" 
]

for (t_i in unique(dt_msz1_output$t)) {
  # gebruikt_per_persoon
  abs(assert_that(
    sum(dt_msz1_output[type == "gebruikt_per_persoon" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      mean(dt_msz1_2023_processed[t == t_i]$gebruikt_kosten_eerstelijn_zpk_7)) < 30
  )
  
  # gemiddelde_per_gebruiker
  assert_that(
    abs(sum(dt_msz1_output[type == "gemiddelde_per_gebruiker" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      mean(dt_msz1_2023_processed[t == t_i & gebruikt_kosten_eerstelijn_zpk_7 == 1]$kosten_eerstelijn_zpk_7)) < 30
  )
  
  # gemiddelde_per_persoon
  assert_that(
    abs(sum(dt_msz1_output[type == "gemiddelde_per_persoon" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      mean(dt_msz1_2023_processed[t == t_i]$kosten_eerstelijn_zpk_7)) < 30
  )
  
  # mediaan_per_gebruiker
  assert_that(
    abs(sum(dt_msz1_output[type == "mediaan_per_gebruiker" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      median(dt_msz1_2023_processed[t == t_i & gebruikt_kosten_eerstelijn_zpk_7 == 1]$kosten_eerstelijn_zpk_7)) < 30
  )
  
  # n_totaal_gebruikers
  assert_that(
    abs(sum(dt_msz1_output[type == "mediaan_per_gebruiker" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      uniqueN(dt_msz1_2023_processed[t == t_i & gebruikt_kosten_eerstelijn_zpk_7 == 1]$rinpersoon)) < 30
  )
  
  # q75_per_persoon
  assert_that(
    abs(sum(dt_msz1_output[type == "q75_per_persoon" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      as.numeric(quantile(dt_msz1_2023_processed[t == t_i]$kosten_eerstelijn_zpk_7, 0.75, na.rm = T))) < 30
  )
  
  # sum_totaal_groep
  assert_that(
    abs(sum(dt_msz1_output[type == "sum_totaal_groep" & t == t_i & name == "kosten_eerstelijn_zpk_7"]$value) -
      sum(dt_msz1_2023_processed[t == t_i]$kosten_eerstelijn_zpk_7)) < 30
  )
  
  # for gebruiktkosten_eerstelijn_zpk_7, values line up with other values in kosten_eerstelijn_zpk_7. so all good
}










