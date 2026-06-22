# Project: Laatste 1000 dagen
# Author: Marco Griep

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
options(scipen = 999)


#### check whether the msz costs are equal for DBCs and OZPs combined, compared to the aggregated/binned costs ####
dt_zpk_counts <- as.data.table(openxlsx2::read_xlsx(glue(
  "{output_folder}zpk_categorieen_tellingen.xlsx"
)))

dt_msz_costs <- as.data.table(openxlsx2::read_xlsx(
  glue("{output_folder}cost_aggregations.xlsx"),
  sheet = "msz_prestaties"
  ))


demog_names <- c("age_cat", "geslacht", "inkomen_klasse", "seswoa_cat", 
"migratie_achtergrond", "huishoudsamenstelling","stedgem", "wlz_start_period", 
"provincie", "used_any_acp_2years")


test_row_msz_costs <- dt_msz_costs[
  died == "Overleden" & 
    cohort == "2023" & 
    bin_size == "1000" & 
    variable == "vektmszvergoedbedragzvw_sum_totaal_groep"
]

for (demog_var in demog_names) {
  test_row_msz_costs <- test_row_msz_costs[get(demog_var) == "all"]
}

test_rows_zpk_counts <- dt_zpk_counts[
  died == "Overleden" & 
    cohort == "2023" &
    bin_size == "1000d"& 
    vektmszsettingzpk == "all"
]
# assert costs equal
assert_that(abs(test_row_msz_costs$value - sum(test_rows_zpk_counts$sum_totaal_groep)) < 10)

# assert equal population size
assert_that(abs(test_row_msz_costs$n_totaal - test_rows_zpk_counts$n_totaal_population[1]) < 10)


#### match population counts ####
dt_regression <- as.data.table(openxlsx2::read_xlsx(glue(
  "{output_folder}regression_results.xlsx"
)))

population_2023_overleden_from_regression <- dt_regression[
  dependent_var == "vektmszvergoedbedragzvw_1000d"& 
    used_cohorts == "2023"
]$n_obs[1]

assert_that(abs(population_2023_overleden_from_regression - test_row_msz_costs$n_totaal) <= 2)

#### assert equality with iteration 1 ####
dt_wlz_iteration_1 <- openxlsx2::read_xlsx("output/iteration_1/all_output_final.xlsx",
                                           sheet = "wlz")
setDT(dt_wlz_iteration_1)

iteration_1_wlz_test <- dt_wlz_iteration_1[
  cohort == "2023" &
    died == "Overleden" &
    bin_size == "1000days" & 
    doodsoorzaak == "all"
]

current_wlz_costs <- openxlsx2::read_xlsx("output/iteration_3c/cost_aggregations.xlsx",                                        sheet = "wlz")
setDT(current_wlz_costs)

test_rows_wlz <- current_wlz_costs
for (demog_var in demog_names) {
  test_rows_wlz <- test_rows_wlz[
    died == "Overleden" & 
      cohort == "2023" &
      bin_size == "1000" &
      get(demog_var) == "all"
    ]
}

# assert equal costs
assert_that(abs(
  iteration_1_wlz_test[variable == "bedragwlzzin_sum_totaal_groep"]$value - 
    test_rows_wlz[variable == "bedragwlzzin_sum_totaal_groep"]$value
) < 1000)

# assert equal users
assert_that(abs(
  iteration_1_wlz_test[variable == "bedragwlzzin_n_totaal_gebruikers"]$value - 
    test_rows_wlz[variable == "bedragwlzzin_n_totaal_gebruikers"]$value
) < 1)


# also check for zvw
dt_zvw_iteration_1 <- openxlsx2::read_xlsx("output/iteration_1/all_output_final.xlsx",
                                           sheet = "zvw")
setDT(dt_zvw_iteration_1)

iteration_1_zvw_test <- dt_zvw_iteration_1[
  cohort == "2023" &
    died == "Overleden" &
    bin_size == "1000days" & 
    doodsoorzaak == "all"
]

current_zvw_costs <- openxlsx2::read_xlsx("output/iteration_3c/cost_aggregations.xlsx", 
                                          sheet = "zvw")
setDT(current_zvw_costs)

test_rows_zvw <- current_zvw_costs
for (demog_var in demog_names) {
  test_rows_zvw <- test_rows_zvw[
    died == "Overleden" & 
      cohort == "2023" &
      bin_size == "1000" &
      get(demog_var) == "all"
     & doodsoorzaak == "all"
  ]
}

# assert equal costs
zvw_cost_variable_names <- grep("sum_totaal_groep", unique(iteration_1_zvw_test$variable), value=T)

for (zvw_cost_variable in zvw_cost_variable_names) {
  assert_that(abs(
    iteration_1_zvw_test[variable == zvw_cost_variable]$value - 
      test_rows_zvw[variable == zvw_cost_variable]$value
  ) < 1000)
  
  # assert equal users
  assert_that(abs(
    iteration_1_zvw_test[variable == zvw_cost_variable]$value - 
      test_rows_zvw[variable == zvw_cost_variable]$value
  ) < 1)
}

# NOTES: MSZ ITERATION 3 DOES NOT LINE UP WITH ITERATION 1; iteration 3 has higher costs, and higher n users, for cohort 2023 1000days, overleden. Same n_totaal, though.
# Reason found: the rinpersoon class mismatch again in iteration 1
# For wlz costs, this issue was not present in iteration 1
# Neither for wvp
# Neither for zvw

dt_msz_iteration_1 <- openxlsx2::read_xlsx("output/iteration_1/all_output_final.xlsx",
                                           sheet = "msz_prestaties")
setDT(dt_msz_iteration_1)

iteration_1_msz_test <- dt_msz_iteration_1[
  cohort == "2023" &
    died == "Overleden" &
    bin_size == "1000days" & 
    doodsoorzaak == "all"
]

current_msz_costs <- openxlsx2::read_xlsx("output/iteration_3c/cost_aggregations.xlsx", sheet = "msz_prestaties")
setDT(current_msz_costs)

test_rows_msz <- current_msz_costs
for (demog_var in demog_names) {
  test_rows_msz <- test_rows_msz[
    died == "Overleden" & 
      cohort == "2023" &
      bin_size == "1000" &
      get(demog_var) == "all"
  ]
}

# print diff in costs (perc change)
print(
  (test_rows_msz[variable == "vektmszvergoedbedragzvw_sum_totaal_groep"]$value - 
  iteration_1_msz_test[variable == "vektmszvergoedbedragzvw_sum_totaal_groep"]$value)
  / iteration_1_msz_test[variable == "vektmszvergoedbedragzvw_sum_totaal_groep"]$value
)

# print diff in users (perc change)
print(
  (test_rows_msz[variable == "vektmszvergoedbedragzvw_n_totaal_gebruikers"]$value - 
     iteration_1_msz_test[variable == "vektmszvergoedbedragzvw_n_totaal_gebruikers"]$value)
  / iteration_1_msz_test[variable == "vektmszvergoedbedragzvw_n_totaal_gebruikers"]$value
)

#### check top50 activiteiten ####
top50_activiteiten <- openxlsx2::read_xlsx("output/iteration_3c/top50_activiteiten.xlsx")

#### check zpk categories ####
zpk_categories_counts <- openxlsx2::read_xlsx("output/iteration_3c/zpk_categorieen_tellingen.xlsx")
setDT(zpk_categories_counts)
# do a quick manual recount, to make sure the aggregations are being performed correctly
dt_mszprest_DBC_overlapped <- r_parquet_get_dt("data/processed/mszprest_DBC_overlapped.parquet")
setDT(dt_mszprest_DBC_overlapped)
# In leven, oper_verr, t == -5, setting == 1, cohort 2019
subset_row <- zpk_categories_counts[
  died == "In leven" & zpk_category == "oper_verr" & t == -5 & 
    vektmszsettingzpk == "1" & cohort == "2019"
]

subset_dbc_overlap <- dt_mszprest_DBC_overlapped[
  died == "In leven" & zpk_category == "oper_verr" & t == -5 & vektmszsettingzpk == "1" & cohort == "2019"
]

assert_that(abs(
  subset_row$n_totaal_declaraties - nrow(subset_dbc_overlap)
) < 10)

assert_that(abs(
  subset_row$n_totaal_gebruikers - uniqueN(subset_dbc_overlap$sample_id)
) < 10)

assert_that(abs(
  subset_row$sum_totaal_groep - sum(as.numeric(subset_dbc_overlap$vektmszvergoedbedragzvw), na.rm=T)
) < 10)

assert_that(abs(
  subset_row$median_cost_per_declaratie - median(as.numeric(subset_dbc_overlap$vektmszvergoedbedragzvw), na.rm=T)
) < 10)

rm(dt_mszprest_DBC_overlapped, subset_dbc_overlap)
gc()

# do the same for OZPs
# do a quick manual recount, to make sure the aggregations are being performed correctly
dt_mszprest_OZP_overlapped <- r_parquet_get_dt("data/processed/mszprest_OZP_overlapped.parquet")
setDT(dt_mszprest_OZP_overlapped)
# Overleden, ovg_zpk, t == -5, setting == 1, cohort 2023
subset_row <- zpk_categories_counts[
  died == "Overleden" & zpk_category == "ovg_zpk" & t == -5 & vektmszsettingzpk == "1" & cohort == "2023" & prestatie_type == "OZP"
]

subset_OZP_overlap <- dt_mszprest_OZP_overlapped[
  died == "Overleden" & zpk_category == "ovg_zpk" & t == -5 & vektmszsettingzpk == "1" & cohort == "2023"
]

assert_that(abs(
  subset_row$n_totaal_declaraties - nrow(subset_OZP_overlap)
) < 10)

assert_that(abs(
  subset_row$n_totaal_gebruikers - uniqueN(subset_OZP_overlap$sample_id)
) < 10)

assert_that(abs(
  subset_row$sum_totaal_groep - sum(as.numeric(subset_OZP_overlap$vektmszvergoedbedragzvw), na.rm=T)
) < 10)

assert_that(abs(
  subset_row$median_cost_per_declaratie - median(as.numeric(subset_OZP_overlap$vektmszvergoedbedragzvw), na.rm=T)
) < 10)

rm(dt_mszprest_OZP_overlapped, subset_OZP_overlap)
gc()
