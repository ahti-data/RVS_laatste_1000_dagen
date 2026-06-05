rm(list=ls())
gc()

source("src/00_inputs.R")
options(scipen = 999)

#### TODO ####

#### functions ####

create_linear_regression <- function(regression_dt, indep_vars, dep_var, population_name) {
  formula_str <- paste(dep_var, "~", paste0(indep_vars, collapse = " + "))
  form <- as.formula(formula_str)
  
  model <- feols(
    form, 
    regression_dt
  )
  
  res_dt <- as.data.table(coeftable(model), keep.rownames = "variable")
  res_dt[, dependent_var := paste(dep_var, population_name, sep = "_")]
  res_dt[, n_obs := nrow(regression_dt)]
  
  return(res_dt)
}

#### clean and transform table ####

# load dt_regressions
dt_regression <- r_parquet_get_dt("data/processed/regression_table.parquet")

# independent_vars, and their reference values
independent_vars <- list(
  'cohort' = "2019", 
  'doodsoorzaak' = "Palliatief dementie",
  'seswoa_cat' = "35-50%",
  'migratie_achtergrond' = "NL", 
  'geslacht' = "Vrouwen", 
  'age_cat' = "2", 
  'burgstaat' = "Gehuwd of geregist. partnerschap", 
  'stedgem' = "Niet (<500 omgevingsadressen/km2)", 
  'inkomen_klasse' = "tot_120", 
  'huishoudsamenstelling' = "1persoons_hh", 
  'wlz_start_period' = "Nooit WLZ"
  )

cost_cols <- setdiff(names(dt_regression), names(independent_vars))

# create usage cols 
for (cost_col in cost_cols) {
  usage_col_name <- paste0(cost_col, "_gebruik")
  dt_regression[, (usage_col_name) := as.numeric(ifelse(get(cost_col) > 0, 1, 0))]
}
cost_cols <- c(cost_cols, paste0(cost_cols, "_gebruik"))

# set all independent vars as factors
# dt_regression <- dt_regression[, (names(independent_vars)) := lapply(.SD, as.factor) , .SDcols = independent_vars]

for (indep_var in names(independent_vars)) {
  relevel_value <- independent_vars[[indep_var]]
  dt_regression <- dt_regression[, (indep_var) := relevel(as.factor(get(indep_var)), relevel_value)]
}
# create regressions by cost type

regression_results <- rbindlist(lapply(cost_cols, function(cost_col) {
  dt_regression_temp <- copy(dt_regression)
  
  if (cost_col == "bedrag_13_01_88") dt_regression_temp <- dt_regression_temp[doodsoorzaak == "Palliatief kanker"]

  # firstly, the general one for seperate cohorts
  results_2019 <- create_linear_regression(
    dt_regression_temp[cohort == "2019"], 
    indep_vars = c('doodsoorzaak', 
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period'),
    dep_var = cost_col,
    population_name = "2019"
    )
  
  results_2023 <- create_linear_regression(
    dt_regression_temp[cohort == "2023"], 
    indep_vars = c('doodsoorzaak', 
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period'),
    dep_var = cost_col,
    population_name = "2023"
  )
  
  # create one combined
  results_combined <- create_linear_regression(
    dt_regression_temp, 
    indep_vars = c('cohort', 'doodsoorzaak',
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period'),
    dep_var = cost_col,
    population_name = "combined"
  )
  
  # create costs per user
  if (!grepl("gebruik", cost_col)) {
    results_cost_per_user <- create_linear_regression(
      dt_regression_temp[get(cost_col) > 0], 
      indep_vars = c('cohort', 'doodsoorzaak',
                     'migratie_achtergrond', 
                     'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                     'huishoudsamenstelling', 'wlz_start_period'),
      dep_var = cost_col,
      population_name = "_per_user"
    )
    
    return(rbindlist(list(results_2019, results_2023, results_combined, results_cost_per_user)))
    
  } 
  
  return(rbindlist(list(results_2019, results_2023, results_combined)))
  
}))

openxlsx::write.xlsx(regression_results, "output/iteration_3b/regression_results.xlsx")
