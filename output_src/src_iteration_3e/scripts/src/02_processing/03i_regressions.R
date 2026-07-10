rm(list=ls())
gc()

source("src/00_inputs.R")
options(scipen = 999)

#### functions ####

create_linear_regression <- function(regression_dt, indep_vars, dep_var, cohorts_used, description_indep_vars = NULL) {
  
  # browser()
  
  
  if (nrow(regression_dt[!is.na(get(dep_var))]) == 0) {
    return(data.table())
  }
  
  formula_str <- paste(dep_var, "~", paste0(indep_vars, collapse = " + "))
  form <- as.formula(formula_str)
  caught_warnings <- c()
  
  # test before capturing, to print errors
  test <- feols(
    form, 
    regression_dt,
    se = "hetero"
  )

  console_output <- capture.output({
    model <- withCallingHandlers(
      expr = {
        feols(
          form, 
          regression_dt,
          se = "hetero"
        )
      },
      warning = function(w) {
        caught_warnings <<- c(caught_warnings, w$message)
        invokeRestart("muffleWarning")
      }
    )
  }, type = "message")
  
  res_dt <- as.data.table(coeftable(model), keep.rownames = "variable")
  res_dt[, dependent_var := dep_var]
  res_dt[, used_cohorts := cohorts_used]
  res_dt[, description_indep_vars := description_indep_vars]
  res_dt[, warnings := paste(c(console_output, caught_warnings), collapse = "; ")]
  res_dt[, n_obs := nrow(regression_dt)]
  
  return(res_dt)
}

#### clean and transform table ####

# load dt_regressions
dt_regression <- r_parquet_get_dt("data/processed/regression_table.parquet")

# convert province col to their names
replace_values_by_haven_labels(
  dt_regression, 
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
  )

# independent_vars, and their reference values
independent_vars <- list(
  'cohort' = "2019", 
  'doodsoorzaak' = "Overig",
  'seswoa_cat' = "35-50%",
  'migratie_achtergrond' = "NL", 
  'geslacht' = "Vrouwen", 
  'age_cat' = "2", 
  'burgstaat' = "Gehuwd of geregist. partnerschap", 
  'stedgem' = "Niet (<500 omgevingsadressen/km2)", 
  'inkomen_klasse' = "tot_120", 
  'huishoudsamenstelling' = "1persoons_hh", 
  'wlz_start_period' = "Nooit WLZ",
  'used_any_acp_2years'= "0",
  # 'used_acp_31244_2years'= "0",
  # 'used_acp_31381_2years'= "0",
  'provincie'  = "Zuid-Holland",
  'huisarts_consults_cat' = "moderate (10-19 consults in past two years)" 
  )

# set reference value
for (indep_var in names(independent_vars)) {
  relevel_value <- independent_vars[[indep_var]]
  dt_regression <- dt_regression[, (indep_var) := relevel(as.factor(get(indep_var)), relevel_value)]
}

# create usage cols 
cost_cols <- setdiff(names(dt_regression), names(independent_vars))

for (cost_col in cost_cols) {
  usage_col_name <- paste0(cost_col, "_gebruik")
  dt_regression[, (usage_col_name) := as.numeric(ifelse(get(cost_col) > 0, 1, 0))]
}
cost_cols <- c(cost_cols, paste0(cost_cols, "_gebruik"))



# create regressions by cost type

regression_results <- rbindlist(lapply(cost_cols, function(cost_col) {
  
  dt_regression_temp <- copy(dt_regression)
  
  # for the oncolytica, only use doodsoorzaak palliatief kanker
  if (grepl("13_01_88", cost_col)) dt_regression_temp <- dt_regression_temp[doodsoorzaak == "Palliatief kanker"]

  # firstly, the general one for seperate cohorts
  results_2019 <- create_linear_regression(
    dt_regression_temp[cohort == "2019"], 
    indep_vars = c('doodsoorzaak', 
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period', 'provincie'),
    dep_var = cost_col,
    cohorts_used = "2019",
    description_indep_vars = "all vars"
    )
  
  # add acp var to 2023
  results_2023_acp <- create_linear_regression(
    dt_regression_temp[cohort == "2023"], 
    indep_vars = c('doodsoorzaak', 
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period', 'provincie', 
                   'used_any_acp_2years', 'huisarts_consults_cat'),
    dep_var = cost_col,
    cohorts_used = "2023",
    description_indep_vars = "all vars incl ACP"
  )
  
  # and one for 2023 without ACP
  results_2023 <- create_linear_regression(
    dt_regression_temp[cohort == "2023"], 
    indep_vars = c('doodsoorzaak', 
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period', 'provincie', 'huisarts_consults_cat'),
    dep_var = cost_col,
    cohorts_used = "2023",
    description_indep_vars = "all vars excl ACP"
  )
  
  # create one combined
  results_combined <- create_linear_regression(
    dt_regression_temp, 
    indep_vars = c('cohort', 'doodsoorzaak',
                   'migratie_achtergrond', 
                   'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                   'huishoudsamenstelling', 'wlz_start_period', 'provincie'),
    dep_var = cost_col,
    cohorts_used = "both",
    description_indep_vars = "all vars"
  )
  
  # create costs per user
  if (!grepl("gebruik", cost_col)) {
    new_cost_col_name <- glue("{cost_col}_per_user")
    setnames(dt_regression_temp, cost_col, new_cost_col_name)
    
    results_cost_per_user_both <- create_linear_regression(
      dt_regression_temp[get(new_cost_col_name) > 0], 
      indep_vars = c('cohort', 'doodsoorzaak',
                     'migratie_achtergrond', 
                     'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                     'huishoudsamenstelling', 'wlz_start_period', 'provincie'),
      dep_var = new_cost_col_name,
      cohorts_used = "both",
      description_indep_vars = "all vars"
    )
    
    results_cost_per_user_2019 <- create_linear_regression(
      dt_regression_temp[get(new_cost_col_name) > 0 & cohort == "2019"], 
      indep_vars = c('cohort', 'doodsoorzaak',
                     'migratie_achtergrond', 
                     'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                     'huishoudsamenstelling', 'wlz_start_period', 'provincie'),
      dep_var = new_cost_col_name,
      cohorts_used = "2019",
      description_indep_vars = "all vars"
    )
    
    # one with ACP
    results_cost_per_user_2023_acp <- create_linear_regression(
      dt_regression_temp[get(new_cost_col_name) > 0 & cohort == "2023"], 
      indep_vars = c('cohort', 'doodsoorzaak',
                     'migratie_achtergrond', 
                     'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                     'huishoudsamenstelling', 'wlz_start_period', 'provincie', 
                     'used_any_acp_2years', 'huisarts_consults_cat'),
      dep_var = new_cost_col_name,
      cohorts_used = "2023",
      description_indep_vars = "all vars"
    )
    
    # one without
    results_cost_per_user_2023 <- create_linear_regression(
      dt_regression_temp[get(new_cost_col_name) > 0 & cohort == "2023"], 
      indep_vars = c('cohort', 'doodsoorzaak',
                     'migratie_achtergrond', 
                     'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                     'huishoudsamenstelling', 'wlz_start_period', 'provincie',
                     'huisarts_consults_cat'),
      dep_var = new_cost_col_name,
      cohorts_used = "2023",
      description_indep_vars = "all vars excl ACP"
    )
    
    return(rbindlist(list(results_2019, results_2023, results_2023_acp, results_combined, 
                          results_cost_per_user_both, results_cost_per_user_2019, 
                          results_cost_per_user_2023_acp, results_cost_per_user_2023), ignore.attr=T))
    
  } 
  return(rbindlist(list(results_2019, results_2023, results_2023_acp, results_combined), ignore.attr=T))
  
}))

# create one regression, where used_acp is the dependent var
dt_regression[, used_any_acp_2years := as.integer(used_any_acp_2years)]
results_acp_2023 <- create_linear_regression(
  dt_regression[cohort == "2023"],
  indep_vars = c('cohort', 'doodsoorzaak',
                 'migratie_achtergrond', 
                 'geslacht', 'age_cat', 'burgstaat', 'stedgem', 'inkomen_klasse', 
                 'huishoudsamenstelling', 'wlz_start_period', 'provincie', 'huisarts_consults_cat'),
  dep_var = "used_any_acp_2years",
  cohorts_used = "2023",
  description_indep_vars = "all"
)

# write
openxlsx::write.xlsx(rbindlist(list(regression_results, results_acp_2023), ignore.attr=TRUE), glue("{output_folder}regression_results.xlsx"))

