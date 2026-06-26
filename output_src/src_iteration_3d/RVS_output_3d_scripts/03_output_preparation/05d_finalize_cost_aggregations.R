# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Bind all results for the output
# Output: Final output for export
# Last edited: 22 April 2026

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(ggplot2)
library(dplyr)
options(scipen = 999)


datasets_to_output <- list(
  # WE DO NOT USE wijkverpleging
  # NOTE: doodsoorzaak, cohort, and died are always split by default, do not include in demog_vars_to_split
  'huisartsdecltab',
  'msz_prestatie_diagnostiek',
  # 'msz_activit_diagnostiek',
  'msz_addon',
  'msz_addon_oncology_total',
  'msz_addon_oncology_total_cancer',
  'msz_addon_oncology_cancer',
  'msz_prestaties'
  # 'msz_prestaties_corrected',
  # 'wlz',
  # 'wlz_corrected',
  # 'zvw',
  # 'zvw_corrected',
)

# To do:
# Delete all raw files after it is done
# drop VEKTMSZZorgtype VEKTMSZDeclaratiecode
# Drop heeft_zpk_5

wb <- openxlsx::createWorkbook()

agg_costs_path <-'data/processed/aggregated_costs/' 

for (dataset_name in datasets_to_output) {
  dataset <- as.data.table(rio::import(glue('{agg_costs_path}agg_{dataset_name}.csv')))
  
  if (dataset_name == "msz_prestaties") {
    # for msz prestaties, we don't want huisarts consults split for the hartkleppen 
    dataset <- dataset[!(name != "vektmszvergoedbedragzvw" & huisarts_consults_cat != "all")]
    
    # same for used_acp
    dataset <- dataset[!(name != "vektmszvergoedbedragzvw" & used_any_acp_2years != "all")] # TODO: make sure this isn't removing 2019 rows
  }
  
  openxlsx::addWorksheet(wb, dataset_name)
  openxlsx::writeData(wb, dataset_name, dataset)
}

openxlsx::saveWorkbook(wb, glue::glue('{output_folder}cost_aggregations.xlsx'), overwrite = T)

