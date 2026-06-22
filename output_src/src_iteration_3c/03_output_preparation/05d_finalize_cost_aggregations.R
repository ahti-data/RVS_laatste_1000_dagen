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

# To do:
# Delete all raw files after it is done
# drop VEKTMSZZorgtype VEKTMSZDeclaratiecode
# Drop heeft_zpk_5

wb <- openxlsx::createWorkbook()

agg_costs_path <-'data/processed/aggregated_costs/' 

#### Second, get the costs and usage data ####
# Wlz
wlz <- as.data.table(rio::import(glue('{agg_costs_path}agg_wlz.csv')))

# Keep aggregation in total and by Wlz usage
wlz <- wlz[doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
             inkomen_klasse == 'all' & seswoa_cat == 'all' & 
             migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
             stedgem == 'all']

replace_values_by_haven_labels(
  wlz,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'wlz')
openxlsx::writeData(wb, 'wlz', wlz)
rm(wlz)


# Wlz corrected
wlz_corrected <- as.data.table(rio::import(glue('{agg_costs_path}agg_wlz_corrected.csv')))

# Keep aggregation only in total
wlz_corrected <- wlz_corrected[
  doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
                                 inkomen_klasse == 'all' & seswoa_cat == 'all' & 
                                 migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
                                 stedgem == 'all' & wlz_start_period == 'all' & 
                                 provincie == 'all' & used_any_acp_2years == 'all']

openxlsx::addWorksheet(wb, 'wlz_corrected')
openxlsx::writeData(wb, 'wlz_corrected', wlz_corrected)
rm(wlz_corrected)


# ZVW
zvw <- as.data.table(rio::import(glue('{agg_costs_path}agg_zvw.csv')))
zvw <- zvw[bin_size == 1000]

replace_values_by_haven_labels(
  zvw,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'zvw')
openxlsx::writeData(wb, 'zvw', zvw)
rm(zvw)

# ZVW corrected
zvw_corrected <- as.data.table(rio::import(glue('{agg_costs_path}agg_zvw_corrected.csv')))
zvw_corrected <- zvw_corrected[bin_size == 1000]

# Keep aggregation only in total
zvw_corrected <- zvw_corrected[
  doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
                                 inkomen_klasse == 'all' & seswoa_cat == 'all' & 
                                 migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
                                 stedgem == 'all' & wlz_start_period == 'all' & 
                                 provincie == 'all' & used_any_acp_2years == 'all']

openxlsx::addWorksheet(wb, 'zvw_corrected')
openxlsx::writeData(wb, 'zvw_corrected', zvw_corrected)
rm(zvw_corrected)


# MSZ
msz_prestaties <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_prestaties.csv')))

# Keep splits by all characteristics, except for the cause of death
msz_prestaties <- msz_prestaties[doodsoorzaak == 'all']

replace_values_by_haven_labels(
  msz_prestaties,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'msz_prestaties')
openxlsx::writeData(wb, 'msz_prestaties', msz_prestaties)
rm(msz_prestaties)

# MSZ corrected
msz_prestaties_corrected <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_prestaties_corrected.csv')))

# Keep aggregation only in total
msz_prestaties_corrected <- msz_prestaties_corrected[
  doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
    inkomen_klasse == 'all' & seswoa_cat == 'all' & 
    migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
    stedgem == 'all' & wlz_start_period == 'all' & provincie == 'all' & used_any_acp_2years == 'all']
openxlsx::addWorksheet(wb, 'msz_prestaties_corrected')
openxlsx::writeData(wb, 'msz_prestaties_corrected', msz_prestaties_corrected)
rm(msz_prestaties_corrected)

# MSZ prestatie diagnostics
msz_prestaties_diag <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_prestatie_diagnostiek.csv')))

# Keep pnly total of 1000 days and the last month 
msz_prestaties_diag <- msz_prestaties_diag[t == -1]

# Drop several outcomes
msz_prestaties_diag <- msz_prestaties_diag[!grepl(
  "^(kosten_heup_0303|n_heup_0303|kosten_heup_0305|n_heup_0305)", name, ignore.case = T)]

replace_values_by_haven_labels(
  msz_prestaties_diag,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'msz_prestaties_diag')
openxlsx::writeData(wb, 'msz_prestaties_diag', msz_prestaties_diag)
rm(msz_prestaties_diag)


# MSZ activit diagnostics
msz_activit_diag <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_activit_diagnostiek.csv')))

# Keep only total of 1000 days and the last month 
msz_activit_diag <- msz_activit_diag[t == -1]

replace_values_by_haven_labels(
  msz_activit_diag,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'msz_activit_diag')
openxlsx::writeData(wb, 'msz_activit_diag', msz_activit_diag)
rm(msz_activit_diag)

# MSZ add-on oncology total who died from cancer
msz_addon_oncology_total_cancer <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_addon_oncology_total_cancer.csv')))

# Keep only deceased, no control group
msz_addon_oncology_total_cancer <- msz_addon_oncology_total_cancer[died == 'Overleden']

# Notice that I need to keep doodsoorzaak == 'all' as this sample only include 
# people who died from cancer
msz_addon_oncology_total_cancer <- msz_addon_oncology_total_cancer[
  doodsoorzaak == 'all' & geslacht == 'all' & seswoa_cat == 'all' & 
    migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
    stedgem == 'all' & wlz_start_period == 'all']

# Change doodsoorzaak from 'all' to 'Palliatief kanker' 
msz_addon_oncology_total_cancer[doodsoorzaak == 'all', doodsoorzaak := 'Palliatief kanker']

replace_values_by_haven_labels(
  msz_addon_oncology_total_cancer,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'msz_addon_oncology_total_cancer')
openxlsx::writeData(wb, 'msz_addon_oncology_total_cancer', msz_addon_oncology_total_cancer)
rm(msz_addon_oncology_total_cancer)


# MSZ add-on oncology separately among those who died from cancer
msz_addon_oncology_cancer <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_addon_oncology_cancer.csv')))

# Keep only deceased, no control group
msz_addon_oncology_cancer <- msz_addon_oncology_cancer[died == 'Overleden']

# Change doodsoorzaak from 'all' to 'Palliatief kanker' 
msz_addon_oncology_cancer[doodsoorzaak == 'all', doodsoorzaak := 'Palliatief kanker']

replace_values_by_haven_labels(
  msz_addon_oncology_cancer,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'msz_addon_oncology_cancer')
openxlsx::writeData(wb, 'msz_addon_oncology_cancer', msz_addon_oncology_cancer)
rm(msz_addon_oncology_cancer)

# MSZ non-oncology
msz_addon <- as.data.table(rio::import(glue('{agg_costs_path}agg_msz_addon.csv')))

# Drop oncology outcomes
#msz_addon <- msz_addon[!grepl('13', substr(name, pmax(2, nchar(name) - 4), nchar(name)))]

# Keep pnly total of 1000 days and the last month 
msz_addon <- msz_addon[t == -1]

replace_values_by_haven_labels(
  msz_addon,   
  "G:/Maatwerk/STAPELINGSMONITOR/2023/Stapelingsmonitor2023V2.sav",
  "provincie", 
  format = T
)

openxlsx::addWorksheet(wb, 'msz_addon')
openxlsx::writeData(wb, 'msz_addon', msz_addon)
rm(msz_addon)

openxlsx::saveWorkbook(wb, glue::glue('{output_folder}cost_aggregations.xlsx'), overwrite = T)

