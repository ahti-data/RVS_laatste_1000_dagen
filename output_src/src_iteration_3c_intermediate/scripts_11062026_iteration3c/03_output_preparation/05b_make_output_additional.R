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

path <- glue("./output/iteration_2/")
dir.create(path, recursive = T, showWarnings = F)
wb <- openxlsx::createWorkbook()


#### First, get lists of top 20 codes ####
top_20_codes_operatie <- './output/iteration_2/top_20_codes_operatie.xlsx'
top_20_codes_operatie_1000 <- readxl::read_excel(top_20_codes_operatie, 
                                                 sheet = 'top_20_codes_1000_dagen')
top_20_codes_operatie_30 <- readxl::read_excel(top_20_codes_operatie, 
                                               sheet = 'top_20_codes_30_dagen')

# Add names
dbc_names <- rio::import(
  'K:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/codelijst variabeleVEKTMSZDBCZorgproduct.xlsx')
names(dbc_names) <- c('vektmszdbczorgproduct', 'vektmszdbczorgproduct_naam')
dbc_names <- format_data(dbc_names, rin_num = F)
dbc_names[, vektmszdbczorgproduct := gsub('"', '', vektmszdbczorgproduct)]
top_20_codes_operatie_1000 <- merge(top_20_codes_operatie_1000, dbc_names, 
                                    all.x = T, by = c('vektmszdbczorgproduct'))
top_20_codes_operatie_30 <- merge(top_20_codes_operatie_30, dbc_names, 
                                  all.x = T, by = c('vektmszdbczorgproduct'))
setorder(top_20_codes_operatie_1000, cohort, died, period, -n_totaal_gebruikers)
setorder(top_20_codes_operatie_30, cohort, died, period, -n_totaal_gebruikers)

openxlsx::addWorksheet(wb, 'top_20_codes_operatie_1000')
openxlsx::writeData(wb, 'top_20_codes_operatie_1000', top_20_codes_operatie_1000)
openxlsx::addWorksheet(wb, 'top_20_codes_operatie_30')
openxlsx::writeData(wb, 'top_20_codes_operatie_30', top_20_codes_operatie_30)
rm(top_20_codes_operatie_1000, top_20_codes_operatie_30)

top_20_codes_activit <- './output/iteration_2/top_20_codes_activit.xlsx'
top_20_codes_activit_1000 <- readxl::read_excel(top_20_codes_activit, 
                                                sheet = 'top_20_codes_1000_dagen')
top_20_codes_activit_30 <- readxl::read_excel(top_20_codes_activit, 
                                              sheet = 'top_20_codes_30_dagen')

# Add names
dbc_names <- rio::import(
  'K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods')
dbc_names <- format_data(dbc_names, rin_num = F)
dbc_names <- dbc_names[, .(vektmszzorgactiviteit = mszzorgactiviteit, 
                           mszzorgactiviteitomschrijving)]

top_20_codes_activit_1000 <- merge(top_20_codes_activit_1000, dbc_names, 
                                   all.x = T, by = c('vektmszzorgactiviteit'))
top_20_codes_activit_30 <- merge(top_20_codes_activit_30, dbc_names, 
                                 all.x = T, by = c('vektmszzorgactiviteit'))
setorder(top_20_codes_activit_1000, beeldvorming_hoofdcategorie, cohort, died, period, -n_totaal_gebruikers)
setorder(top_20_codes_activit_30, beeldvorming_hoofdcategorie, cohort, died, period, -n_totaal_gebruikers)

openxlsx::addWorksheet(wb, 'top_20_codes_activit_1000')
openxlsx::writeData(wb, 'top_20_codes_activit_1000', top_20_codes_activit_1000)
openxlsx::addWorksheet(wb, 'top_20_codes_activit_30')
openxlsx::writeData(wb, 'top_20_codes_activit_30', top_20_codes_activit_30)
rm(top_20_codes_activit_1000, top_20_codes_activit_30, dbc_names)


#### Second, get the combo of Wlz and MSZ ####
wlz_msz <- './output/iteration_2/wlz_msz.xlsx'
wlz_msz_heup <- as.data.table(readxl::read_excel(wlz_msz, sheet = 'heeft_heup_totaal'))
wlz_msz_heup <- wlz_msz_heup[!(name %in% c('heeft_heup_totaal', 'heeft_aaa_totaal'))]

# Do not export this one because of too small cells
#wlz_msz_aaa <- readxl::read_excel(wlz_msz, sheet = 'heeft_aaa_totaal')

openxlsx::addWorksheet(wb, 'wlz_msz_heup')
openxlsx::writeData(wb, 'wlz_msz_heup', wlz_msz_heup)
rm(wlz_msz_heup)


#### Third, get the costs and usage data ####
# Wlz
wlz <- as.data.table(rio::import('./output/iteration_2/agg_wlz.csv'))

# Keep only 2023 cohort
wlz <- wlz[cohort == 2023]

# Keep aggregation in total and by Wlz usage
wlz <- wlz[doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
             inkomen_klasse == 'all' & seswoa_cat == 'all' & 
             migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
             stedgem == 'all']

openxlsx::addWorksheet(wb, 'wlz')
openxlsx::writeData(wb, 'wlz', wlz)
rm(wlz)


# Wlz corrected
wlz_corrected <- as.data.table(rio::import('./output/iteration_2/agg_wlz_corrected.csv'))

# Keep aggregation only in total
wlz_corrected <- wlz_corrected[
  doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
                                 inkomen_klasse == 'all' & seswoa_cat == 'all' & 
                                 migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
                                 stedgem == 'all' & wlz_start_period == 'all']

openxlsx::addWorksheet(wb, 'wlz_corrected')
openxlsx::writeData(wb, 'wlz_corrected', wlz_corrected)
rm(wlz_corrected)


# ZVW
zvw <- as.data.table(rio::import('./output/iteration_2/agg_zvw.csv'))
zvw <- zvw[bin_size == 1000]

# Keep only 2023 cohort 
zvw <- zvw[cohort == 2023]

# Keep splits by all characterisitcs, except for the cause of death
zvw <- zvw[doodsoorzaak == 'all']

openxlsx::addWorksheet(wb, 'zvw')
openxlsx::writeData(wb, 'zvw', zvw)
rm(zvw)

# ZVW corrected
zvw_corrected <- as.data.table(rio::import('./output/iteration_2/agg_zvw_corrected.csv'))
zvw_corrected <- zvw_corrected[bin_size == 1000]

# Keep aggregation only in total
zvw_corrected <- zvw_corrected[
  doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
                                 inkomen_klasse == 'all' & seswoa_cat == 'all' & 
                                 migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
                                 stedgem == 'all' & wlz_start_period == 'all']

openxlsx::addWorksheet(wb, 'zvw_corrected')
openxlsx::writeData(wb, 'zvw_corrected', zvw_corrected)
rm(zvw_corrected)


# MSZ
msz_prestaties <- as.data.table(rio::import('./output/iteration_2/agg_msz_prestaties.csv'))

# Keep only 2023 cohort
msz_prestaties <- msz_prestaties[cohort == 2023] # TODO: remove filter

# Keep splits by all characterisitcs, except for the cause of death
msz_prestaties <- msz_prestaties[doodsoorzaak == 'all']
openxlsx::addWorksheet(wb, 'msz_prestaties')
openxlsx::writeData(wb, 'msz_prestaties', msz_prestaties)
rm(msz_prestaties)

# MSZ corrected
msz_prestaties_corrected <- as.data.table(rio::import(
  './output/iteration_2/agg_msz_prestaties_corrected.csv'))

# Keep aggregation only in total
msz_prestaties_corrected <- msz_prestaties_corrected[
  doodsoorzaak == 'all' & age_cat == 'all' & geslacht == 'all' & 
    inkomen_klasse == 'all' & seswoa_cat == 'all' & 
    migratie_achtergrond == 'all' & huishoudsamenstelling == 'all' &
    stedgem == 'all' & wlz_start_period == 'all']
openxlsx::addWorksheet(wb, 'msz_prestaties_corrected')
openxlsx::writeData(wb, 'msz_prestaties_corrected', msz_prestaties_corrected)
rm(msz_prestaties_corrected)

# MSZ prestatie diagnostics
msz_prestaties_diag <- as.data.table(rio::import(
  './output/iteration_2/agg_msz_prestatie_diagnostiek.csv'))

# Keep pnly total of 1000 days and the last month 
msz_prestaties_diag <- msz_prestaties_diag[t == -1]

# Drop several outcomes
msz_prestaties_diag <- msz_prestaties_diag[!grepl(
  "^(kosten_heup_0303|n_heup_0303|kosten_heup_0305|n_heup_0305)", name, ignore.case = T)]

openxlsx::addWorksheet(wb, 'msz_prestaties_diag')
openxlsx::writeData(wb, 'msz_prestaties_diag', msz_prestaties_diag)
rm(msz_prestaties_diag)


# MSZ activit diagnostics
msz_activit_diag <- as.data.table(rio::import(
  './output/iteration_2/agg_msz_activit_diagnostiek.csv'))

# Keep pnly total of 1000 days and the last month 
msz_activit_diag <- msz_activit_diag[t == -1]

openxlsx::addWorksheet(wb, 'msz_activit_diag')
openxlsx::writeData(wb, 'msz_activit_diag', msz_activit_diag)
rm(msz_activit_diag)


# MSZ add-on oncology total who died from cancer
msz_addon_oncology_total_cancer <- as.data.table(rio::import(
  './output/iteration_2/agg_msz_addon_oncology_total_cancer.csv'))

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

openxlsx::addWorksheet(wb, 'msz_addon_oncology_total_cancer')
openxlsx::writeData(wb, 'msz_addon_oncology_total_cancer', msz_addon_oncology_total_cancer)
rm(msz_addon_oncology_total_cancer)


# MSZ add-on oncology separately among those who died from cancer
msz_addon_oncology_cancer <- as.data.table(rio::import('./output/iteration_2/agg_msz_addon_oncology_cancer.csv'))

# Keep only deceased, no control group
msz_addon_oncology_cancer <- msz_addon_oncology_cancer[died == 'Overleden']

# Change doodsoorzaak from 'all' to 'Palliatief kanker' 
msz_addon_oncology_cancer[doodsoorzaak == 'all', doodsoorzaak := 'Palliatief kanker']

openxlsx::addWorksheet(wb, 'msz_addon_oncology_cancer')
openxlsx::writeData(wb, 'msz_addon_oncology_cancer', msz_addon_oncology_cancer)
rm(msz_addon_oncology_cancer)

# MSZ non-oncology
msz_addon <- as.data.table(rio::import('./output/iteration_2/agg_msz_addon.csv'))

# Drop oncology outcomes
#msz_addon <- msz_addon[!grepl('13', substr(name, pmax(2, nchar(name) - 4), nchar(name)))]

# Keep pnly total of 1000 days and the last month 
msz_addon <- msz_addon[t == -1]

openxlsx::addWorksheet(wb, 'msz_addon')
openxlsx::writeData(wb, 'msz_addon', msz_addon)
rm(msz_addon)

openxlsx::saveWorkbook(wb, glue::glue('{path}output.xlsx'), overwrite = T)
