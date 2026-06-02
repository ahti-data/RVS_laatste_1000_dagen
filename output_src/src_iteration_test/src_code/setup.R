#### libraries ####
library(data.table)
library(glue)
library(dplyr)
library(logger)

source("H:/utils/backup/20260405/m_functions.R")
source("H:/utils/backup/20260405/demog_functions.R")
source("H:/utils/backup/20260405/comp_functions.R")
source("H:/utils/backup/20260405/anon_output_functions.R")


years <- 2018:2023
years_demog <- 2017:2023
rel_cols_lbz <- c(
  "rinpersoon", "lbzidopname", "lbzopnamedatum",
  "lbzicd10hoofddiagnose", "lbzzorgtype"
)
rel_cols_demog <- c(
  "rinpersoon", "gem", "wc", "bc", "geslacht", "leeftijd"
)

demog_splits <- c("leeftijd_cat", "geslacht", "profile_LG", "all")

unique_zorgtypes <- c("K", "D", "O", "all")