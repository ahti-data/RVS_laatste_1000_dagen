# RVS_laatste_1000_dagen

## Overview
Project analyzing the last 1000 days that people live. The goal of the project is to gain more insights into the last 1000 days, and where potential improvement-of-life policies could help overtreatment of individuals

## Repository Owner
ahti-data

## Linked CBS PDFs
- huisartsdeclaraties
- MSZPrestatiesVeKT
- personenmetgebruikvanzvwwijkverpleging
- zvw-eerstelijnsverblijf

## Upload Layout
- Source upload folder: output_src/src_iteration_2
- Output upload folder: outputs/output_disabled

## Source Files
- src-laatstse_1000_dagen/00_inputs.R

  ```text
  # Project: Laatste 1000 dagen
  # Author: Stanislav Avdeev & Marco Griep
  # Goal: Set inputs used in other files
  # Output: None
  # Last edited: 23 February 2026
  
  #### initialize ####
  library(data.table)
  library(glue)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(collapse)
  
  source("H:/_Personal_folders/Marco/loading function/loading_function.R")
  source("H:/utils/backup/20260109/m_functions.R")
  source("H:/utils/backup/20260109/demog_functions.R")
  
  #### parameters ####
  years <- 2016:2023
  cols_to_select_stapeling_31_12 <- c("burgstaat", "stedgem") # 'geslacht', 'leeftijd'
  ```
- src-laatstse_1000_dagen/00_review_comments.R

  ```text
  # REVIEW
  
  #### marco ####
  ## 00_inputs.R
  # 221: calculate_costs_by_bin_size()
  # Consider only keeping the columns you supply as cost_date_col (with other used columns)
  # and dropping columns you do not want to keep
  
  # 239: costs_dt_clean <- copy(costs_dt)
  # Consider dropping that line if the original file is not used
  
  # 240: overlijden_dt_clean <- copy(overlijden_dt)
  # Consider dropping that line if the original file is not used
  
  # 03b_clean_continuous_healthcosts
  # 76: by = .(rinpersoon, gbadatumoverlijden, cohort, died, t, bin_start, bin_end),
  # If you do not use bin_start an
  ```
- src-laatstse_1000_dagen/01_clean_demographic_characteristics.R

  ```text
  # Project: Laatste 1000 dagen
  # Author: Stanislav Avdeev & Marco Griep
  # Goal: Find demographics characteristics, date and cause of death
  # Output: Individual level processed data
  # Last edited: 20 April 2026
  
  rm(list = ls())
  gc()
  source("./src/00_inputs.R")
  
  #### Find sex and birth year ####
  file_path <- get_path_newest(
    path = "G:/Bevolking/GBAPERSOONTAB/2024/",
    string_pattern = "GBAPERSOONTAB",
    extension = ".csv",
    method = "max_version"
  )
  df_gbapersoon <- fread(file_path, select = cols_to_select_gbapersoon)
  df_gbapersoon <- format_data(df_gbapersoon)
  df_gbapersoon
  ```
- src-laatstse_1000_dagen/02_find_random_matches.R

  ```text
  # Project: Laatste 1000 dagen
  # Author: Stanislav Avdeev & Marco Griep
  # Goal: Find 10 random people based on sex and birth year
  # Output: A sample of rinpersoon
  # Last edited: 3 March 2025
  
  #### initialize ####
  rm(list = ls())
  gc()
  source('./src/00_inputs.R')
  
  # Load death registry
  overlijden <- r_parquet_get_dt('./data/raw/overlijden.parquet')
  
  # Load sex and year of birth 
  df_gbapersoon <- r_parquet_get_dt('./data/raw/gbapersoon.parquet')
  df_overlijden <- merge(overlijden, df_gbapersoon, by = 'rinpersoon', all.x = T)
  gc()
  
  # Load cause of death
  df_doodoorz <- r_parquet_g
  ```

## Output Files
- None

## Notes
- This README was generated in the browser without a backend service.

## AI-Generated Summary

Project summary generated in the browser.