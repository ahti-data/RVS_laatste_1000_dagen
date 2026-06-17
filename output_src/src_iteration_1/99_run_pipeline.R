# Project: Laatste 1000 dagen
# Author: Marco Griep
# Goal: Run all R files
# Output: output in Excel
# Last edited: 12 March 2026

rm(list = ls())
gc()
library(tictoc)
library(beepr)

withCallingHandlers(
  {
    # tic("01_clean_demograpic_characteristics.R")
    # source("src/01_clean_demographic_characteristics.R")
    # rm(list = ls())
    # gc()
    # toc()

    # tic("02_find_random_matches.R")
    # source("src/02_find_random_matches.R")
    # rm(list = ls())
    # gc()
    # toc()

    tic("03a_clean_annual_healthcosts.R")
    source("src/03a_clean_annual_healthcosts.R")
    rm(list = ls())
    gc()
    toc()

    tic("03b_clean_continuous_healthcosts.R")
    source("src/03b_clean_continuous_healthcosts.R")
    rm(list = ls())
    gc()
    toc()
    
    tic("03c_clean_dbc_codes.R")
    source("src/03c_clean_dbc_codes.R")
    rm(list = ls())
    gc()
    toc()
    
    # tic("03d_calculate_atc4_codes.R")
    # source("src/03d_calculate_atc4_codes.R")
    # rm(list = ls())
    # gc()
    # toc()
    
    tic("04_make_aggregations.R")
    source("src/04_make_aggregations.R")
    rm(list = ls())
    gc()
    toc()
    
    
    # tic("04_analyse_plots.R")
    # source("src/04_analyse_plots.R")
    # rm(list = ls())
    # gc()
    # toc()
    
    # tic("05_analyze_demographics.R")
    # source("05_analyze_demographics.R")
    # rm(list = ls())
    # gc()
    # toc()
    # 

    # source("src/05_merge_costs.R")
    # rm(list = ls())
    # gc()
  },
  error = function(e) {
    beep(9)
  }
)
