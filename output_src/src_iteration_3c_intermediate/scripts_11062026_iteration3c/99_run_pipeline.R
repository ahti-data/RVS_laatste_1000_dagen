# Project: Laatste 1000 dagen
# Author: Marco Griep
# Goal: Run all R files
# Output: output in Excel
# Last edited: 12 March 2026

rm(list = ls())
gc()
library(tictoc)
library(beepr)

files_to_run_in_order <- c(
  list.files("src/01_cleaning"),
  list.files("src/02_processing"),
  list.files("src/03_output_preparation"),
  list.files("src/04_quality_checks")
)

log_file <- "data/logs/pipeline_runtime.log"


withCallingHandlers(
  {
    for (filepath in files_to_run_in_order) {
      tic(filepath)
      source(filepath, local = new.env())
      toc(log = T)
    }
  },
  error = function(e) {
    beep(9)
  }
)

all_log <- tic.log(format=T)

writelines(all_log, con=log_file, sep = "\n")
