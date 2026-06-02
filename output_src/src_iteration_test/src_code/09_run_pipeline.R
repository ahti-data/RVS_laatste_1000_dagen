rm(list=ls())
gc()

library(tictoc)

ordered_scripts_to_run <- c(
  # "src/00_data.R",
  # "src/01_merge.R",
  "src/02_analysis2.R",
  "src/03_format.R"
)

run_scripts <- function(script_paths) {
  
  if (!dir.exists("logs")) {
    dir.create("logs")
    message("Created 'logs' directory")
  }
  
  tictoc::tic.clearlog()
  
  for (script in script_paths) {
    if (file.exists(script)) {
      tic(paste("Script:", script))
      
      result <- try({
        source(script, local = new.env())
      }, silent = FALSE)
      
      toc(log = TRUE, quiet = TRUE)
      
      if (inherits(result, "try-error")) {
        message(paste("!!! Error in:", script))
      }
      
    } else {
      warning(paste("File not found:", script))
    }
  }
        
      
  
  log_output <- tic.log(format=TRUE)
  
  if (length(log_output) > 0) {
    time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_path <- file.path("logs", paste0(time_stamp, ".log"))
    
    writeLines(unlist(log_output), file_path)
  } else {
    message("No scripts were succesfully timed.")
  }
}

run_scripts(ordered_scripts_to_run)