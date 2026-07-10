library(glue)

csv_to_parquet <- function(folder) {
  files <- list.files(folder, pattern = ".csv.gz")
  
  for (f in files) {
    print(glue("Reading {f}"))
    arrow::write_parquet(
      data.table::fread(file.path(folder, f)),
      file.path(folder, gsub(".csv.gz", ".parquet", f)))
    
    print(glue("Removing: {f}"))
    file.remove(file.path(folder, f))
  }
}

csv_to_parquet("H:/ibo_ggz/data/edit")
