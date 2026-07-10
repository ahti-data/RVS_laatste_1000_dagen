#' @name Master_functions_overview
#' @title Content
#' @md
#' @description
#' Described functions in this document:
#' * [get_path_newest()]
#' * [get_wk_naam()]
#' * [format_data()]
#' * [generate_spss_translate_script()]
#' * [generate_batch_spss_translate_script()]
#' * [to_ymd_to_date()]
#' * [compress_delete_csvs_in_folder()]
#' * [to_num()]
#' * [search_in_files()]
#' * [source_and_backup()]
#' * [list_folders_with_sizes()]
#' * [r_parquet_get_dt()]
#' * [describe_dataset()]
#' * [get_newest_parquet()]
#' * [comp_newest_sav_parquet()]
#' * [get_newest_parquet_check()]
NULL


#' Get newest matching file path
#'
#' Returns the most recent path of a file with a user-defined extension and
#' matching string. In case of multiple matches, the newest file is selected
#' based on the chosen method.
#'
#' @param path Folder in which the function should look for matching files.
#' @param string_pattern Character string to match in filenames.
#' @param extension File extension such as ".dta" or ".sav"; case-insensitive.
#' @param method In case of multiple matches, determines how to select the
#'   newest file. Options: "newest" (based on modification time) or
#'   "max_version" (based on highest version number, assumes pattern 'V%d' in name).
#' @param recursive Boolean indicating whether the file listing function should search recursively.
#'
#' @return Character: path of the newest matching file, or message if none found.
#' @examples
#' \dontrun{
#' get_path_newest("G:/GezondheidWelzijn/GEBWLZTAB", "glue("gebwlz{yr}")",
#' ".sav", "newest")
#' }
#' \dontrun{
#' get_path_newest("file.path(G:/GezondheidWelzijn/MEDICIJNTAB, yr)", "MEDICIJN",
#' ".csv")
#' }
#' @export

get_path_newest <- function(
    path,
    string_pattern,
    extension=".dta",
    method="max_version",
    recursive = FALSE
){
  files_target_extension = list.files(path, pattern = paste0("\\",extension,"$"), full.names = T, ignore.case = T, recursive = recursive)
  
  matching_files = files_target_extension[grepl(string_pattern,files_target_extension)]
  
  if(length(matching_files)==0){
    return(paste0("No matching ",extension," files found."))
  }
  
  if(method=="newest"){
    file_info = file.info(matching_files)
    out_file_path = matching_files[which.max(file_info$mtime)]
  }else if(method=="max_version"){
    matching_files = matching_files[grepl("V[0-9]+",matching_files)]
    if(length(matching_files)==0){
      return("No files matching pattern 'V%digit' found.")
    }
    file_numbers = as.numeric(gsub(".*V([0-9]+).*","\\1",basename(matching_files)))
    out_file_path = matching_files[which.max(file_numbers)]
  }
  
  return(out_file_path)
}



#' Get wijk name per year
#'
#' Reads CBS wijk shapefile for a given year and returns a data.table with
#' wijk code, wijk name, and optionally geometry.
#'
#' @param year Desired year.
#' @param add_geometry_col Logical; whether to include geometry column. Default = False
#'
#' @return data.table with WK_CODE, WK_NAAM, and optionally geometry.
#' @section Notes:
#' years 2014, 2015, 2021 have no wijk naam
#'
#' @examples
#' \dontrun{wk <- get_wk_naam(2020)}
#'
#' @importFrom sf st_read
#' @importFrom data.table as.data.table setnames
#' @export
get_wk_naam <- function(year, add_geometry_col = F){
  shp_file_path = get_path_newest(path = paste0("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/",
                                                year,"/"),
                                  string_pattern = "wijk|wk",
                                  extension = ".shp",
                                  method = "newest")
  
  dt_shp <- as.data.table(sf::st_read(shp_file_path))
  
  matching_cols = grep("wk|wijk", colnames(dt_shp), value = T, ignore.case = T)
  
  dt_shp = dt_shp[,.SD, .SDcols = matching_cols]
  
  setnames(dt_shp, paste0("WK", year),"WK_CODE",skip_absent = T)
  
  return(dt_shp)
}


#' Format data: names, year suffixes, and RIN as numeric
#'
#' Transforms column names to lower case, removes year suffixes, and converts
#' RINPERSOON to numeric.
#'
#' @param dt Input data (data.frame or data.table).
#' @param lower Logical; whether to make column names lowercase. Default is TRUE
#' @param year Logical; whether to remove year suffixes. Default is TRUE
#' @param rin_num Logical; whether to convert rinpersoon to numeric. Default is TRUE
#'
#' @return data.table with formatted columns.
#' @importFrom data.table setDT setnames
#' @export
format_data <- function(dt, lower = TRUE, year = TRUE, rin_num = TRUE) {
  ## Add check for datatype (haven does not always load as.data.table)
  
  data.table::setDT(dt)
  #print('runt dit?')
  if (lower) {
    cat("Changing names to lower\n")
    #names(dt) <- tolower(names(dt))
    setnames(dt, tolower(names(dt)))
  }
  #print('en runt dit?')
  if (year) {
    cat("Removing year extensions from columns\n")
    names(dt) <- gsub("_\\d{4}", "", names(dt))
  }
  if (rin_num) {
    cat("Setting rinpersoon to numeric\n")
    dt[, rinpersoon := as.numeric(rinpersoon)]
  }
  return(dt)
}



#' Build SPSS syntax to translate SAV to CSV
#'
#' Generate SPSS commands that read a `.sav` file and write a `.csv`, using only
#' the columns specified by the user (case-insensitive, partial match).
#'
#' @param file_path Full path of the `.sav` file.
#' @param cols_to_select Character vector of column names to be selected from `.sav` file.
#' @param output_path Full path to the target `.csv`.
#' @param test Logical. If `TRUE`, add `N OF CASES 10.` for a 10-row test export. Default is False
#'
#' @return Character vector of SPSS syntax lines.
#' @examples
#' \dontrun{
#' lines <- generate_spss_translate_script(
#'   file_path = "x.sav",
#'   cols_to_select = c("rinpersoon", "^age$"),
#'   output_path = "x.csv"
#' )
#' writeLines(lines, "translate.sps")
#' }
#' @export
generate_spss_translate_script <- function(file_path, cols_to_select,
                                           output_path, test = F){
  dt = haven::read_sav(file_path, n_max =1)
  colnames_dt = colnames(dt)
  cols_to_select_match = colnames_dt[
    sapply(colnames_dt, function(cn) any(grepl(
      paste(cols_to_select, collapse="|"), cn, ignore.case=T)))
  ]
  drop_cols = setdiff(colnames(dt),cols_to_select_match)
  chunks = split(drop_cols, ceiling(seq_along(drop_cols)/6))
  lines = vapply(chunks, paste,collapse=" ", FUN.VALUE = "")
  drop_cols_line = paste(drop_cols,collapse = "\n ")
  
  if(test==T){
    test_line = "N OF CASES 10."
  }else{
    test_line = ""
  }
  
  spss_lines = c(
    "OUTPUT CLOSE ALL.",
    glue("GET FILE='{file_path}'."),
    "DATASET NAME DataSet1 WINDOW=FRONT.",
    "",
    test_line,
    glue("SAVE TRANSLATE OUTFILE='{output_path}'"),
    "/TYPE=CSV",
    "/ENCODING='Locale'",
    "/MAP",
    "/REPLACE",
    "/FIELDNAMES",
    "/CELLS=VALUES",
    glue("/DROP={drop_cols_line}"),
    ".",
    ""
  )
  
  return(spss_lines)
}


#' Build a batch SPSS translate script for many SAV files
#'
#' Create a single `.sps` script that converts multiple `.sav` files to `.csv`
#' in a given output folder, keeping only selected columns.
#'
#' @param sav_paths Character vector. Full paths to `.sav` files.
#' @param file_output_folder_full_path Output folder for the CSVs.
#' @param cols_to_select Character vector of column names to keep.
#' @param script_name Base name for the `.sps` file (without extension).
#' @param script_path Folder to write the `.sps` file to. Default  is `"./spss_scripts"`.
#' @param test Logical. If `TRUE`, include the 10-row test line for each file. Default is False
#'
#' @return `NULL`. Writes the `.sps` file to disk as a side effect.
#' @examples
#' \dontrun{
#' generate_batch_spss_translate_script(
#'   sav_paths = c("a.sav","b.sav"),
#'   file_output_folder_full_path = "out",
#'   cols_to_select = c("rinpersoon","^age$"),
#'   script_name = "batch_translate"
#' )
#' }
#' @export
generate_batch_spss_translate_script <- function(sav_paths,
                                                 file_output_folder_full_path,
                                                 cols_to_select,
                                                 script_name,
                                                 script_path = "./spss_scripts",
                                                 test = F){
  script = c()
  for(sav_path in sav_paths){
    cat(sav_path,"\n")
    csv_filename = glue("{tools::file_path_sans_ext(basename(sav_path))}.csv")
    csv_file_path = glue("{file_output_folder_full_path}/{csv_filename}")
    script_temp = generate_spss_translate_script(file_path = sav_path,
                                                 cols_to_select = cols_to_select,
                                                 output_path = csv_file_path,
                                                 test = test
    )
    script = c(script, script_temp)
  }
  
  writeLines(script, glue("{script_path}/{script_name}.sps"))
}


#' Convert yyyymmdd integers to Date columns (in-place)
#'
#' Convert one or more columns containing `yyyymmdd` values to `Date` in-place.
#'
#' @param dt data.table
#' @param cols Character vector. Column names to convert.
#'
#' @return `NULL`. Modifies `dt` by reference.
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(d = 20230131L)
#' to_ymd_to_date(dt, "d")
#' str(dt$d)
#' }
#' @export
to_ymd_to_date <-function(dt, cols){
  dt[,(cols):=lapply(.SD, lubridate::ymd), .SDcols = cols]
  dt[,(cols):=lapply(.SD, as.Date), .SDcols = cols]
}


#' Gzip all CSVs in a folder (optional cleanup and date conversion)
#'
#' Read each `.csv` in a folder, optionally convert date columns from `yyyymmdd`
#' to `Date`, write as `csv.gz`, and optionally delete the original CSV.
#'
#' @param folder_path Character. Folder containing CSV files.
#' @param delete_bool Logical. If `TRUE`, delete the original CSV after successful gzip.Default is FALSE
#' @param convert_cols_to_date_bool Logical. If `TRUE`, convert `date_cols` using `to_ymd_to_date()`. Default is FALSE
#' @param date_cols Character vector. Columns to convert when `convert_cols_to_date_bool = TRUE`. Default is NULL
#'
#' @return `NULL`. Writes compressed files; prints progress messages.
#' @examples
#' \dontrun{
#' compress_delete_csvs_in_folder("out/csv", delete_bool = TRUE,
#'                               convert_cols_to_date_bool = TRUE,
#'                               date_cols = c("start_datum","eind_datum"))
#' }
#' @export
compress_delete_csvs_in_folder <- function(folder_path, delete_bool = F,
                                           convert_cols_to_date_bool = F,
                                           date_cols = NULL){
  csv_files = list.files(folder_path, pattern = "\\.csv$", full.names = T)
  for(csv_file in csv_files){
    
    data.table::setDTthreads(0)
    dt = data.table::fread(csv_file)
    
    gz_file = glue::glue("{csv_file}.gz")
    
    if(convert_cols_to_date_bool==T){
      to_ymd_to_date(dt = dt, cols = date_cols)
    }
    
    data.table::fwrite(dt,gz_file, compress = "gzip")
    
    if(file.exists(gz_file)){
      message("Compressed file ", basename(csv_file))
      if(delete_bool==T){
        file.remove(csv_file)
        message("Deleted ",basename(csv_file))
      }
    }else{
      warning("Compression failed for ", basename(csv_file))
    }
  }
}


#' Convert columns to numeric (in-place)
#'
#' Coerce specified columns in a data.table to numeric, modifying in-place.
#'
#' @param dt data.table. Modified by reference.
#' @param cols Character vector. Column names to convert.
#'
#' @return `NULL`. Modifies `dt` by reference.
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(a = "1", b = "2.5")
#' to_num(dt, c("a","b"))
#' }
#' @export
to_num <-function(dt, cols){
  dt[,(cols):=lapply(.SD, as.numeric), .SDcols = cols]
}


#' Search R scripts for a pattern
#'
#' Recursively scan `.R` files under a path and print lines that match a pattern.
#'
#' @param path Character. path to folder with scripts to search. Default is `"H:/ibo_ggz"`
#' @param pattern Character. Regex pattern to search for. Default is `"_6y.csv.gz"`
#'
#' @return `NULL`. Prints file paths and matching lines.
#' @examples
#' \dontrun{
#' search_in_files(path = "R", pattern = "data\\.csv")
#' }
#' @export
search_in_files <- function(path = "H:/ibo_ggz", pattern = "_6y.csv.gz") {
  files <- list.files(path, pattern = "\\.R$", recursive = T, full.names=T)
  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    matches <- grep(pattern, lines, value=TRUE)
    if (length(matches)) {
      cat(sprintf("\nFile: %s\n", file))
      print(matches)
    }
  }
}


#' Source and back up R scripts
#'
#' Sources one or more R script files and creates a dated backup copy of each
#' in a specified backup directory. If the backup directory does not exist, it
#' will be created automatically.
#'
#' @param files Character vector of full file paths to R scripts to source.
#' @param backup_dir Character string indicating the directory where backups
#'   should be stored. A dated subfolder (e.g., `backup_20251107`) is created
#'   inside this directory.
#'
#' @return NULL. The function sources each file, copies it to the backup folder,
#'   and prints progress messages.
#'
#' @details
#' For each file:
#' \itemize{
#'   \item Checks that the file exists.
#'   \item Sources the file with `base::source()`.
#'   \item Copies the file into a new dated subdirectory within `backup_dir`.
#' }
#' Any errors during sourcing are caught and reported as warnings, allowing
#' the process to continue for the remaining files.
#'
#' @examples
#' \dontrun{
#' source_and_backup(
#'   files = c("R/utils.R", "R/analysis.R"),
#'   backup_dir = "backups"
#' )
#' }
#'
#' @export
source_and_backup <- function(files, backup_dir){
  if(!dir.exists(backup_dir)){
    dir.create(backup_dir,recursive=T)
    message("Created backup directory: ", backup_dir)
  }
  
  ts = format(Sys.time(), "%d%m%Y")
  backup_subdir <-file.path(backup_dir,paste0("backup_",ts))
  dir.create(backup_subdir)
  
  for(f in files){
    if(!file.exists(f)){
      warning("File not found: ", f)
      next
    }
    
    message("Sourcing: ", f)
    tryCatch(
      source(f),
      error = function(e) warning("Error sourcing ", f,": ",e$message)
    )
    
    file.copy(f,backup_subdir, overwrite = T)
    
    message("All files sourced and backed up in: ", backup_subdir)
    
  }
}


#' List folder sizes
#'
#' lists the memory of all folders within the given directory path
#' 
#' @param dir Directory path to use
#' 
#' @return Dataframe with memmory of all folders in directory
#' 
#' 
#' @export
list_folders_with_sizes <- function(dir = "H:/") {
  
  cmd <- sprintf("du -sk %s/*/", shQuote(dir))
  output <- system(cmd, intern = TRUE)
  
  df <- read.table(text = output, col.names = c("size_kb", "folder",
                                                stringsAsFactors = F))
  df$size_mb <- round(df$size_b / 1024, 2)
  df[order(-df$size_mb), ]
  rownames(df) <- NULL
  return(df)
}


#' Read parquet file and get data.table
#'
#' Returns data.table with the desired columns of a parquet file
#'
#' @param file_path Path of the parquet file to be read
#' @param columns String vector with the column names that the user wants to read in
#' @return data.table with the desired columns of the table stored in file file_path
#' @examples
#' \dontrun{
#' dt = r_parquet_get_dt("./data.parquet")
#' dt_cols_1_2 = r_parquet_get_dt("./data.parquet", columns = c("col1","col2"))
#' }
#' @export
r_parquet_get_dt <- function(file_path, columns = NULL){
  
  if(is.null(columns)){
    dt = arrow::read_parquet(file_path)
  }else{
    dt = arrow::read_parquet(file_path, col_select = all_of(columns))
  }
  
  dt = haven::zap_labels(dt)
  data.table::setDT(dt)
  
  return(dt)
}


#' Get dataset descriptives
#'
#' Returns list with descriptives on id, factor variables and numeric variables
#'
#' @param dt Tabular object to be described
#' @param year year of the dataset? REVIEW LEONARDO: UNCLEAR (same year for all rows? what about dataset with multiple years? or with a year column?)
#' @param id_cols String vector with the column names that define a unique row, for example c("profile_complexity","age", "sex","year")
#' @param num_cols String vector with the column names that define a unique row, for example c("profile_complexity","age", "sex","year")
#' @param level_cols String vector with the column names that define a unique row, for example c("profile_complexity","age", "sex","year")
#' @return List with elements "id", "levels", "nums" that describe id, factor variables and numeric variables
#' @examples
#' \dontrun{
#' add examples :)
#' }
#' @export
describe_dataset <- function(
    dt,
    year=NULL,
    id_cols=NULL,
    num_cols=NULL,
    level_cols=NULL) {
  dt <- data.table::as.data.table(dt)
  
  id_desc <- data.table::rbindlist(lapply(id_cols, function(col) {
    data.table::data.table(variable = col,
                           unique_n = data.table::uniqueN(dt[[col]], na.rm=T),
                           year = year)
  }))
  
  levels_desc <- data.table::rbindlist(lapply(level_cols, function(col) {
    l <- as.character(dt[[col]])
    l[is.na(l)] <- "NA"
    tmp <- data.table(variable=col, level=l, year=year)[
      , .(n=.N), by = .(variable, level, year)]
    return(tmp)
  }),
  use.names=T, fill=T)
  
  num_desc <- data.table::rbindlist(lapply(num_cols, function(col) {
    x <- as.numeric(dt[[col]])
    nn <- DescTools::RoundTo(sum(!is.na(x)), 5)
    na <- DescTools::RoundTo(sum(is.na(x)), 5)
    n_pos <- DescTools::RoundTo(sum(x[!is.na(x)] > 0), 5)
    
    na_lt_10 <- function(x) return(ifelse(x < 10, NA, x))
    
    mean <- mean(x, na.rm=T)
    tmp <- data.table(variable=col, nn = na_lt_10(nn), na = na_lt_10(na),
                      n_pos = na_lt_10(n_pos), mean=mean, year=year,
                      q_25 = as.numeric(quantile(x, 0.25, na.rm=T)),
                      q_75 = as.numeric(quantile(x, 0.75, na.rm=T)))
    
  }),
  use.names=T, fill=T
  )
  
  id_desc[id_desc == 0] <- NA
  levels_desc[levels_desc == 0] <- NA
  num_desc[num_desc == 0] <- NA
  num_desc <- num_desc[num_desc$nn > 10, ]
  
  return(
    list("id"=id_desc,
         "levels"=levels_desc,
         "nums"=num_desc)
  )
}


#' Get newest parquet file based on H:/ path and G:/ path
#'
#' Returns newest parquet path (most recent version can be both in G and H drive)
#' @param folder_g Folder path in G drive in which the function should look for matching parquet file.
#' @param folder_h Folder path in H drive in which the function should look for matching parquet file.
#' @param string_pattern Character string to match in filenames.
#' @param recursive Boolean indicating whether the file get_path_newest function should search recursively.
#' @return Character: path of the newest matching file, or message if none found.
#' @examples
#' \dontrun{
#' parquet_path = get_newest_parquet(
#' folder_g = "G:/Maatwerk/STAPELINGSMONITOR/Geconverteerde bestanden/",
#' folder_h = "H:/data/Parquet_files_G_drive/Stapeling/parquet_files/",
#' string_pattern = yr)
#' }
#' @export
get_newest_parquet <- function(
    folder_g,
    folder_h,
    string_pattern,
    recursive = FALSE
){
  
  file_g_path = get_path_newest(path = folder_g,string_pattern = string_pattern,extension = ".parquet", recursive = recursive)
  file_h_path = get_path_newest(path = folder_h,string_pattern = string_pattern,extension = ".parquet", recursive = recursive)
  
  matching_files = c(file_g_path,file_h_path)
  
  matching_files = matching_files[grepl("V[0-9]+|v[0-9]+",matching_files)]
  if(length(matching_files)==0){
    return("No files matching pattern 'V%digit' found.")
  }
  file_numbers = as.numeric(gsub(".*V([0-9]+).*","\\1",basename(matching_files)))
  out_file_path = matching_files[which.max(file_numbers)]
  
  return(out_file_path)
}

#' Check if newest parquet path and newest .sav path coincide in content (filename)
#'
#' Function to check if newest parquet path and newest .sav path coincide in content (filename),
#' there is an option to stop the script upon mismatch, a log file of mismatches is always created.
#' @param parquet_path File path of parquet file
#' @param sav_path File path of .sav file 
#' @param stop_on_mismatch Boolean indicating whether script should stop/break upon a mismatch, False by default
#' @return Log .txt file
#' @examples
#' \dontrun{
#' for(yr in 2013:2023){
#'   sav_path = 
#'     get_path_newest(
#'       path = paste0("G:/Maatwerk/STAPELINGSMONITOR/", yr,"/"),
#'       string_pattern = "Stapelings",
#'       extension = ".sav",
#'       method = "max_version")
#'   
#'   parquet_path = 
#'     get_newest_parquet(
#'       folder_g = "G:/Maatwerk/STAPELINGSMONITOR/Geconverteerde bestanden/",
#'       folder_h = "H:/data/Parquet_files_G_drive/Stapeling/parquet_files/",
#'       string_pattern = yr)
#'   
#'   comp_newest_sav_parquet(parquet_path = parquet_path,
#'                           sav_path = sav_path,
#'                           stop_on_mismatch = T)
#' }
#' }
#' @export
comp_newest_sav_parquet <- function(parquet_path, sav_path, stop_on_mismatch = F
){
  
  parquet_basename = tools::file_path_sans_ext(basename(parquet_path))
  sav_basename = tools::file_path_sans_ext(basename(sav_path))
  
  dir_save_log = glue("./comp_newest_sav_parquet_log/")
  
  if(!dir.exists(dir_save_log)) dir.create(dir_save_log)
  
  date_str = format(Sys.time(),"%Y-%m-%d")
  
  if(sink.number()==1){
    sink()
  }
  
  sink(glue("{dir_save_log}log_{date_str}.txt"),
       append = T)
  
  if(!identical(parquet_basename,sav_basename)){
    msg = glue("Found basename mismatch, parquet path:\n {parquet_basename}",
               "\n sav path:\n {sav_basename}")
    cat(msg,"\n","\n")
    sink()
    if(isTRUE(stop_on_mismatch)){
      stop(msg, call. = FALSE)
    }else{
      warning(msg,call. = FALSE
      )
      return(FALSE)
    }
    
  }else{
    sink()
  }
  TRUE
}




#' Get newest parquet file based on G:/ and H:/ paths, and check if newest parquet path and newest .sav path coincide in content (filename)
#'
#' Function to get newest parquet file and check if newest parquet path and newest .sav path coincide in content (filename),
#' function joins functionality of get_newest_parquet() and comp_newest_sav_parquet()
#' @param folder_g_parquet Folder path in G drive in which the function should look for matching parquet file, use NULL as input if only h folder is needed
#' @param folder_h_parquet Folder path in H drive in which the function should look for matching parquet file, use NULL as input if only g folder is needed
#' @param folder_g_sav Folder path in H drive in which the function should look for matching .sav files
#' @param string_pattern_parquet Character string to match in filenames in parquet folders
#' @param string_pattern_sav Character string to match in filenames in sav folder
#' @param stop_on_mismatch Boolean indicating whether script should stop/break upon a mismatch, False by default
#' @param recursive Boolean indicating whether the list.files function should search recursively.

#' @return Character: path of the newest matching file, and log file with mismatches found between parquet and sav
#' @examples
#' \dontrun{
#' for(yr in 2013:2023){
#'   parquet_path = 
#'     get_newest_parquet_check(
#'       folder_g_parquet ="G:/Maatwerk/STAPELINGSMONITOR/Geconverteerde bestanden/",
#'       folder_h_parquet = "H:/data/Parquet_files_G_drive/Stapeling/parquet_files/",
#'       folder_g_sav = paste0("G:/Maatwerk/STAPELINGSMONITOR/", yr,"/"),
#'       string_pattern_parquet = yr,
#'       string_pattern_sav = "Stapelings",
#'       stop_on_mismatch = T)
#'   cat(parquet_path,"\n")
#' }
#' }
#' @export
get_newest_parquet_check <- function(folder_g_parquet,
                                     folder_h_parquet,
                                     folder_g_sav,
                                     string_pattern_parquet,
                                     string_pattern_sav,
                                     stop_on_mismatch = F,
                                     recursive = FALSE){
  
  sav_path = 
    get_path_newest(
      path = folder_g_sav,
      string_pattern = string_pattern_sav,
      extension = ".sav",
      method = "max_version",
      recursive = recursive)
  
  
  if(is.null(folder_h_parquet) & !is.null(folder_g_parquet)){
    parquet_path =
      get_path_newest(
        path = folder_g_parquet,
        string_pattern = string_pattern_parquet,
        extension = ".parquet",
        method = "max_version",
        recursive = recursive)
  }else if(is.null(folder_g_parquet) & !is.null(folder_h_parquet)){
    parquet_path =
      get_path_newest(
        path = folder_h_parquet,
        string_pattern = string_pattern_parquet,
        extension = ".parquet",
        method = "max_version",
        recursive = recursive)
  }else{
    parquet_path = 
      get_newest_parquet(
        folder_g = folder_g_parquet,
        folder_h = folder_h_parquet,
        string_pattern = string_pattern_parquet,
        recursive = recursive)
  }
  
  
  comp_newest_sav_parquet(parquet_path = parquet_path,
                          sav_path = sav_path,
                          stop_on_mismatch = stop_on_mismatch)
  
  return(parquet_path)
}



#' 
#' Finds the directory of user specified dataset name. 
#'
#' @param dataset Exact name of the "bronbestand" (dataset) of which the directory is to be found.
#' @param drive Literal, indicating what drive to search.

#' @return Character: path of directory if found, otherwise NULL
#' @examples
#' \dontrun{
#' dir_path <- find_dataset_directory("wlzzintab", "G")
#' @export
find_dataset_directory <- function(dataset_name, drive = c("G", "H")) {
  
  drive <- match.arg(drive)
  dir_found <- FALSE
  final_dir <- NULL
  
  find_closest_folder_match <- function(dataset_name, full_paths, min_match = 0.51) {
    
    folder_names <- basename(full_paths)
    
    search_name <- tolower(dataset_name)
    dir_list_clean <- tolower(folder_names)
    
    if (search_name %in% dir_list_clean) {
      return(full_paths[which(dir_list_clean == search_name)[1]])
    }
    
    # if no exact match:
    scores <- vapply(dir_list_clean, function(d) {
      dist <- stringdist(search_name, d, method = "lcs")
      (nchar(search_name) + nchar(d) - dist) / 2
    }, numeric(1))
    
    relative_scores <- scores / pmax(nchar(search_name), nchar(dir_list_clean))
    
    match_df <- data.frame(
      full_path = full_paths,
      folder_name = folder_names,
      overlap_score = scores,
      relative_score = relative_scores,
      name_length = nchar(folder_names)
    )
    
    match_df <- match_df[order(-match_df$overlap, match_df$name_length), ]
    valid_matches <- match_df[match_df$relative_score >= min_match, ]
    
    valid_matches <- valid_matches[order(-valid_matches$relative_score, valid_matches$name_length),]
    
    
    if (nrow(valid_matches) >= 2) {
      warning("Multiple valid matches found for ", dataset_name, ", selecting ", valid_matches$full_path[1])
    }
    
    
    return(valid_matches$full_path[1])
  }
  
  # list.dirs is slow, so use list.files
  if (drive == "G") {
    first_layer_dirs <- list.files("G:/", recursive = FALSE, include.dirs = T, full.names = T)
    first_layer_dirs <- setdiff(first_layer_dirs, "G:/_TOELICHTING_INDELING")
    
    for (first_layer_dir in first_layer_dirs) {
      second_layer_dirs_full <- list.files(first_layer_dir, recursive = F, full.names = T)
      second_layer_dirs <- list.files(first_layer_dir, recursive = F, full.names = F)
      dir_found <- tolower(dataset_name) %in% tolower(second_layer_dirs)
      
      if (dir_found) {
        index <- match(tolower(dataset_name), tolower(second_layer_dirs))
        final_dir <- second_layer_dirs_full[[index]]
        break 
      }
    }
    
  } else {
    first_layer_dirs_full <-  c(
      list.files("H:/data/Parquet_files_g_drive", recursive = FALSE, include.dirs = T, full.names = T)
    )
    first_layer_dirs <-  c(
      list.files("H:/data/Parquet_files_g_drive", recursive = FALSE, include.dirs = T, full.names = F)
    )
    
    # first, try to find exact match
    dir_found <- tolower(dataset_name) %in% tolower(first_layer_dirs)
    
    if (dir_found) {
      index <- match(tolower(dataset_name), tolower(first_layer_dirs))
      final_dir <- first_layer_dirs_full[[index]]
    } else { # secondly, try to load from excel G-H crosswalk
      
      find_crosswalk_match <- function(dataset_name) {
        crosswalk_g_h <- list(
          "GBAADRESOBJECTBUS" = "GBAadresobjectbus",
          "GBAHUISHOUDENSBUS" =	"GBAhuishoudensbus",
          "GBAPERSOONTAB"	= "GBApersoontab",
          "GGZDECLVEKTIS" = "GGZDECLVEKT",
          "HUISARTSDECLTAB"	= "HUISARTSDECLTAB",
          "MEDICIJNTAB"	= "Medicijntab",
          "MSZZORGACTIVITEITENVEKTTAB" = "MSZActiviteiten",
          "MSZPRESTATIESVEKTTAB" = "MSZPrestaties",
          "STAPELINGSMONITOR" =	"Stapeling",
          "ZVWZORGKOSTENTAB" = "ZVWZorgkostentab"
        )
        
        index <- match(tolower(dataset_name), tolower(names(crosswalk_g_h)))
        
        if (is.na(index)) return(NULL)
        
        return(crosswalk_g_h[[index]])
      }
      
      matched_folder_name <- find_crosswalk_match(dataset_name)
      
      if (!is.null(matched_folder_name)) {
        final_dir <- glue("H:/data/Parquet_files_G_drive/{matched_folder_name}")
      } else final_dir <- NULL
      
    }
    
    # find partial matches in H drive
    # final_dir <- find_closest_folder_match(dataset_name, first_layer_dirs_full)
  }
  
  return(final_dir)
}


#' Loads in microdata any "bronbestand" dataset based on input folder name. 
#'
#' @param years year(s) (numeric) of the dataset that should be loaded. 
#' @param dataset The exact folder name in the G drive, of the dataset you would like to load (lowercase or uppercase). To print examples, run this function without inputs: load_dataset()
#' @param cols Character vector (ALWAYS LOWERCASE) of columns that should loaded along. If none passed; all columns will be selected.
#' @param labelled_cols Character vector (ALWAYS LOWERCASE) of columns that should loaded, and replaced by their labels. 
#' @param filetype The preferred filetype that should be loaded from. By default, the functions will try to load parquet > csv > sav > dta
#' @param n_max N rows to load (only works for non-parquet files)
#' @param format Boolean, indicating whether to run format_data() on the dataset
#' @param rinpersoon_chunk Optional chunk (numeric vector or character vector) that will be filtered in the dataset (to reduce syntax length and RAM usage)
#' @param create_year_col Boolean indicating whether to create a column "year". 

#' @return dt: loaded dataset.
#' @examples
#' \dontrun{
#' dt <- load_dataset(2020:2023, "huisartsdecltab", cols = c("rinpersoon", "hadeclhuisarts"))
#' @export

load_dataset <- function(
    years = NULL,
    dataset = NULL, 
    cols = NULL,
    labelled_cols = NULL,
    filetype = c(".parquet", ".sav", ".csv", ".dta"),
    n_max = Inf,
    format = TRUE,
    rinpersoon_chunk = NULL,
    create_year_col = FALSE,
    stop_on_mismatch=T
) {

  if (is.null(dataset)) {
    print(glue("dataset name not provided. See non-exhaustive list of datasets:
      
      DOODOORZTAB
      GBAHUISHOUDENSBUS
      GBAOVERLIJDENTAB
      HUISARTSDECLTAB
      MEDICIJNTAB
      MSZPRESTATIESVEKTTAB 
      MSZZORGACTIVITEITENVEKTTAB 
      LBZBASISTAB
      WLZZINTAB
      ZVWZORGKOSTENTAB 
      ZVWWVPTAB
      "))
    return("")
  }
  
  # initialize
  if (length(filetype) == 1) explicit_filetype = T else explicit_filetype = F
  filetype <- match.arg(filetype)
  all_cols <- union(cols, labelled_cols)
  
  # guard clauses
  assertthat::assert_that(is.numeric(years))
  assertthat::assert_that(is.character(dataset))
  assertthat::assert_that(length(dataset) == 1)
  assertthat::assert_that(is.character(cols) | is.null(cols))
  assertthat::assert_that(is.character(labelled_cols) | is.null(labelled_cols))
  assertthat::assert_that(is.character(filetype))
  assertthat::assert_that(is.numeric(n_max))
  
  if (n_max != Inf & filetype == ".parquet") {
    print("Warning: n_max does not work for parquet files. Loading csv instead:")
    filetype <- ".csv"
  }
  
  # load appropriate directories
  folder_h_parquet <- find_dataset_directory(dataset, "H")
  folder_g <- find_dataset_directory(dataset, "G")
  
  assertthat::assert_that(
    !is.null(folder_h_parquet) | !is.null(folder_g),
    msg = "Could not find the dataset folder in the H or G drive. Please ensure the dataset spelling is identical to the folder name of the dataset."
  )
  
  # always convert rinpersoon_chunk to numeric
  if (!is.null(rinpersoon_chunk)) rinpersoon_chunk <- as.numeric(rinpersoon_chunk)
  
  # browser()
  
  dt_final <- rbindlist(lapply(years, function(yr) {
    # get all files for the dataset
    all_files_h_parquet <- NULL
    if (!is.null(folder_h_parquet)) {
      # see if there are partitioned datasets
      partitioned_datasets_available <- any(grepl("partition=", list.dirs(folder_h_parquet, recursive = TRUE)))
      all_files_h_parquet <- grep(yr, list.files(folder_h_parquet, recursive = T, full.names = T), value = TRUE)
    }
    
    all_files_g <- NULL
    if (!is.null(folder_g)) all_files_g <- grep(yr, list.files(folder_g, recursive = T, full.names = T), value = TRUE)
    
    all_files <- c(all_files_h_parquet, all_files_g)
    assertthat::assert_that(!is.null(all_files), msg = glue("No files found for year {yr}"))
    
    # get the sav filepath, and get the uppercase cols
    filepath_sav <- get_path_newest(folder_g, yr, ".sav", recursive = T)
    
    all_cols_uppercase <- tryCatch({
      names(haven::read_sav(filepath_sav, n_max = 0))
    }, error = function(e) {
      filepath_csv <- get_path_newest(folder_g, yr, extension = ".csv", recursive = TRUE)
      names(fread(filepath_csv, nrows = 0))
    })
    
    if (!is.null(all_cols)) all_cols_uppercase <- all_cols_uppercase[tolower(all_cols_uppercase) %in% all_cols]
    
    if (filetype == ".parquet") {
      if (!any(grepl(".parquet", all_files))) {
        if (explicit_filetype) {
          print("Dataset does not have .parquet files. Please specify another filetype, or leave empty for automatic filetype handling.")
          return(NULL)
        }
        print("Warning: dataset does not have .parquet files. Trying to load csv instead.")
        filetype <- ".csv"
        
      } else {
        
        if (partitioned_datasets_available) {
          base_folder <- grep(yr, list.dirs(folder_h_parquet, recursive = T), value = T)[[1]]
          print(base_folder)
          ds <- arrow::open_dataset(base_folder)
          
          comp_newest_sav_parquet(
            base_folder,
            filepath_sav,
            stop_on_mismatch = stop_on_mismatch
          )
          
        } else{
          filepath <- get_newest_parquet_check(
            folder_g_parquet = folder_g,
            folder_h_parquet = folder_h_parquet,
            folder_g_sav = folder_g,
            string_pattern_parquet = yr,
            string_pattern_sav = yr,
            recursive = TRUE,
            stop_on_mismatch=stop_on_mismatch
          )
          print(filepath)
          
          ds <- arrow::open_dataset(filepath)
        }
        
        
        if (!is.null(rinpersoon_chunk)) {
          
          df <- ds |>
            mutate(RINPERSOON_num = cast(RINPERSOON, arrow::int64())) |>
            filter(RINPERSOON_num %in% rinpersoon_chunk) |>
            select(c(all_of(all_cols_uppercase))) |>
            collect()
          
        } else {
          df <- ds |>
            select(c(all_of(all_cols_uppercase))) |>
            collect()
        }
      }
    } 
    if (filetype == ".csv") {
      if (!any(grepl(".csv", all_files))) {
        print("Warning: dataset does not have .csv files. Trying to load .sav instead.")
        filetype <- ".sav"
      } else {
        filepath_csv <- get_path_newest(folder_g, yr, extension = ".csv", recursive = TRUE)
        print(filepath_csv)
        df <- fread(filepath_csv, nrows = n_max, select = all_cols_uppercase)
        
        if (!is.null(rinpersoon_chunk)) {
          if (is.character(df$RINPERSOON)) {
            df[, RINPERSOON := as.numeric(RINPERSOON)]
          }

          df <- subset(df, RINPERSOON %in% rinpersoon_chunk)
          }
        }
      }
      
    if (filetype == ".sav") {
      if (!any(grepl(".sav", all_files))) {
        print("Warning: dataset does not have .sav files. Trying to load .dta instead.")
        filetype <- ".dta"
      } else {
        
        filesize_mb <- file.info(filepath_sav)$size / 1024^2
        est_time <- round(0.005374528 * length(all_cols_uppercase) * filesize_mb)
        
        if (n_max == Inf & est_time > 60) {
          print(glue("Warning: loading a large file. Estimated time > 60 seconds"))
        }
        print(filepath_sav)
        
        df <- haven::read_sav(filepath_sav, n_max = n_max, col_select = all_of(all_cols_uppercase))
        
        if (!is.null(rinpersoon_chunk)) {
          if (is.character(df$RINPERSOON)) {
            df$RINPERSOON <- as.numeric(df$RINPERSOON)
          }
            
            df <- subset(df, RINPERSOON %in% rinpersoon_chunk)
          
        }
      }
    }
    
    if (filetype == ".dta") {
      if (!any(grepl(".dta", all_files))) {
        print("Warning: dataset does not have .sav files. Trying to load .dta instead.")
        filetype <- ".dta"
      } else {
        filepath_dta <- get_path_newest(folder_g, yr, extension = ".dta", recursive = TRUE)
        df <- haven::read_dta(filepath_dta, n_max = n_max, col_select = all_of(all_cols_uppercase))
        
        if (!is.null(rinpersoon_chunk)) {
          if (is.character(df$RINPERSOON)) {
            df$RINPERSOON <- as.numeric(df$RINPERSOON)
          }
            
            df <- subset(df, RINPERSOON %in% rinpersoon_chunk)
        }
      }
    }

    if (format) {
      if ("RINPERSOON" %in% names(df)) rin_num <- T else rin_num <- F
      dt <- format_data(df, rin_num = rin_num)
    } else dt <- as.data.table(df)
    
    
    if (!is.null(labelled_cols)) {
      replace_values_by_haven_labels(
        dt, 
        filepath_sav,
        labelled_cols,
        format = format
      )
    }
    
    if(create_year_col){
      dt[, year := yr]
    }
    
    print(glue("finished loading dataset for year {yr}"))
    return(dt)
  }), ignore.attr=T)
  return(dt_final)
}

#' Faster method of converting date columns, if large dataset. Finds the unique 
#' values in the date column, converts those, then merges them back.
#'
#' @param dt data table, containing column to be converted
#' @param col_name Name of column, to convert to date
#' @param format format to pass to as.IDate() function.
#' @return dt: data table with converted col.
#' @examples
#' \dontrun{
#' dt <- fast_to_date(dt, "vektmszbegindatum", format = "%Y%m%d")
#' @export
#' 

fast_as_date <- function(dt, col_name, format = "%Y%m%d") {
  unique_values <- dt[!is.na(get(col_name)), .(raw = unique(get(col_name)))]
  
  unique_values[, clean := as.IDate(raw, format = format)]
  
  dt[unique_values, clean_col := i.clean, on = setNames("raw", col_name)]
  
  dt[, (col_name) := NULL]
  setnames(dt, "clean_col", col_name)
  
  return(dt)
}


#' Uniform function to load in commonly used shapefiles of the Netherlands.
#'
#' @param yr Year of shapefile to be loaded
#' @param type Granularity/type of shapefile. Options:c("provincie", "gemeente", "wijk", "buurt", "pc4")
#' @param format Boolean indicating whether the shapefile should be formatted, or loaded in raw. Default True.
#' @return sf/data.frame: Loaded shapefile.
#' @examples
#' \dontrun{
#' buurt_sf_2021 <- load_shapefile(2021, "buurt")
#' @export
load_shapefile <- function(
    yr,
    type = c("provincie", "gemeente", "wijk", "buurt", "pc4", "ams_stadsdelen"),
    format = T) {
  
  if (type == "ams_stadsdelen") {
    warning("Currently, stadsdelen are loaded by default from 2017, and merged with 
            wijken shapefile from 2022.")
    
    map_stadsdelen <- format_data(rio::import("H:/data/geo/Amsterdam Gemeente & CBS indelingen 2017 .xlsx"), rin_num = FALSE, year = FALSE)
    map_stadsdelen <- map_stadsdelen[, .(wc = wk_code, stadsdelen)]
    
    map_wijken <- load_shapefile(2022, "wijk", FALSE)
    map_wijken <- format_data(map_wijken, rin_num = FALSE, year = FALSE)
    map_wijken <- map_wijken[, .(wc = statcode, gm_naam, geometry)]
    map_wijken <- map_wijken[gm_naam %in% c("Amsterdam", "Weesp")]
    
    map_stadsdelen <- merge(map_wijken, map_stadsdelen, by = "wc", all=T)
    
    map_stadsdelen <- map_stadsdelen[complete.cases(gm_naam)]
    map_stadsdelen[gm_naam == "Weesp", stadsdelen := "Weesp"]
    map_stadsdelen[wc == "WK036350", stadsdelen := "M Oost"]
    table(map_stadsdelen$stadsdelen, useNA = "ifany")
    
    wijken_to_stadsdelen <- unique(map_stadsdelen[, .(wc = substring(wc, 3), stadsdelen)])
    map_stadsdelen <- map_stadsdelen[, .(geometry = sf::st_union(geometry)), by = stadsdelen]
    map_stadsdelen <- sf::st_as_sf(map_stadsdelen)
    sf::st_crs(map_stadsdelen) <- 28992
    
    return(map_stadsdelen)
  } else if (type == "provincie") {
    path <- glue("K:/Utilities/Tools/GISHulpbestanden/Provincies/pv_{yr}.shp")
  } else if (type == "pc4") {
    dir_path <- glue("K:/Utilities/Tools/GISHulpbestanden/pc4")
    
    prefixes <- c("pc4", "s00pc4")
    
    type_regex_underscores <- paste0(prefixes, "_", yr, collapse = "|") # all the options with underscores
    type_regex_no_underscores <- paste0(prefixes, yr, collapse = "|") # all the options without underscores
    type_regex_final <- paste0(type_regex_underscores, "|", type_regex_no_underscores)
    
    path <- get_path_newest(
      dir_path,
      type_regex_final,
      extension = ".shp",
      method = "newest"
    )
  } else {
    
    filename_options_by_type <- switch(
      type,
      gemeente = c("gm", "GM", "gem", "gemeente"),
      wijk = c("wk", "WK", "wijk"),
      buurt = c("bu", "BU", "buurt")
    )
    
    dir_path <- glue("K:/Utilities/Tools/GISHulpbestanden/Gemeentewijkbuurt/{yr}")
    
    type_regex_underscores <- paste0(filename_options_by_type, "_", yr, collapse = "|") # all the options with underscores
    type_regex_no_underscores <- paste0(filename_options_by_type, yr, collapse = "|") # all the options without underscores
    type_regex_final <- paste0(type_regex_underscores, "|", type_regex_no_underscores)
    
    path <- get_path_newest(
      dir_path,
      type_regex_final,
      extension = ".shp",
      method = "newest"
    )
  }
  
  map <- sf::st_read(path)
  
  if (format) {
    # set columns to lower
    map <- map |>
      rename_with(tolower)
    
    # remove rows that contain bodies of water
    if ("water" %in% colnames(map)) {
      map <- map[map$water == "NEE",]
    }
    
    # if there are multiple columns that contain year, we want the column with suffix "_{year}", 
    # otherwise we just want to remove the year suffix from the col name
    cols <- names(map)
    colname_type_prefix <- switch(
      type,
      gemeente = c("gm"),
      wijk = c("wk"),
      buurt = c("bu"),
      provincie = c("pv"),
      pc4 = c("pc4")
    )
    col_underscore <- glue("{colname_type_prefix}_{yr}")
    col_no_underscore <- glue("{colname_type_prefix}{yr}")
    
    if (col_underscore %in% cols & col_no_underscore %in% cols) {
      map[[col_no_underscore]] <- NULL
      names(map)[names(map) == col_underscore] <- paste0(colname_type_prefix, "_code")
    } else {
      names(map)[names(map) == col_no_underscore] <- paste0(colname_type_prefix, "_code")
    }
  }
  return(map)
}

