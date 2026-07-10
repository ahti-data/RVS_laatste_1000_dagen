library(data.table)
library(glue)
#### function to get total storage used by folder ####
dir_size <-function(path, recursive = T){
  stopifnot(is.character(path))
  files = list.files(path, full.names=T, recursive = recursive)
  if(length(files)==0){
    return("Directory is empty.")
  }else{
    vect_size = sapply(files, function(x) file.size(x))
    size_files <-sum(vect_size)
  }
  
  return(utils:::format.object_size(size_files,"auto"))
}

#### function to list storage usage by folder under main directory ####
create_storage_log <- function(main_dir = "H:/",
                               parent_dir_save_log = "H:/utils/"){
  
  dir_save_log = glue("{parent_dir_save_log}storage_usage_log/")
  dir_save_tables = glue("{parent_dir_save_log}storage_usage_log/tables/")
  
  if(!dir.exists(dir_save_log)) dir.create(dir_save_log)
  if(!dir.exists(dir_save_tables)) dir.create(dir_save_tables)
  
  date_str = format(Sys.time(),"%Y-%m-%d%_%H%M")
  
  dirs = list.dirs(main_dir, recursive = F,full.names = T)
  
  tot_size = dir_size(main_dir)
  
  store_list = list()
  
  # get size of sub-directories
  for(dir in dirs){
    print(glue("Evaluating {dir}\n"))
    
    size_dir = dir_size(glue("{dir}/"))
    
    store_list[[length(store_list)+1]] = list(directory = dir, sorage_val = size_dir)
  }
  
  # transform list into table and sort for large folders (size>1Gb)
  dt = do.call(rbind, lapply(store_list,as.data.frame))
  setDT(dt)
  dt[, storage_val_num:=as.numeric(gsub(" Gb","",sorage_val))]
  dt = dt[order(-storage_val_num)]
  
  # log size per folder, from largest to smallest storage usage
  if(sink.number()==1){
    sink()
  }
  sink(glue("{dir_save_log}log_{date_str}.txt"),split=T)
  
  print(glue("Total storage usage in folder {main_dir} is {tot_size} \n"))
  print(glue("\n"))
  
  print(glue("Directories with the highest usage (ordered high to low for size>1Gb):\n"))
  print(glue("\n"))
  
  for(i in seq_len(nrow(dt[!is.na(storage_val_num)]))){
    dir_val = dt[!is.na(storage_val_num)][i][["directory"]]
    storage_val = dt[!is.na(storage_val_num)][i][["sorage_val"]]
    
    share = round(as.numeric(gsub(" Gb","",storage_val)) * 100/
                    as.numeric(gsub(" Gb","",tot_size)),2)
    
    print(glue("{dir_val} storage usage:\n"))
    print(glue("{storage_val} ({share}%)\n"))
  }
  
  print(glue("\n"))
  print(glue("Directories with size<1Gb:\n"))
  print(glue("\n"))
  
  for(i in seq_len(nrow(dt[is.na(storage_val_num)]))){
    dir_val = dt[is.na(storage_val_num)][i][["directory"]]
    storage_val = dt[is.na(storage_val_num)][i][["sorage_val"]]
    
    print(glue("{dir_val} storage usage:\n"))
    print(glue("{storage_val} \n"))
  }
  
  sink()
  
  fwrite(dt,glue("{dir_save_tables}log_{date_str}.csv"))
  
  print(glue("Log .txt file saved in {dir_save_log}\n"))
  print(glue("Table with storage usage per folder saved in ",
             "{dir_save_tables}log_{date_str}.csv\n"))
}

#### call function ####
create_storage_log()