library(arrow)
library(haven)
library(future)
library(DBI)
library(duckdb)
library(purrr)

make_schema <- function(df){
  types = lapply(names(df), function(x){
    col = df[[x]]
    if(is.integer(col)){
      float64()
    }else if(is.double(col)){
      float64()
    }else if(is.character(col)){
      string()
    }else if(is.logical(col)){
      bool()
    }
  })
  
  names(types) = names(df)
  
  sch = do.call(schema, types)
  sch
}


read_sav_save_parquet <- function(sav_path, parquet_path, chunk_size){
  skip = 0
  
  chunk = read_sav(sav_path,
                   skip = skip,
                   n_max = chunk_size)
  
  chunk = zap_labels(chunk)
  
  tab = Table$create(chunk)
  
  sch = make_schema(chunk)
  
  sink = FileOutputStream$create(parquet_path)
  writer = ParquetFileWriter$create(schema = sch, sink = sink, 
                                    properties = ParquetWriterProperties$create(names(sch)))
  
  writer$WriteTable(tab, chunk_size = nrow(chunk))
  skip = skip + nrow(chunk)
  cat("Wrote", skip, "rows so far\n")
  
  keep_going = T
  while(keep_going){
    # for (i in 1:100) {
    chunk = read_sav(sav_path,
                     skip = skip,
                     n_max = chunk_size)
    
    if(nrow(chunk)==0L){
      keep_going = F
      break
    }
    
    chunk = zap_labels(chunk)
    
    tab = Table$create(chunk)
    
    writer$WriteTable(tab, chunk_size = nrow(chunk))
    skip = skip + nrow(chunk)
    cat("Wrote", skip, "rows so far\n")
  }
  writer$Close()
  sink$close()
}

read_sav_save_parquet_new <- function(sav_path, out_parent_folder_path, chunk_size){
  skip = 0
  chunk_id = 1
  
  dir_name = tools::file_path_sans_ext(basename(sav_path))
  parquet_dir = glue::glue("{out_parent_folder_path}{dir_name}/")
  dir.create(out_parent_folder_path, recursive=T, showWarnings=F)
  
  process_chunk <- function(sk, cid){
    chunk = haven::read_sav(sav_path,
                            skip = sk,
                            n_max = chunk_size)
    if(nrow(chunk)==0L) return(list(bool=F, chunk_len = 0))
    chunk = haven::zap_labels(chunk)
    out_dir = file.path(parquet_dir, paste0("chunk_id=", cid))
    dir.create(out_dir, recursive=T, showWarnings=F)
    arrow::write_parquet(chunk, file.path(out_dir,"part-0.parquet"))
    return(list(bool=T, chunk_len = nrow(chunk)))
  }
  
  keep_going = T
  while(keep_going){
    # for (i in 1:100) {
    res = process_chunk(sk = skip,cid =chunk_id)
    
    if(!res$bool){
      keep_going = F
      break
    }
    
    skip = skip + res$chunk_len
    chunk_id = chunk_id + 1
    
    message(sprintf("[%s] Wrote: %d rows so far", format(Sys.time(), "%H:%M:%S"), skip))
  }
  
}

func_sav_parquet_parallel <- function(
    sav_path, 
    chunk_size,
    out_parent_folder_path,
    n_workers = 4
){
  dir_name = tools::file_path_sans_ext(basename(sav_path))
  parquet_dir = glue::glue("{out_parent_folder_path}{dir_name}/")
  dir.create(out_parent_folder_path, recursive=T, showWarnings=F)
  
  ## test if parallel is working
  rscript_path = Sys.which("Rscript")
  res_test = system2(rscript_path, args = c("-e","cat('hello')"))
  cat("\n")
  if(res_test==5){
    message(sprintf("[%s] Parallelization not available", format(Sys.time(), "%H:%M:%S"), skip))
    read_sav_save_parquet_new(sav_path = sav_path,
                              out_parent_folder_path = out_parent_folder_path,
                              chunk_size = chunk_size)
    dt = data.frame(x=1)
    arrow::write_parquet(dt, glue("./parquet_files/{dir_name}.parquet"))
    return()
  }
  
  plan(multisession, workers = n_workers)
  
  process_chunk <- function(sk, cid){
    chunk = haven::read_sav(sav_path,
                            skip = sk,
                            n_max = chunk_size)
    if(nrow(chunk)==0L) return(FALSE)
    chunk = haven::zap_labels(chunk)
    out_dir = file.path(parquet_dir, paste0("chunk_id=", cid))
    dir.create(out_dir, recursive=T, showWarnings=F)
    arrow::write_parquet(chunk, file.path(out_dir,"part-0.parquet"))
    return(TRUE)
  }
  
  eof = F
  skip = 0L
  chunk_id = 1L
  pool <- list()
  t_start = Sys.time()
  chunks_written = 0L
  
  
  message(sprintf("[%s] Starting conversion: %s", format(Sys.time(), "%H:%M:%S"), sav_path))
  message(sprintf("[%s] Workers: %d | Chunk size : %d M", format(Sys.time(), "%H:%M:%S"),n_workers, chunk_size/1000000))
  
  # seed pool with first n_workers futures
  while(length(pool)< n_workers && !eof){
    message(sprintf("[%s] Dispatching chunk %d (skip =%d)", format(Sys.time(), "%H:%M:%S"), chunk_id, skip))
    
    pool[[length(pool)+ 1L]] <- list(
      f = future(process_chunk(skip, chunk_id), packages = c("haven","arrow")),
      skip = skip
    )
    skip = skip + chunk_size
    chunk_id = chunk_id + 1L
  }
  
  # keep dispatching until eof is TRUE and pool is drained
  while(length(pool) > 0L){
    for(i in rev(seq_along(pool))){
      if(!resolved(pool[[i]]$f)) next
      
      cid = pool[[i]]$chunk_id
      wrote = value(pool[[i]]$f)
      pool[[i]] <- NULL
      
      if(!wrote){
        message(sprintf("[%s] Chunk %d reached EOF - stopping dispatch", format(Sys.time(), "%H:%M:%S"), cid))
        eof = T
        next
      }
      
      chunks_written = chunks_written +1L
      
      message(sprintf("[%s] Chunk %d written | In flight: %d | Elapsed: %.1f",
                      format(Sys.time(), "%H:%M:%S"),
                      cid, 
                      length(pool),
                      as.numeric(Sys.time()-t_start, units = "mins")))
      
      
      if(!eof){
        message(sprintf("[%s] Dispatching chunk %d (skip =%d)", format(Sys.time(), "%H:%M:%S"), chunk_id, skip))
        pool[[length(pool) + 1L]] <- list(
          f = future(process_chunk(skip, chunk_id), packages = c("haven","arrow")),
          skip = skip
        )
        skip = skip + chunk_size
        chunk_id = chunk_id + 1L
      }
    }
    
    if (length(pool)> 0L) Sys.sleep(0.4)
  }
  
  message(sprintf("[%s] Done - %d chunks written in %.1f minutes",
                  format(Sys.time(), "%H:%M:%S"),
                  chunks_written, 
                  as.numeric(Sys.time()-t_start, units = "mins")))
  
  plan(sequential)
  invisible(parquet_dir)
  
  dt = data.frame(x=1)
  arrow::write_parquet(dt, glue("./parquet_files/{dir_name}.parquet"))
}



write_parquet_partitions_db <- function(in_path, 
                                        out_parent_folder_path,
                                        partitioned_input = T,
                                        n_partitions=15){
  
  parquet_folder = tools::file_path_sans_ext(basename(in_path))
  
  out_path = glue("{out_parent_folder_path}{parquet_folder}")
  
  unlink(out_path, recursive = T)
  
  con = dbConnect(duckdb())
  
  for(p in 0:(n_partitions-1)){
    message(sprintf(
      "[%s] Writing partition %d/%d",
      format(Sys.time(), "%H:%M:%S"), p+1, n_partitions))
    
    dir.create(glue("{out_path}/partition={p}"),
               recursive = T, showWarnings = F)
    
    if(partitioned_input){
      query = glue("
                  COPY (
                  SELECT *
                  FROM read_parquet('{in_path}', hive_partitioning = true)
                  WHERE hash(RINPERSOON) % {n_partitions} = {p}
                  )
                  TO '{out_path}/partition={p}/data_0.parquet'
                  (FORMAT PARQUET, OVERWRITE_OR_IGNORE true)
                  ")
    }else{
      query = glue("
                  COPY (
                  SELECT *
                  FROM read_parquet('{in_path}')
                  WHERE hash(RINPERSOON) % {n_partitions} = {p}
                  )
                  TO '{out_path}/partition={p}/data_0.parquet'
                  (FORMAT PARQUET, OVERWRITE_OR_IGNORE true)
                  ")
    }
    
    dbExecute(con, query)
  }
  
  return(T)
}
