#' Anonymize count columns below 10
#'
#' Returns dt with anonymized columns
#'
#' @param dt data.table dt to be rounded
#' @param cols_to_round character vector with column names to be rounded
#' @return NULL: dt is changed by the function
#' @examples
#' \dontrun{
#' below_10_to_NA(dt, cols_to_round = c("a","b")) 
#' fwrite(dt,'./dt_anon.csv')
#' }
#' @export
below_10_to_NA <- function(dt, cols_to_round){
  dt[, (cols_to_round)  := lapply(.SD, function(x){
    x[x <10] = NA
  }), .SDcols = cols_to_round]
}


#' Round columns to specific multiple of input round_to, i.e. 5 or 10 
#'
#' Returns dt with rounded columns
#'
#' @param dt data.table dt to be rounded
#' @param cols_to_round character vector with column names to be rounded
#' @param round_to round to closest multiple of round_to
#' @return NULL: dt is changed by the function
#' @examples
#' \dontrun{
#' below_10_to_NA_round_to_x(dt, cols_to_round = c("a","b")) 
#' fwrite(dt,'./dt_anon.csv')
#' }
#' @export
below_10_to_NA_round_to_x <- function(dt, cols_to_round, round_to = 5){
  dt[, (cols_to_round)  := lapply(.SD, function(x){
    x[x <10] = NA
    DescTools::RoundTo(x,5)
  }), .SDcols = cols_to_round]
}


#' Add rows with missing combinations to count dt
#'
#' Returns count dataset with missing combination rows added
#'
#' @param dt count dataset that has missing combinations in rows due to count 0
#' @param grp character vector with column names used to compute counts
#' @param levels_all list that contains all unique levels of set of columns, needs elements named after columns in grp
#' @return dt: count dataset dt with missing combination rows added
add_missing_unique_combs <- function(dt, grp, levels_all){
  grp = as.character(unlist(grp))
  stopifnot(all(grp %in% names(levels_all)))
  
  lev = levels_all[grp]
  grid = do.call(CJ,c(lev, list(unique=T)))
  
  dt_extra = grid[!dt, on = grp]
  
  dt = rbindlist(list(dt, dt_extra), use.names = T,fill = T)
  
  return(dt)
}



#' Count by variables named in grp and add missing combinations to count dt
#'
#' Returns count dataset with missing combination rows added
#'
#' @param dt raw data table to be processed into counts dt
#' @param grp character vector with column names used to compute counts
#' @param levels_all list that contains all unique levels of set of columns, needs elements named after columns in grp
#' @param count_col_name name of count column, string variable, optional
#' @return dt1: count dataset dt with missing combination rows added
#' @examples
#' \dontrun{
#' grps_list_hosp_inf = 
#'   list(
#'     "yr" = c("year"),
#'     "mth_yr" = c("year","month"),
#'     "wijk_mth_yr" = c("year","month","WK_NAAM")
#'   )
#' 
#' levels_all = list(
#'   "year" = analysis_years,
#'   "month" = 1:12,
#'   "quarter" = 1:4,
#'   "age" = unique(demog[["age"]]),
#'   "WK_NAAM" = unique(demog[["WK_NAAM"]])
#' )
#' 
#' for(sheet in names(grps)){
#'   dt_res = get_counts_by_grp(dt = dt,
#'                              grp = grps[[sheet]],
#'                              levels_all = levels_all)
#'  
#'   sheets_list[[sheet]] <- dt_res
#' }
#' }
#' @export
get_counts_by_grp <- function(dt, grp,levels_all,
                              count_col_name=NULL){
  dt1 = dt[,.(count=.N), by = grp]
  
  dt1 = add_missing_unique_combs(dt = dt1,grp = grp,levels_all = levels_all)
  
  if(!is.null(count_col_name)){
    setnames(dt1, "count", count_col_name)
  }
  
  setorderv(dt1, grp)
  
  return(dt1)
}