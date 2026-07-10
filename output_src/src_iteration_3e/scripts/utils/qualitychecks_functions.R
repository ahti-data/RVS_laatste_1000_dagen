#' @name QC_functions_overview
#' @title Content
#' @md
#' @description
#' Described functions in this document:
#' * [compare_data_tables()]
#' * [delta_outputs()]
NULL


#' Compare Two Data Tables for Structural and Value Differences
#'
#' @description
#' Compares two data tables (e.g., old vs. new output) and reports:
#' - Columns that were **added** in `data1` relative to `data2`
#' - Columns that were **removed** in `data2` relative to `data1`
#' - A data frame of **differing values**, showing:
#'   - Row identifier (`key_column`)
#'   - Column name
#'   - Value in `data1`
#'   - Value in `data2`
#'
#' The function reshapes both datasets to long format and joins them to detect
#' differences. Missing values caused by newly added / removed columns are kept.
#'
#' @param data1 A data frame or data table. Represents the "new" version.
#' @param data2 A data frame or data table. Represents the "old" version.
#' @param key_column A character string naming the identifier column used to
#'   merge the two tables. Default is `"variable"`.
#'
#' @return
#' A named list with three elements:
#' \describe{
#'   \item{added_columns}{Character vector of column names present in `data1` but not in `data2`.}
#'   \item{removed_columns}{Character vector of column names present in `data2` but not in `data1`.}
#'   \item{differing_values}{A data frame with: key column, column name, value in data1, value in data2.}
#' }
#'
#' @examples
#' \donttest{
#' data_old <- data.frame(
#'   variable = 1:3,
#'   a = c(1, 2, 3),
#'   b = c("x", "y", "z")
#' )
#'
#' data_new <- data.frame(
#'   variable = 1:3,
#'   a = c(1, 99, 3),   # changed value
#'   c = c(TRUE, FALSE, TRUE)   # new column
#' )
#'
#' compare_data_tables(data_new, data_old, key_column = "variable")
#' }
#'
#' @export
compare_data_tables <- function(data1, data2, key_column = "variable"){

  added_columns <- setdiff(names(data1), names(data2))
  removed_columns <- setdiff(names(data2), names(data1))

  merged <- full_join(data1, data2, by = key_column, suffix = c("_data1", "_data2"))

  df1_long <- data1 %>%
    tidyr::pivot_longer(-eval(key_column), names_to = "Column", values_to = "new")

  df2_long <- data2 %>%
    tidyr::pivot_longer(-eval(key_column), names_to = "Column", values_to = "old")

  differences <- df1_long %>%
    full_join(df2_long, by = c(key_column, "Column")) %>%
    filter((is.na(new) & !is.na(old)) |
             (!is.na(new) & is.na(old)) |
             (new != old))


  list(
    added_columns = added_columns,
    removed_columns = removed_columns,
    differing_values = differences
  )
}



#' Get delta between old and new output table
#'
#' Returns the absolute and relative delta of each matching cell, between the old and new table
#'
#' @param prev_output Data.table with the old output table
#' @param new_output Data.table with the old output table
#' @param profile_vars String vector with the column names that define a unique row, for example c("profile_complexity","age", "sex","year")
#' @return data.table with the absolute and relative delta of each matching cell, between the old and new table
#' @examples
#' \dontrun{
#' add examples :)
#' }
#' @export
delta_outputs <- function(prev_output, new_output,
                          profile_vars){
  ### Delta met previous output/data
  prev_output_melt <- melt(prev_output,
                           id.vars = profile_vars)
  
  new_output_melt <- melt(new_output,
                          id.vars = profile_vars)
  
  delta_dt <- merge(prev_output_melt,
                    new_output_melt,
                    by = c(profile_vars, "variable"),
                    suffixes = c("_old", "_new"))
  
  delta_dt[, delta := value_new - value_old]
  delta_dt[, proc_delta := (value_new - value_old) / value_old]
  
  return (delta_dt)
}