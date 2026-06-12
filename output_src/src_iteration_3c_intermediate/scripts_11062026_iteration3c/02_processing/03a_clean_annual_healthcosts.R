# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Create data at the annual level (ZVW)
# Output: Annual data
# Last edited: 27 February 2026

rm(list = ls())
gc()
source("./src/00_inputs.R")

#### Find healthcare costs ####
calculate_zvw_kosten <- function(inflation_correction = F){
  
  df_zorgkosten <- data.table()
  for (yr in years) {
    print(yr)
    file_path <- get_path_newest(
      path = "G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/geconverteerde bestanden/",
      string_pattern = yr,
      extension = ".parquet",
      method = "max_version"
    )
    
    # ZVWKTOTAAL does not exist in 2016, calculate it (sum all ZVW variables)
    if (yr == 2016) {
      schema <- arrow::ParquetFileReader$create(file_path)
      col_names <- names(schema$GetSchema())
      cols_zvw <- col_names[grepl("^ZVW", col_names)]
      cols_zvw <- setdiff(cols_zvw, "ZVWKOPHOOGFACTOR")
      
      cols_to_select <- c(
        "RINPERSOON", cols_zvw, "NOPZVWKHUISARTSINSCHRIJF",
        "NOPZVWKHUISARTSCONSULT", "NOPZVWKHUISARTSOVERIG"
      )
      df <- arrow::read_parquet(file_path, col_select = all_of(cols_to_select))
      df <- format_data(df)
      
      df[, zvwktotaal := rowSums(.SD, na.rm = T),
         .SDcols = tolower(cols_zvw)
      ]
      df[, zvwkggzzpmtotaal := rowSums(.SD, na.rm = T),
         .SDcols = c("zvwkgenbasggz", "zvwkspecggz")
      ]
    }
    
    # ZVWKGGZZPMTOTAAL introduced from 2022, before 2022 sum ZVWKGENBASGGZ and ZVWKSPECGGZ
    if (yr %between% c(2017, 2021)) {
      cols_to_select <- c(cols_to_select_zorgkosten, "ZVWKGENBASGGZ", "ZVWKSPECGGZ")
      df <- arrow::read_parquet(file_path, col_select = all_of(cols_to_select))
      df <- format_data(df)
      
      df[, zvwkggzzpmtotaal := rowSums(.SD, na.rm = T),
         .SDcols = c("zvwkgenbasggz", "zvwkspecggz")
      ]
    }
    
    if (yr >= 2022) {
      cols_to_select <- c(cols_to_select_zorgkosten, "ZVWKGGZZPMTOTAAL")
      df <- arrow::read_parquet(file_path, col_select = all_of(cols_to_select))
      df <- format_data(df)
    }
    
    cols_to_keep <- c(tolower(cols_to_select_zorgkosten), "zvwkggzzpmtotaal")
    df <- df[, ..cols_to_keep]
    
    # If negative values, code as 0 (negligible share)
    cols_no_rin <- setdiff(names(df), "rinpersoon")
    df[, (cols_no_rin) := lapply(.SD, function(x) fifelse(x < 0, 0, x)),
       .SDcols = cols_no_rin
    ]
    
    # Adjust for inflation
    if (inflation_correction == T){
      cpi_year <- cpi[year == yr, cpi]
      df[, (cols_no_rin) :=
          lapply(.SD, function(x) x / cpi_year),
        .SDcols = cols_no_rin]
    }
    
    df[, year := yr]
    
    # Create indicators of costs > 0
    #df[, paste0("gebruikt_", cols_no_rin) := lapply(.SD, function(x) {
    #  as.integer(x > 0)
    #}), .SDcols = cols_no_rin]
    
    df[, year := yr]
    df_zorgkosten <- rbindlist(list(df_zorgkosten, df), use.names = T)
    rm(df)
    gc()
  }
  
  #### Prorate the costs ####
  df <- r_parquet_get_dt("./data/raw/overlijden_with_matched_add_demog.parquet")
  all_group_cols <- names(df)
  cols_zorgkosten <- setdiff(names(df_zorgkosten), c("rinpersoon", "year"))
  df <- merge(df, df_zorgkosten, by = "rinpersoon", all.x = T, allow.cartesian = T)
  rm(df_zorgkosten)
  gc()
  
  # Find the date of the start of the 1000 days window
  df[, start_1000_days := gbadatumoverlijden - 1000]
  
  # Create year ranges
  year_range <- data.table(
    year = years,
    year_start = as.IDate(paste0(years, "-01-01")),
    year_end = as.IDate(paste0(years, "-12-31"))
  )
  year_range[, year_days := as.integer(year_end - year_start) + 1L]
  df <- merge(df, year_range, by = "year", all.x = T)
  setorder(df, rinpersoon, year)
  
  # Find the overlap of the annual costs in the last 1000 days window
  df[, overlap_start := pmax(start_1000_days, year_start)]
  df[, overlap_end := pmin(gbadatumoverlijden, year_end)]
  df[, overlap_days := pmax(0L, as.integer(overlap_end - overlap_start) + 1L)]
  
  # Drop people with no observed costs
  df <- df[complete.cases(zvwkhuisarts)]
  
  # Drop rows with no overlap
  df <- df[overlap_days > 0]
  
  # Adjust the costs by the proportion that the year falls within the last 1000 days
  df[, (cols_zorgkosten) := lapply(.SD, function(x) {
    x * overlap_days / year_days
  }),
  .SDcols = cols_zorgkosten
  ]
  
  # Calculate costs in the last 1000 days in 30 days windows
  df[, seg_start := as.integer(gbadatumoverlijden - overlap_end)]
  df[, seg_end := as.integer(gbadatumoverlijden - overlap_start)]
  
  df[, days_observed_year := fifelse(
    year == year(gbadatumoverlijden),
    as.integer(gbadatumoverlijden - year_start) + 1L,
    year_days
  )]
  
  df[, (cols_zorgkosten) := lapply(.SD, function(x) {
    x / days_observed_year
  }),
  .SDcols = cols_zorgkosten
  ]
  
  # Get vectors of costs and usage
  cols_zorgkosten_kosten <- cols_zorgkosten[grepl("^(zvw|nopzvw)", cols_zorgkosten)]
  cols_zorgkosten_gebruikt <- cols_zorgkosten[grepl("^(gebruikt)", cols_zorgkosten)]
  
  # Adjust usage to 0 and 1
  df[, (cols_zorgkosten_gebruikt) := lapply(.SD, function(x) as.integer(x > 0)),
     .SDcols = cols_zorgkosten_gebruikt
  ]
  
  # Create bins
  step_days <- 30L
  n_bins <- floor(1000 / step_days)
  bins <- data.table(t = 1:(n_bins + 1L))
  bins[, ":="(bin_start = (t - 1L) * step_days,
              bin_end = pmin(t * step_days - 1L, 1000L))]
  
  setkey(df, seg_start, seg_end)
  setkey(bins, bin_start, bin_end)
  df <- foverlaps(df, bins,
                  by.x = c("seg_start", "seg_end"),
                  by.y = c("bin_start", "bin_end"),
                  mult = "all",
                  type = "any",
                  nomatch = NA
  )
  
  df[, overlap := pmax(0L, pmin(seg_end, bin_end) - pmax(seg_start, bin_start) + 1L)]
  df[, (cols_zorgkosten_kosten) := lapply(.SD, function(x) {
    x * overlap
  }),
  .SDcols = cols_zorgkosten_kosten
  ]
  
  # Merge 34th month with 33th
  df <- df[t == 34, t := 33]
  
  # Calculate costs in the last 1000 days (include the date of death because
  # randomly matched alive people might have different "death dates" because they
  # are matched to multiple deceased people)
  df_binned_kosten <- df[, lapply(.SD, sum, na.rm = T),
                         by = all_group_cols,
                         .SDcols = cols_zorgkosten_kosten
  ]
  gc()
  
  df_binned_gebruikt <- df[, lapply(.SD, max, na.rm = T),
                           by = all_group_cols,
                           .SDcols = cols_zorgkosten_gebruikt
  ]
  rm(df)
  gc()
  
  df_binned <- merge(df_binned_kosten, df_binned_gebruikt, all = T,
                     by = all_group_cols
  )
  rm(df_binned_kosten, df_binned_gebruikt)
  gc()
  
  # If costs and usage are NA, code them as 0
  df <- r_parquet_get_dt("./data/raw/overlijden_with_matched_add_demog.parquet")
  n_total <- nrow(df)
  df_binned <- merge(df, df_binned, all.x = T,
                     by = all_group_cols)
  rm(df)
  stopifnot(nrow(df_binned) == n_total)
  
  cols_to_fill <- setdiff(names(df_binned), all_group_cols)
  setnafill(df_binned, fill = 0, cols = cols_to_fill)
  return(df_binned)
}

zvw_1000_dagen <- calculate_zvw_kosten(inflation_correction = F)
setindex(zvw_1000_dagen, NULL)
arrow::write_parquet(zvw_1000_dagen, "./data/processed/zvw_1000_dagen.parquet")
rm(zvw_1000_dagen)
gc()

zvw_1000_dagen_corrected <- calculate_zvw_kosten(inflation_correction = T)
setindex(zvw_1000_dagen_corrected, NULL)
arrow::write_parquet(zvw_1000_dagen_corrected, "./data/processed/zvw_1000_dagen_corrected.parquet")
rm(zvw_1000_dagen_corrected)
gc()

