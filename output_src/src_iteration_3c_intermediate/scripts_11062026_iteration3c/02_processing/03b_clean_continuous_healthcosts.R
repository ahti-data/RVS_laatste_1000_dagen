# Project: Laatste 1000 dagen
# Author:  Marco Griep & Stanislav Avdeev
# Goal: Create monthly data on costs for MSZ, Wlz, Wijk, Huisarts
# Output: Microdata
# Last edited: 21 April 2026

#### initialize ####
rm(list = ls())
gc()

source("src/00_inputs.R")
library(glue)
library(dplyr)

# Load in general dataset
dt_overlijden_with_matched <- r_parquet_get_dt("data/raw/overlijden_with_matched_add_demog.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)

cost_years_to_load <- 2016:2023

#### MSZ costs ####
# load in filtered prestaties, chunk because of large size
n_chunks <- 10
rinpersoon_set_chunks <- split(rinpersoon_set, cut(seq_along(rinpersoon_set), n_chunks, labels = FALSE))

for (i in seq_along(rinpersoon_set_chunks)) {
  rinpersoon_set_chunk <- rinpersoon_set_chunks[[i]]
  print(glue("Currently processing chunk {i}/{n_chunks} for mszprestaties"))
  dt_mszprest_costs <- list()
  
  for (yr in cost_years_to_load) {
    print(glue("Currently processing {yr} for mszprestaties"))
    
    file_path <- get_newest_parquet_check(
      folder_g_parquet = NULL,
      folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files/",
      folder_g_sav = 'G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/',
      string_pattern_parquet = yr,
      string_pattern_sav = yr,
      stop_on_mismatch = F)
    file_path = tools::file_path_sans_ext(file_path)
    
    ds <- arrow::open_dataset(file_path)
    dt <- ds |>
      # REVIEW: add as.numeric(RINPERSOON)
      filter(RINPERSOON %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(c(
          "rinpersoon",
          "vektmszbegindatumprest",
          "vektmszeinddatumprest",
          "vektmszvergoedbedragzvw"))])) |>
      collect()
    dt <- format_data(dt)
    dt_mszprest_costs[[yr]] <- dt
    rm(dt)
    gc()
  }
  
  dt_mszprest_costs <- rbindlist(dt_mszprest_costs, use.names = T)
  
  dt_mszprest_monthly <- calculate_costs_by_bin_size(
    dt_mszprest_costs,
    dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set_chunk],
    cost_columns = 'vektmszvergoedbedragzvw',
    cost_date_col = "vektmszbegindatumprest",
    bin_size = "months33",
    aggregate_groupby_cols = names(dt_overlijden_with_matched)
  )
  
  # Save
  setindex(dt_mszprest_monthly, NULL)
  arrow::write_parquet(dt_mszprest_monthly, 
                       glue::glue("data/raw/vektmszkosten_monthly_{i}.parquet"))
  rm(dt_mszprest_monthly)
  gc()
  
  # Corrected for inflation
  dt_mszprest_monthly_corrected <- calculate_costs_by_bin_size(
    dt_mszprest_costs,
    dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set_chunk],
    cost_columns = 'vektmszvergoedbedragzvw',
    cost_date_col = "vektmszbegindatumprest",
    bin_size = "months33",
    aggregate_groupby_cols = names(dt_overlijden_with_matched),
    inflation_correction = T
  )
  
  # Save
  setindex(dt_mszprest_monthly_corrected, NULL)
  arrow::write_parquet(dt_mszprest_monthly_corrected, 
                       glue::glue("data/raw/vektmszkosten_monthly_corrected_{i}.parquet"))
  rm(dt_mszprest_monthly_corrected, dt_mszprest_costs)
  gc()
}

for (outcome in c('vektmszkosten_monthly', 'vektmszkosten_monthly_corrected')){
  print(outcome)
  data_list <- list()
  
  for (i in 1:n_chunks){ 
    df <- r_parquet_get_dt(glue::glue("./data/raw/{outcome}_{i}.parquet")) 
    data_list[[i]] <- df
    rm(df)
    gc()
  }
  data_list <- rbindlist(data_list, use.names = T)
  setindex(data_list, NULL)
  arrow::write_parquet(data_list, glue::glue("./data/processed/{outcome}.parquet"))
  rm(data_list)
  gc()
}


#### WLZZINTAB ####
print("Processing WLZ costs")

dt_wlz <- load_dataset(cost_years_to_load, "WLZZINTAB", 
                       cols = c("rinpersoon", "beginwlzzin", "bedragwlzzin"),
                       rinpersoon_chunk = rinpersoon_set)

dt_wlz_monthly <- calculate_costs_by_bin_size(
  dt_wlz,
  dt_overlijden_with_matched,
  cost_columns = "bedragwlzzin",
  cost_date_col = "beginwlzzin",
  bin_size = "months33",
  aggregate_groupby_cols = names(dt_overlijden_with_matched)
)

# Save
setindex(dt_wlz_monthly, NULL)
arrow::write_parquet(dt_wlz_monthly, "data/processed/wlzkosten_monthly.parquet")
rm(dt_wlz_monthly)
gc()

# Correct for inflation
dt_wlz_monthly_corrected <- calculate_costs_by_bin_size(
  dt_wlz,
  dt_overlijden_with_matched,
  cost_columns = "bedragwlzzin",
  cost_date_col = "beginwlzzin",
  bin_size = "months33",
  aggregate_groupby_cols = names(dt_overlijden_with_matched),
  inflation_correction = T
)

# Save
setindex(dt_wlz_monthly_corrected, NULL)
arrow::write_parquet(dt_wlz_monthly_corrected, "data/processed/wlzkosten_monthly_corrected.parquet")
rm(dt_wlz_monthly_corrected, dt_wlz)
gc()

#### Add-on geneesmiddelen ####
print("Processing msz addon costs")
dt_msz_addon <- load_dataset(2017:2023, "MSZDGADDONGENEESMVEKTTAB", cols = c(
  "rinpersoon", "vektmszvergoedbedragzvwdg", 
  "vektmszhoofdgroepsubgroepdg", "vektmszdatumdg"),
  rinpersoon_chunk = rinpersoon_set
)

setnames(dt_msz_addon, 'vektmszvergoedbedragzvwdg', 'bedrag')

# Group codes in categories 01 and 16 together
dt_msz_addon[, vektmszhoofdgroepsubgroepdg := ifelse(substr(vektmszhoofdgroepsubgroepdg, 1, 2) == '01', 
                       '01',
                       vektmszhoofdgroepsubgroepdg)]
dt_msz_addon[, vektmszhoofdgroepsubgroepdg := ifelse(substr(vektmszhoofdgroepsubgroepdg, 1, 2) == '16', 
                                                     '16',
                                                     vektmszhoofdgroepsubgroepdg)]

cats_to_split <- unique(dt_msz_addon$vektmszhoofdgroepsubgroepdg)
for (cat in cats_to_split) {
  new_col_name <- paste0("bedrag_", cat)
  dt_msz_addon[, (new_col_name) := 0]
  dt_msz_addon[vektmszhoofdgroepsubgroepdg == cat, (new_col_name) := bedrag]
}

# Sum of oncology costs
dt_msz_addon[, bedrag_13_01_88 := rowSums(.SD, na.rm = T), 
             .SDcols = c('bedrag_13_01', 'bedrag_13_02', 
                         'bedrag_13_03', 'bedrag_13_04', 
                         'bedrag_13_05', 'bedrag_13_06', 
                         'bedrag_13_07', 'bedrag_13_08',
                         'bedrag_13_88')]
cost_cols <- grep("bedrag_", names(dt_msz_addon), value = T)

# Count the number of declarations
n_cols <- sub('bedrag_', 'n_', cost_cols)
dt_msz_addon[, (n_cols) := lapply(.SD, function(x) 
  as.integer(x > 0)),
  .SDcols = cost_cols
]

costs_n <- c(cost_cols, n_cols)
cost_butches <- split(costs_n, cut(seq_along(costs_n), breaks = 10, labels = F))

# Calculate costs in bins
results <- vector('list', length(cost_butches)) 
for (costs_i in seq_along(cost_butches)){
  cat('Running batch: ', costs_i)
  results[[costs_i]] <- calculate_costs_by_bin_size(
    dt_msz_addon[, c('rinpersoon', 'vektmszdatumdg', 
                     cost_butches[[costs_i]]),
                 with = F],
    dt_overlijden_with_matched,
    cost_columns = cost_butches[[costs_i]],
    cost_date_col = "vektmszdatumdg",
    bin_size = "months33",
    aggregate_groupby_cols = names(dt_overlijden_with_matched)
  )
}
rm(dt_msz_addon)
gc()

dt_msz_addon_monthly <- Reduce(function(x, y) 
  merge(x, y, by = c(names(dt_overlijden_with_matched), 't'),
        all = T),
  results)
rm(results)
gc()

# Save total oncology separately
cols_to_select_oncology_total <- setdiff(names(dt_msz_addon_monthly), 
                                         grep("bedrag_|n_", names(dt_msz_addon_monthly), value = T))
cols_to_select_oncology_total <- c(cols_to_select_oncology_total, 'inkomen_klasse',
                                   'bedrag_13_01_88', 'n_13_01_88')

dt_msz_addon_oncology_total_monthly <- dt_msz_addon_monthly[, ..cols_to_select_oncology_total]
setindex(dt_msz_addon_oncology_total_monthly, NULL)
arrow::write_parquet(dt_msz_addon_oncology_total_monthly, 
                     "data/processed/msz_addon_oncology_total_monthly.parquet")

# Save only people who died from cancer as we need to calculate aggregation split
# by age and income for this group
dt_msz_addon_oncology_total_cancer_monthly <- dt_msz_addon_oncology_total_monthly[
  doodsoorzaak == 'Palliatief kanker'
]
setindex(dt_msz_addon_oncology_total_cancer_monthly, NULL)
arrow::write_parquet(dt_msz_addon_oncology_total_cancer_monthly, 
                     "data/processed/msz_addon_oncology_total_cancer_monthly.parquet")
rm(dt_msz_addon_oncology_total_cancer_monthly, dt_msz_addon_oncology_total_monthly)

# Save outcomes related to cancer only for people who died from cancer
dt_msz_addon_monthly[, c('bedrag_13_01_88', 'n_13_01_88') := NULL]

dt_msz_addon_monthly_oncology <- dt_msz_addon_monthly[doodsoorzaak == 'Palliatief kanker']
cols_to_select_oncology <- setdiff(names(dt_msz_addon_monthly_oncology), 
                                         grep("bedrag_|n_", names(dt_msz_addon_monthly_oncology), value = T))
cols_to_select_oncology <- c(cols_to_select_oncology, 'inkomen_klasse',
                             grep("bedrag_13_|n_13_", names(dt_msz_addon_monthly), value = T))

dt_msz_addon_monthly_oncology <- dt_msz_addon_monthly_oncology[, ..cols_to_select_oncology]

setindex(dt_msz_addon_monthly_oncology, NULL)
arrow::write_parquet(dt_msz_addon_monthly_oncology, "data/processed/msz_addon_oncology_cancer_monthly.parquet")
rm(dt_msz_addon_monthly_oncology)
gc()

# Save non-oncology outcome for everyone
setindex(dt_msz_addon_monthly, NULL)
arrow::write_parquet(dt_msz_addon_monthly, "./data/processed/msz_addon_monthly.parquet")
rm(dt_msz_addon_monthly)
gc()
  