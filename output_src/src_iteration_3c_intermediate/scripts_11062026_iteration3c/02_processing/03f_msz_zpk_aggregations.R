#### Initialize ####

rm(list=ls())
gc()
source("src/00_inputs.R")

#### TODO ####



#### functions ####
create_prestaties_aggregations_by_category <- function(dt, groupby_cols_always, cols_to_create_all) {

  dt[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]
  
  add_n_totaal <- function(dt_aggregated, dt_original) {
    all_potential_population_group_cols <- c("died", "cohort", "doodsoorzaak")
    actual_population_group_cols <- intersect(all_potential_population_group_cols, names(dt_aggregated))
    
    dt_population_sizes <- dt_original[, .(n_totaal_population = fnunique(sample_id)), by = actual_population_group_cols]
    
    dt_aggregated <- merge(
      dt_aggregated,
      dt_population_sizes,
      by = actual_population_group_cols
    )
    
    return(dt_aggregated)
  }
  
  # first, aggregate with all splits
  dt_agg <- dt[, .(
    n_totaal_gebruikers = fnunique(sample_id),
    n_totaal_declaraties = .N,
    sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm=T),
    median_cost_per_declaratie = fmedian(vektmszvergoedbedragzvw, na.rm=T),
    n_instellingen = fnunique(vektmszinstellingprest),
    share_main_instelling = sum(vektmszvergoedbedragzvw[vektmszinstellingprest == fmode(vektmszinstellingprest)], na.rm=T) / sum(vektmszvergoedbedragzvw, na.rm=T)
  ), by = c(groupby_cols_always, cols_to_create_all)]
  
  # add n_totaal_population
  dt_agg <- add_n_totaal(dt_agg, dt)
  
  
  if (is.null(cols_to_create_all)) {
    return(dt_agg)
  }
  
  # then, aggregate with "all", where we set all demog vars to "all"
  dt_agg_all <- dt[, .(
    n_totaal_gebruikers = fnunique(sample_id),
    n_totaal_declaraties = .N,
    sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm=T),
    median_cost_per_declaratie = fmedian(vektmszvergoedbedragzvw, na.rm=T),
    n_instellingen = fnunique(vektmszinstellingprest),
    share_main_instelling = sum(vektmszvergoedbedragzvw[vektmszinstellingprest == fmode(vektmszinstellingprest)], na.rm=T) / sum(vektmszvergoedbedragzvw, na.rm=T)
  ), by = c(groupby_cols_always)][, (cols_to_create_all) := "all"]
  
  # add n totaal population
  dt_agg_all <- add_n_totaal(dt_agg_all, dt)
  
  
  # then, create all combinations
  if (length(cols_to_create_all) > 1) {
    dt_agg_all_combinations <- rbindlist(lapply(cols_to_create_all, function(target_col) {
      non_target_cols <- setdiff(cols_to_create_all, target_col)
      dt_target_agg <- dt[, .(
        n_totaal_gebruikers = fnunique(sample_id),
        n_totaal_declaraties = .N,
        sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm=T),
        median_cost_per_declaratie = fmedian(vektmszvergoedbedragzvw, na.rm=T),
        n_instellingen = fnunique(vektmszinstellingprest),
        share_main_instelling = sum(vektmszvergoedbedragzvw[vektmszinstellingprest == fmode(vektmszinstellingprest)], na.rm=T) / sum(vektmszvergoedbedragzvw, na.rm=T)
      ), by = c(groupby_cols_always, non_target_cols)][, (target_col) := "all"]
      
      return(dt_target_agg)
    }), use.names = TRUE)
    
    dt_agg_all_combinations <- add_n_totaal(dt_agg_all_combinations, dt)
    
    return(rbindlist(list(
      dt_agg, dt_agg_all, dt_agg_all_combinations
    ), use.names=T))
  }

  
  return(rbindlist(list(
    dt_agg, dt_agg_all
  ), use.names=T))
}

create_activiteiten_aggregations_by_category <- function(dt, groupby_cols_always, cols_to_create_all) {
  
  add_n_totaal <- function(dt_aggregated, dt_original) {
    all_potential_population_group_cols <- c("died", "cohort", "doodsoorzaak")
    actual_population_group_cols <- intersect(all_potential_population_group_cols, names(dt_aggregated))
    
    dt_population_sizes <- dt_original[, .(n_totaal_population = fnunique(sample_id)), by = actual_population_group_cols]
    
    dt_aggregated <- merge(
      dt_aggregated,
      dt_population_sizes,
      by = actual_population_group_cols
    )
    
    return(dt_aggregated)
  }
  
  # first, aggregate with all splits
  dt_agg <- dt[, .(
    n_totaal_gebruikers = fnunique(sample_id),
    n_totaal_activiteiten = .N,
    n_instellingen = fnunique(vektmszinstellingza)
  ), by = c(groupby_cols_always, cols_to_create_all)]
  
  dt_agg <- add_n_totaal(dt_agg, dt)
  
  
  if (is.null(cols_to_create_all)) {
    return(dt_agg)
  }
  
  # then, aggregate with "all", where we set all demog vars to "all"
  dt_agg_all <- dt[, .(
    n_totaal_gebruikers = fnunique(sample_id),
    n_totaal_activiteiten = .N,
    n_instellingen = fnunique(vektmszinstellingza)
  ), by = c(groupby_cols_always)][, (cols_to_create_all) := "all"]
  
  # then, create all combinations
  dt_agg_all_combinations <- rbindlist(lapply(cols_to_create_all, function(target_col) {
    non_target_cols <- setdiff(cols_to_create_all, target_col)
    dt_target_agg <- dt[, .(
      n_totaal_gebruikers = fnunique(sample_id),
      n_totaal_activiteiten = .N,
      n_instellingen = fnunique(vektmszinstellingza)
    ), by = c(groupby_cols_always, non_target_cols)][, (target_col) := "all"]
    
    return(dt_target_agg)
  }), use.names = TRUE)
  
  dt_agg_all <- add_n_totaal(dt_agg_all, dt)
  dt_agg_all_combinations <- add_n_totaal(dt_agg_all_combinations, dt)
  
  
  return(rbindlist(list(
    dt_agg, dt_agg_all, dt_agg_all_combinations
  ), use.names=T))
}

#### 1: create distributions for OZPs ####

# load in processed datasets
dt_mszprest_OZP_overlapped <- r_parquet_get_dt("data/processed/mszprest_OZP_overlapped.parquet")

# Drop NA rows (people who had no matched prestaties)
dt_mszprest_OZP_overlapped <- dt_mszprest_OZP_overlapped[!is.na(zpk_category)]

# aggregate OZPs for bin size 30days, and 1000days
dt_mszprest_OZP_agg <- create_prestaties_aggregations_by_category(
  dt_mszprest_OZP_overlapped,
  groupby_cols_always = c("zpk_category", "t", "cohort", "died"),
  cols_to_create_all = c("vektmszsettingzpk")
)[order(cohort, died, vektmszsettingzpk, -t, zpk_category)][, bin_size := "monthly"]

dt_mszprest_OZP_agg_1000 <- create_prestaties_aggregations_by_category(
  dt_mszprest_OZP_overlapped,
  groupby_cols_always = c("zpk_category", "cohort", "died"),
  cols_to_create_all = c("vektmszsettingzpk")
)[order(cohort, died, vektmszsettingzpk, zpk_category)][, bin_size := "1000d"][, t := -1]

# combine 1000days and 30d versions
dt_mszprest_OZP_agg_final <- rbindlist(list(dt_mszprest_OZP_agg, dt_mszprest_OZP_agg_1000), use.names=T)
invalid_counts <- dt_mszprest_OZP_agg_final[, .(
  n_invalid = sum(n_instellingen < 3 | share_main_instelling > 0.5, na.rm=T),
  n_t = uniqueN(t)
), 
by = .(died, cohort, zpk_category, vektmszsettingzpk, bin_size)]

# write
openxlsx2::write_xlsx(list(
  "figures" = dt_mszprest_OZP_agg_final,
  "invalid_counts" = invalid_counts
), "data/processed/msz_prestaties_OZP_agg.xlsx")

rm(dt_mszprest_OZP_agg, dt_mszprest_OZP_overlapped, dt_mszprest_OZP_agg_1000, dt_mszprest_OZP_agg_final)
gc()


#### 2: create distributions for DBCs ####
# load in processed datasets
dt_mszprest_DBC_overlapped <- r_parquet_get_dt("data/processed/mszprest_DBC_overlapped.parquet")

# Drop NA rows (people who had no matched prestaties)
dt_mszprest_DBC_overlapped <- dt_mszprest_DBC_overlapped[!is.na(zpk_category)]

# aggregate DBCs for bin size 30days, and 1000days
dt_mszprest_DBC_agg <- create_prestaties_aggregations_by_category(
  dt_mszprest_DBC_overlapped,
  groupby_cols_always = c("zpk_category", "t", "cohort", "died"),
  cols_to_create_all = c("vektmszsettingzpk")
)[order(cohort, died, vektmszsettingzpk, -t, zpk_category)][, bin_size := "monthly"]

dt_mszprest_DBC_agg_1000 <- create_prestaties_aggregations_by_category(
  dt_mszprest_DBC_overlapped,
  groupby_cols_always = c("zpk_category", "cohort", "died"),
  cols_to_create_all = c("vektmszsettingzpk")
)[order(cohort, died, vektmszsettingzpk, zpk_category)][, bin_size := "1000d"][, t := -1]

# combine 1000days and 30d versions
dt_mszprest_DBC_agg_final <- rbindlist(list(dt_mszprest_DBC_agg, dt_mszprest_DBC_agg_1000), use.names=T)
invalid_counts <- dt_mszprest_DBC_agg_final[, .(
  n_invalid = sum(n_instellingen < 3 | share_main_instelling > 0.5, na.rm=T),
  n_t = uniqueN(t)
), 
by = .(died, cohort, zpk_category, vektmszsettingzpk, bin_size)]

# write
openxlsx2::write_xlsx(list(
  "figures" = dt_mszprest_DBC_agg_final,
  "invalid_counts" = invalid_counts
), "data/processed/msz_prestaties_DBC_agg.xlsx")

rm(dt_mszprest_DBC_agg, dt_mszprest_DBC_overlapped, dt_mszprest_DBC_agg_1000, dt_mszprest_DBC_agg_final)
gc()

#### 3: create top 50 tables for activiteiten ####
# load in datasets
dt_mszact_overlapped <- r_parquet_get_dt("data/processed/mszact_categorized_overlapped.parquet")

# Drop NA rows (people who had no matched prestaties)
dt_mszact_overlapped <- dt_mszact_overlapped[!is.na(vektmszzorgactiviteit)]

# drop unnecessary cols
dt_mszact_overlapped[, c("start_temp", "end_temp", 
                         "bin_end", "bin_start", "vektmszkoppelidprestza", "vektmszbeginjaarprest",
                         "vektmszzorgactiviteitdatum", "gbadatumoverlijden") := NULL]

# first, create aggregations
dt_mszact_agg_1000 <- create_activiteiten_aggregations_by_category(
  dt_mszact_overlapped,
  groupby_cols_always = c("cohort", "died", "vektmszzorgactiviteit", "zpk_category"),
  cols_to_create_all = NULL
)[order(cohort, died)][,bin_size := "1000d"]

dt_mszact_agg_30 <- create_activiteiten_aggregations_by_category(
  dt_mszact_overlapped[t == -1],
  groupby_cols_always = c("cohort", "died", "vektmszzorgactiviteit", "zpk_category"),
  cols_to_create_all = NULL
)[order(cohort, died)][,bin_size := "30d"]

dt_mszact_agg <- rbindlist(list(dt_mszact_agg_1000, dt_mszact_agg_30))
dt_mszact_agg[died == "In leven", died := "In_leven"]
rm(dt_mszact_agg_1000, dt_mszact_agg_30)
gc()

ranking_variables <- c("n_totaal_gebruikers")
indicator_variables <- c("n_totaal_gebruikers", "n_instellingen")

dt_results <- rbindlist(lapply(split(dt_mszact_agg, by = c("zpk_category", "bin_size", "cohort", "died")), function(dt) {
  
  dt[died == "In leven", died := "In_leven"]
  
  # get split metadata
  split_zpk_category <- dt[1]$zpk_category
  split_bin_size <- dt[1]$bin_size
  split_cohort <- dt[1]$cohort
  split_died <- dt[1]$died
  
  dt_ranked_by_variable <- rbindlist(lapply(ranking_variables, function(ranking_var) {
    dt_ranked <- copy(dt)
    dt_ranked <- dt_ranked[, ':='(
      ranking = frank(-get(ranking_var)),
      ranked_by = paste(split_zpk_category, split_bin_size, split_cohort, split_died, ranking_var, sep = "_")
    )][ranking <= 50]
    
    vars_to_compare <- list(
      "died" = c("Overleden", "In_leven"),
      "bin_size" = c("30d", "1000d")
    )

    for (comparison_var in names(vars_to_compare)) {
      split_comparison_var_value <- dt[, .SD, .SDcols = comparison_var][1]
      opposite_split_comparison_var_value <- setdiff(vars_to_compare[[comparison_var]], split_comparison_var_value)
      
      
      dt_temp <- copy(dt_mszact_agg)
      
      if (comparison_var == "bin_size"){
        dt_temp <- dt_temp[get(comparison_var) == opposite_split_comparison_var_value 
                           & died == split_died 
                           & cohort == split_cohort
                           & zpk_category == split_zpk_category
        ]
      } else {
        dt_temp <- dt_temp[get(comparison_var) == opposite_split_comparison_var_value 
                           & bin_size == split_bin_size
                           & cohort == split_cohort
                           & zpk_category == split_zpk_category
        ]
      }
      opposite_col_names <- paste0(indicator_variables, "_", opposite_split_comparison_var_value)
      
      setnames(dt_temp, 
               indicator_variables,
               opposite_col_names
      )
      
      dt_ranked <- merge(
        dt_ranked, 
        dt_temp[, .SD, .SDcols = c("vektmszzorgactiviteit", opposite_col_names)], 
        by = "vektmszzorgactiviteit",
        all.x=T
      )
    }
    
    # browser()
    return(dt_ranked)
    
  }), use.names=T)
  
  return(dt_ranked_by_variable[order(-ranking)])
}), fill=T)

# add the reflijst descriptions to the results
reflijst_activiteiten <- load_reflijst_activiteiten(c("mszzorgactiviteit", "mszzorgactiviteitomschrijving"))

dt_results <- merge_with_validate(
  dt_results,
  reflijst_activiteiten,
  by.x = "vektmszzorgactiviteit",
  by.y = "mszzorgactiviteit",
  require_match = "left"
)

# determine how many rows are invalid by ranked_by
invalid_counts <- dt_results[, .(
  invalid_ranked_col = sum(n_instellingen < 3, na.rm=T),
  invalid_comparison_30d = sum(n_instellingen_30d < 3, na.rm=T),
  invalid_comparison_1000d = sum(n_instellingen_1000d < 3, na.rm=T),
  invalid_comparison_Overleden = sum(n_instellingen_Overleden < 3, na.rm=T),
  invalid_comparison_In_leven = sum(n_instellingen_In_leven < 3, na.rm=T)
), by="ranked_by"]

openxlsx2::write_xlsx(list(
  "figures" = dt_results,
  "invalid_counts" = invalid_counts
), "data/processed/top50_mszact_codes_by_category.xlsx")


#### TEST ####
# test_dbc <- arrow::open_dataset("data/processed/mszprest_DBC_overlapped.parquet") |>
#   select(c("rinpersoon", "id_prestatie", "zpk_category")) |>
#   collect() |>
#   as.data.table()
#   
# test_dbc[, .(.N), by = "zpk_category"]
# 
# test_OZP <- arrow::open_dataset("data/processed/mszprest_OZP_overlapped.parquet") |>
#   select(c("rinpersoon", "id_prestatie", "zpk_category", "cohort", "died")) |>
#   collect() |>
#   as.data.table()
# 
# test_OZP[, .(.N), by = c("zpk_category", "cohort", "died")]
