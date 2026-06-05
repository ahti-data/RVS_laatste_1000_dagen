rm(list=ls())
gc()

#### TODO ####

# add the n_totaal for the top20 counts: DONE
# add a third ranking category, median cost per declaratie: DONE
# Do top 50 instead of top 20: DONE
# Add comparison for overleden vs. in leven : DONE

# add n_totaal for the first output as well: DONE
# median costs as well: DONE
# add the 1000 day total for the binned analysis as well: DONE

source("src/00_inputs.R")

#### functions ####
create_aggregations_by_category <- function(dt, groupby_cols_always, cols_to_create_all) {
  
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
  
  # then, create all combinations
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
  
  dt_agg_all <- add_n_totaal(dt_agg_all, dt)
  dt_agg_all_combinations <- add_n_totaal(dt_agg_all_combinations, dt)
  
  
  return(rbindlist(list(
    dt_agg, dt_agg_all, dt_agg_all_combinations
  ), use.names=T))
}

#### analyse 1: Classification of prestaties into categories ####
dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)
# rinpersoon_set <- sample(rinpersoon_set, 5000) # for development


dt_mszact <- load_dataset(
  years,
  "MSZZORGACTIVITEITENVEKTTAB",
  cols = c("rinpersoon", "vektmszkoppelidprestza", "vektmszzorgactiviteitdatum", "vektmszzorgactiviteit"),
  rinpersoon_chunk = rinpersoon_set
)
# dt_mszact_raw <- dt_mszact # for development

# load and clean reflijst
reflijst_zorgactiviteiten <- rio::import("K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods")
reflijst_zorgactiviteiten <- format_data(reflijst_zorgactiviteiten, rin_num = F)[, .SD, .SDcols = c(
  "mszzorgactiviteit", "zpkcode"
  )]
reflijst_zorgactiviteiten[, mszzorgactiviteit := as.character(mszzorgactiviteit)]
reflijst_zorgactiviteiten[nchar(mszzorgactiviteit) == 5, mszzorgactiviteit := paste0("0", mszzorgactiviteit)]

# small guard clause to ensure that merging the reflijst is going well
assertthat::assert_that(sum(!unique(dt_mszact$vektmszzorgactiviteit) %in% unique(reflijst_zorgactiviteiten$mszzorgactiviteit)) <= 1)

reflijst_zorgactiviteiten[, ':='(
  oper_verr = as.integer(zpkcode == 5),
  ovg_ther_ver = as.integer(zpkcode == 6),
  ovg_zpk = as.integer(!zpkcode %in% c(5, 6))
)]

dt_mszact <- merge(
  dt_mszact,
  reflijst_zorgactiviteiten,
  all.x=T,
  by.x = "vektmszzorgactiviteit",
  by.y = "mszzorgactiviteit"
)

# load and merge prestaties
dt_mszprest <- load_dataset(
  years,
  "mszprestatiesvekttab",
  cols = c("rinpersoon", "vektmszkoppelidprestza", "vektmszbegindatumprest", "vektmszvergoedbedragzvw", "vektmszdeclaratiecode", "vektmszsettingzpk", "vektmszdbczorgproduct", "vektmszinstellingprest"),
  rinpersoon_chunk = rinpersoon_set
)

# Process OZP and other prestaties separately, bind together later
dt_mszprest_OZP <- dt_mszprest[!substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")]
dt_mszprest_other <- dt_mszprest[substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")]
dt_mszprest_other[, vektmszdeclaratiecode := NULL] # this column is currently useless for non-OZPs. will be replaced by activiteiten column
rm(dt_mszprest)
gc()

# merge non-OZP prestaties and activiteiten
dt_mszprest_other <- merge(
  dt_mszprest_other,
  dt_mszact,
  by = c("vektmszkoppelidprestza", "rinpersoon"),
  all.x=T
)

# rename the column to vektmszdeclaratiecode, so we can get the counts later
setnames(dt_mszprest_other, "vektmszzorgactiviteit", "vektmszdeclaratiecode")

# Make dummy cols based on underlying activiteiten
dt_mszprest_other <- dt_mszprest_other[, .(
  oper_verr = max(oper_verr, na.rm=T),
  ovg_ther_ver = max(ovg_ther_ver, na.rm=T),
  ovg_zpk = max(ovg_zpk, na.rm=T)),
  by = .(rinpersoon, vektmszbegindatumprest, vektmszdbczorgproduct, vektmszdeclaratiecode, vektmszvergoedbedragzvw, vektmszsettingzpk, vektmszinstellingprest, zpkcode)
  ]

# convert the three dummy cols to a single "zpk_category" col
dt_mszprest_other[, zpk_category := fcase(
  oper_verr == 1, "oper_verr",
  ovg_ther_ver == 1 & oper_verr != 1, "ovg_ther_ver",
  (ovg_ther_ver != 1 & oper_verr != 1) | is.na(zpkcode), "ovg_zpk" # NA's should be set to overig
)][, c("oper_verr", "ovg_ther_ver", "ovg_zpk", "zpkcode") := NULL]

rm(dt_mszact)
gc()

## now apply similar logic to the OZP's
# small guard clause to ensure that merging the reflijst is going well
assertthat::assert_that(sum(!unique(dt_mszprest_OZP$vektmszdeclaratiecode) %in% unique(reflijst_zorgactiviteiten$mszzorgactiviteit)) <= 1)

dt_mszprest_OZP <- merge(
  dt_mszprest_OZP,
  reflijst_zorgactiviteiten,
  all.x=T,
  by.x = "vektmszdeclaratiecode",
  by.y = "mszzorgactiviteit"
)

# Make dummy cols based on underlying activiteiten
dt_mszprest_OZP <- dt_mszprest_OZP[, .(
  oper_verr = max(oper_verr, na.rm=T),
  ovg_ther_ver = max(ovg_ther_ver, na.rm=T),
  ovg_zpk = max(ovg_zpk, na.rm=T)),
  by = .(rinpersoon, vektmszbegindatumprest, vektmszdbczorgproduct, vektmszdeclaratiecode, vektmszvergoedbedragzvw, vektmszsettingzpk, vektmszinstellingprest, zpkcode)
]

# convert the three dummy cols to a single "zpk_category" col
dt_mszprest_OZP[, zpk_category := fcase(
  oper_verr == 1, "oper_verr",
  ovg_ther_ver == 1 & oper_verr != 1, "ovg_ther_ver",
  (ovg_ther_ver != 1 & oper_verr != 1) | is.na(zpkcode), "ovg_zpk"
)][, c("oper_verr", "ovg_ther_ver", "ovg_zpk", "zpkcode") := NULL]

## Then, bind the two separate tables together
dt_mszprest <- rbindlist(list(dt_mszprest_OZP, dt_mszprest_other), use.names=TRUE)
dt_mszprest[is.na(zpk_category), zpk_category := "ovg_zpk"]
rm(dt_mszprest_OZP, dt_mszprest_other)
gc()

# intermediate save, for quality checks later
# arrow::write_parquet(dt_mszprest, "data/processed/msz_zpk_activiteiten.parquet")

## Finally, perform the overlaps
dt_mszprest <- calculate_costs_by_bin_size(
  dt_mszprest,
  dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set, .SD, .SDcols = c("rinpersoon", "cohort", "gbadatumoverlijden", "died", "doodsoorzaak", "sample_id")],
  cost_columns = NULL,
  cost_date_col = "vektmszbegindatumprest",
  bin_size = "months33"
)

# intermediate save, for quality checks later
# arrow::write_parquet(dt_mszprest, "data/processed/msz_zpk_activiteiten_overlapped.parquet")


#### 1: create distribution per month ####
# create aggregations by category: 33 months
dt_mszprest_agg_zpkcat <- create_aggregations_by_category(
  dt_mszprest,
  groupby_cols_always = c("zpk_category", "t", "cohort", "died"),
  cols_to_create_all = c("vektmszsettingzpk")
)[order(cohort, died, vektmszsettingzpk, -t, zpk_category)]

dt_mszprest_agg_zpkcat <- dt_mszprest_agg_zpkcat[!is.na(zpk_category)]
dt_mszprest_agg_zpkcat <- dt_mszprest_agg_zpkcat[, bin_size := "monthly"]

gc()

dt_mszprest_agg_zpkcat_1000 <- create_aggregations_by_category(
  dt_mszprest,
  groupby_cols_always = c("zpk_category", "cohort", "died"),
  cols_to_create_all = c("vektmszsettingzpk")
)[order(cohort, died, vektmszsettingzpk, zpk_category)]

dt_mszprest_agg_zpkcat_1000 <- dt_mszprest_agg_zpkcat_1000[!is.na(zpk_category)]
dt_mszprest_agg_zpkcat_1000 <- dt_mszprest_agg_zpkcat_1000[, bin_size := "1000d"]
dt_mszprest_agg_zpkcat_1000 <- dt_mszprest_agg_zpkcat_1000[, t := -1]
gc()

dt_mszprest_agg_final <- rbindlist(list(dt_mszprest_agg_zpkcat, dt_mszprest_agg_zpkcat_1000), use.names=T)
invalid_counts <- dt_mszprest_agg_final[, .(
  n_invalid = sum(n_instellingen < 3 | share_main_instelling > 0.5, na.rm=T),
  n_t = uniqueN(t)
  ), 
  by = .(died, cohort, zpk_category, vektmszsettingzpk, bin_size)]

# openxlsx2::write_xlsx(list(
#   "figures" = dt_mszprest_agg_final,
#   "invalid_counts" = invalid_counts
# ), "data/processed/msz_prestaties_agg_zpkcategories.xlsx")

#### 2: create top 20 tables ####
# first, create aggregations
dt_mszprest_agg_1000 <- create_aggregations_by_category(
  dt_mszprest,
  groupby_cols_always = c("cohort", "died", "vektmszdeclaratiecode", "zpk_category"),
  cols_to_create_all = NULL
)[order(cohort, died)][!is.na(vektmszdeclaratiecode)][,bin_size := "1000d"]

dt_mszprest_agg_30 <- create_aggregations_by_category(
  dt_mszprest[t == -1],
  groupby_cols_always = c("cohort", "died", "vektmszdeclaratiecode", "zpk_category"),
  cols_to_create_all = NULL
)[order(cohort, died)][!is.na(vektmszdeclaratiecode)][,bin_size := "30d"]

dt_mszprest_agg <- rbindlist(list(dt_mszprest_agg_1000, dt_mszprest_agg_30))
dt_mszprest_agg[died == "In leven", died := "In_leven"]
rm(dt_mszprest_agg_30, dt_mszprest_agg_1000)
gc()

ranking_variables <- c("n_totaal_gebruikers", "sum_totaal_groep")
indicator_variables <- c("n_totaal_gebruikers", "sum_totaal_groep", "median_cost_per_declaratie", "n_instellingen", "share_main_instelling")

dt_results <- rbindlist(lapply(split(dt_mszprest_agg, by = c("zpk_category", "bin_size", "cohort", "died")), function(dt) {
  
  dt[died == "In leven", died := "In_leven"]
  
  # browser()
  # get split metadata
  split_zpk_category <- dt[1]$zpk_category
  split_bin_size <- dt[1]$bin_size
  split_cohort <- dt[1]$cohort
  split_died <- dt[1]$died

  dt_ranked_by_variable <- rbindlist(lapply(ranking_variables, function(ranking_var) {
    # browser()
    dt_ranked <- copy(dt)
    dt_ranked <- dt_ranked[, ':='(
      ranking = frank(-get(ranking_var)),
      ranked_by = paste(split_zpk_category, split_bin_size, split_cohort, split_died, ranking_var, sep = "_")
    )][ranking <= 50]
    
    vars_to_compare <- list(
      "bin_size" = c("30d", "1000d"), 
      "died" = c("Overleden", "In_leven")
      )
    
    for (comparison_var in names(vars_to_compare)) {
      split_comparison_var_value <- dt[, .SD, .SDcols = comparison_var][1]
      opposite_split_comparison_var_value <- setdiff(vars_to_compare[[comparison_var]], split_comparison_var_value)
      
      
      dt_temp <- copy(dt_mszprest_agg)
      
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
        dt_temp[, .SD, .SDcols = c("vektmszdeclaratiecode", opposite_col_names)], 
        by = "vektmszdeclaratiecode",
        all.x=T
      )
      
      # browser()
    }
    
    
    # add the activiteit desc
    reflijst_zorgactiviteiten <- rio::import("K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods")
    reflijst_zorgactiviteiten <- format_data(reflijst_zorgactiviteiten, rin_num = F)[, .SD, .SDcols = c(
      "mszzorgactiviteit", "mszzorgactiviteitomschrijving"
    )]
    reflijst_zorgactiviteiten[, mszzorgactiviteit := as.character(mszzorgactiviteit)]
    reflijst_zorgactiviteiten[nchar(mszzorgactiviteit) == 5, mszzorgactiviteit := paste0("0", mszzorgactiviteit)]
    
    
    dt_ranked <- merge(
      dt_ranked,
      reflijst_zorgactiviteiten,
      by.x = "vektmszdeclaratiecode",
      by.y = "mszzorgactiviteit",
      all.x=T
    )[order(ranking)]
    
  }))
  
  return(dt_ranked_by_variable)
}), fill=T)

# determine how many rows are invalid by ranked_by
invalid_counts <- dt_results[, .(
  invalid_ranked_col = sum(n_instellingen < 3 | share_main_instelling > 0.5, na.rm=T),
  invalid_comparison_30d = sum(n_instellingen_30d < 3 | share_main_instelling_30d > 0.5, na.rm=T),
  invalid_comparison_1000d = sum(n_instellingen_1000d < 3 | share_main_instelling_1000d > 0.5, na.rm=T),
  invalid_comparison_Overleden = sum(n_instellingen_Overleden < 3 | share_main_instelling_Overleden > 0.5, na.rm=T),
  invalid_comparison_In_leven = sum(n_instellingen_In_leven < 3 | share_main_instelling_In_leven > 0.5, na.rm=T)
), by="ranked_by"]

openxlsx2::write_xlsx(list(
  "figures" = dt_results,
  "invalid_counts" = invalid_counts
  ), "data/processed/top20_codes_by_category.xlsx")



