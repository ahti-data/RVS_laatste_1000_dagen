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

#### analyse 1: Classification of prestaties into categories ####
dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")[died == "Overleden" & cohort == "2019"]
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)

test_codes <- c(
  "038913",
  "030898",
  "070702",
  "190015",
  "039680"
)

dt_mszprest <- rbindlist(lapply(years, function(yr){
  gc()
  
  dt_mszprest_yr <- load_dataset(
    yr,
    "mszprestatiesvekttab",
    cols = c("vektmszbegindatumprest", "rinpersoon", "vektmszkoppelidprestza","vektmszvergoedbedragzvw", "vektmszdeclaratiecode", "vektmszsettingzpk"),
    rinpersoon_chunk = rinpersoon_set
  )
  
  dt_mszprest_yr_test <- load_dataset_old(
    yr,
    "mszprestatiesvekttab",
    cols = c("vektmszbegindatumprest", "rinpersoon", "vektmszkoppelidprestza","vektmszvergoedbedragzvw", "vektmszdeclaratiecode", "vektmszsettingzpk"),
    rinpersoon_chunk = rinpersoon_set
  )
  
  assertthat::are_equal(dt_mszprest_yr, dt_mszprest_yr_test)
  rm(dt_mszprest_yr_test)

  
  dt_mszprest_OZP_yr <- dt_mszprest_yr[!substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")]
  dt_mszprest_other_yr <- dt_mszprest_yr[substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")]
  
  # rm(dt_mszprest_yr)
  gc()
  
  dt_mszprest_OZP_yr <- dt_mszprest_OZP_yr[vektmszdeclaratiecode %in% test_codes]
  
  dt_mszact <- load_dataset(
    yr,
    "MSZZORGACTIVITEITENVEKTTAB",
    cols = c("rinpersoon", "vektmszkoppelidprestza", "vektmszzorgactiviteit"),
    rinpersoon_chunk = unique(dt_mszprest_other_yr$rinpersoon)
  )[vektmszzorgactiviteit %in% test_codes]
  
  dt_mszprest_other_yr <- merge(
    dt_mszact,
    dt_mszprest_other_yr,
    by = c("vektmszkoppelidprestza", "rinpersoon"),
    all.x=T
  )
  
  dt_mszprest_other_yr[, vektmszdeclaratiecode := vektmszzorgactiviteit][, vektmszzorgactiviteit := NULL]
  
  return(rbindlist(list(dt_mszprest_other_yr, dt_mszprest_OZP_yr), use.names=T))
}))


# overlap
dt_mszprest_overlapped <- calculate_costs_by_bin_size(
  dt_mszprest[!is.na(vektmszbegindatumprest)],
  dt_overlijden_with_matched[as.numeric(rinpersoon) %in% unique(dt_mszprest$rinpersoon), 
                             .SD, .SDcols = c("rinpersoon", "cohort", "gbadatumoverlijden", "died", "doodsoorzaak", "sample_id")],
  cost_columns = NULL,
  cost_date_col = "vektmszbegindatumprest",
  bin_size = "months33"
)

# save
arrow::write_parquet(dt_mszprest_overlapped, "data/raw/temp_checks_top50.parquet")

for (code in test_codes) {
  count <- uniqueN(dt_mszprest_overlapped[vektmszdeclaratiecode == code & died == "Overleden" & cohort == "2019" & t == "-1" & vektmszvergoedbedragzvw > 0]$sample_id)
  print(glue("n_totaal_gebruikers 1000 dagen for cohort 2019 & code {code}: {count}"))
  
  count_decl <- nrow(dt_mszprest_overlapped[vektmszdeclaratiecode == code & died == "Overleden" & cohort == "2019" & t == "-1" & vektmszvergoedbedragzvw > 0])
  print(glue("n_totaal_declaraties 1000 dagen for cohort 2019 & code {code}: {count_decl}"))
}
