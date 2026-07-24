rm(list=ls())
gc()

source("src/00_inputs.R")

#### TODO ####

#### create table ####

# load sample
dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog_add_huisarts.parquet")


# filter for overleden
dt_regression <- dt_overlijden_with_matched[died == "Overleden"]

# start adding costs to the table
costs_to_add <- list(
  "msz_addon_cancer"= list(
    "filename" = "msz_addon_oncology_total_cancer_monthly.parquet",
    cost_cols = c("n_13_01_88", "bedrag_13_01_88")
  ),
  "msz" = list(
    "filename" = "vektmszkosten_monthly.parquet",
    cost_cols = c("vektmszvergoedbedragzvw")
    ),
  "wlz" = list(
    "filename" = "wlzkosten_monthly.parquet",
    cost_cols = c("bedragwlzzin")
    ),
  "zvw" = list(
    "filename" = "zvw_1000_dagen.parquet",
    cost_cols = c("zvwkfarmacie", "zvwkggzzpmtotaal", "zvwkhuisarts", "zvwkhulpmiddel", 
                  "zvwktotaal", "zvwkwykverpleging", "zvwkziekenhuis", "nopzvwkhuisartsconsult",
                  "nopzvwkhuisartsinschrijf", "nopzvwkhuisartsoverig")
  )
)

for (cost_type in names(costs_to_add)) {
  print(glue("currently adding {cost_type}"))
  
  # extract configs
  cost_config <- costs_to_add[[cost_type]]
  cost_filename <- cost_config[["filename"]]
  cost_cols <- cost_config[["cost_cols"]]
  
  cost_dir <- "data/processed/"

  # load dataset
  dt_costs <- r_parquet_get_dt(paste0(cost_dir, cost_filename))
  
  if (cost_type == "msz") dt_costs[, c("n_hartkleppen_totaal", "n_hartkleppen_transkatheter", "n_hartkleppen_klepchirurgie") := NULL]

  # add new huisarts consult col
  if (!"huisarts_consults_cat" %in% names(dt_costs)){
    dt_costs <- merge_with_validate(
      dt_costs,
      dt_overlijden_with_matched[, .SD, .SDcols = c("sample_id", "huisarts_consults_cat")],
      by = "sample_id",
      all.x=T,
      validate = "many_to_one",
      require_match = "left"
    )
  }
  
  # filter for overleden
  dt_costs <- dt_costs[died == "Overleden"]
  dt_costs_raw <- dt_costs # for development
  
  # create 30day and 1000day versions of costs, and merge
  if ("t" %in% names(dt_costs)) {
    non_cost_cols <- setdiff(names(dt_costs), cost_cols)
    
    dt_costs_30 <- dt_costs[t == -1][, t := NULL]
    setnames(dt_costs_30, cost_cols, paste0(cost_cols, "_30d"))
    
    # for msz_addon, we only want the 24month versions
    if (cost_type == "msz_addon_cancer") dt_costs <- dt_costs[t >= -24]
    
    dt_costs_1000 <- dt_costs[, lapply(.SD, sum, na.rm=T), 
                              by = setdiff(names(dt_costs), c(cost_cols, "t")), 
                              .SDcols = cost_cols][, t := NULL]
    
    # for msz_addon costs, this is 2years instead of 1000days
    suffix <- ifelse(cost_type == "msz_addon_cancer", "_2years", "_1000d")
    setnames(dt_costs_1000, cost_cols, paste0(cost_cols, suffix))
    
    
    dt_costs <- merge(
      dt_costs_30, 
      dt_costs_1000,
      by = setdiff(non_cost_cols, "t")
    )
  } else {
    setnames(dt_costs, cost_cols, paste0(cost_cols, "_1000d"))
  }

  if (cost_type != "msz_addon_cancer") assert_that(nrow(dt_regression) == nrow(dt_costs))
  
  dt_regression <- merge(
    dt_regression, 
    dt_costs, 
    by = names(dt_overlijden_with_matched),
    all.x=T
  )
  
  print(names(dt_regression))
}

# drop unnecessary cols
dt_regression[, c("rinpersoon", "gbadatumoverlijden", "died", "sample_id") := NULL]

# save
arrow::write_parquet(dt_regression, "data/processed/regression_table.parquet")
