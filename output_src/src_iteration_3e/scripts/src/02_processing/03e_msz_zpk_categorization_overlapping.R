#### initialize ####

rm(list=ls())
gc()

source("src/00_inputs.R")
development_mode <- F

#### TODO ####


#### functions ####

load_categorized_reflijst_zpk <- function() {
  reflijst_zorgactiviteiten <- load_reflijst_activiteiten(c("mszzorgactiviteit", "zpkcode"))
  
  reflijst_zorgactiviteiten[, ':='(
    oper_verr = as.integer(zpkcode == 5),
    ovg_ther_ver = as.integer(zpkcode == 6),
    ovg_zpk = as.integer(!zpkcode %in% c(5, 6))
  )][, zpkcode := NULL]
}

#### 1: Load and split prestaties into OZP vs DBC ####
dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)
assert_that(is.numeric(rinpersoon_set))
if (development_mode) rinpersoon_set <- sample(rinpersoon_set, 5000)

# load and merge prestaties
dt_mszprest <- load_dataset(
  years,
  "mszprestatiesvekttab",
  cols = c("rinpersoon", "vektmszkoppelidprestza", "vektmszbegindatumprest", 
           "vektmszvergoedbedragzvw", "vektmszdeclaratiecode", "vektmszsettingzpk", 
           "vektmszdbczorgproduct", "vektmszbeginjaarprest", "vektmszinstellingprest",
           "vektmszspecialismediagnosecombinatie"),
  rinpersoon_chunk = rinpersoon_set
)
if (development_mode) dt_mszprest_raw <- copy(dt_mszprest)

dt_mszprest[, id_prestatie := .I] # make an ID column for ease

# Process OZP and other prestaties separately, bind together later
dt_mszprest_OZP <- dt_mszprest[!substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")]
dt_mszprest_OZP[, vektmszdeclaratiecode := as.numeric(vektmszdeclaratiecode)] # for potential leading whitespace issues

dt_mszprest_DBC <- dt_mszprest[substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")]

n_prestaties_total <- nrow(dt_mszprest)
n_prestaties_DBC <- nrow(dt_mszprest_DBC)
n_prestaties_OZP <- nrow(dt_mszprest_OZP)

print(glue("n prestaties total sample: {n_prestaties_total}"))
print(glue("n prestaties OZP: {n_prestaties_OZP}"))
print(glue("n prestaties DBC: {n_prestaties_DBC}"))

rm(dt_mszprest)
gc()

#### 2: categorize & overlap DBCs ####
# load activiteiten
dt_mszact <- load_dataset(
  years,
  "MSZZORGACTIVITEITENVEKTTAB",
  cols = c("rinpersoon", "vektmszkoppelidprestza", "vektmszzorgactiviteit", "vektmszbeginjaarprest", "vektmszzorgactiviteitdatum", "vektmszinstellingza"),
  rinpersoon_chunk = rinpersoon_set
)[, vektmszzorgactiviteit := as.numeric(vektmszzorgactiviteit)]
if (development_mode) dt_mszact_raw <- copy(dt_mszact)

# load and clean reflijst
reflijst_zorgactiviteiten <- load_categorized_reflijst_zpk()

# small guard clause to ensure that merging the reflijst is going well
assertthat::assert_that(sum(!unique(dt_mszact$vektmszzorgactiviteit) %in% unique(reflijst_zorgactiviteiten$mszzorgactiviteit)) <= 1)

# REVIEW: why do you need to write this function merge_with_validate() and not simply use 
# merge()? The function is too hard to follow
dt_mszact <- merge_with_validate(
  dt_mszact,
  reflijst_zorgactiviteiten,
  all.x=T,
  by.x = "vektmszzorgactiviteit",
  by.y = "mszzorgactiviteit",
  validate = "many_to_one"
)

# handle NA activiteiten, put in overig category
dt_mszact[!vektmszzorgactiviteit %in% unique(reflijst_zorgactiviteiten$mszzorgactiviteit), 
          ':=' (ovg_zpk = 1, ovg_ther_ver = 0, oper_verr = 0)]


# overlap and write, for creating top50codes later
dt_mszact_overlapped <- calculate_costs_by_bin_size(
  dt_mszact,
  dt_overlijden_with_matched[, .SD, .SDcols = c("rinpersoon", "cohort", "gbadatumoverlijden", "died", "doodsoorzaak", "sample_id")],
  cost_columns = NULL,
  cost_date_col = "vektmszzorgactiviteitdatum",
  bin_size = "months33"
)

# categorize
dt_mszact_overlapped[, zpk_category := fcase(
  oper_verr == 1, "oper_verr",
  ovg_ther_ver == 1 & oper_verr != 1, "ovg_ther_ver",
  (ovg_ther_ver != 1 & oper_verr != 1), "ovg_zpk"
)][, c("oper_verr", "ovg_ther_ver", "ovg_zpk") := NULL]

# Drop NA rows (people without activiteiten)
dt_mszact_overlapped <- dt_mszact_overlapped[!is.na(vektmszzorgactiviteit)]

# for places where vektmszinstellingza == "99999999", the instelling is equal to the prestaties instelling. Merge these
dt_mszprest <- load_dataset(
  years,
  "mszprestatiesvekttab",
  cols = c("rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest", "vektmszinstellingprest"),
  rinpersoon_chunk = rinpersoon_set
)

dt_mszact_overlapped <- merge_with_validate(
  dt_mszact_overlapped,
  dt_mszprest,
  by = c("rinpersoon", "vektmszkoppelidprestza", "vektmszbeginjaarprest"),
  all.x=T,
  validate = "many_to_one"
)
rm(dt_mszprest)
gc()

dt_mszact_overlapped[vektmszinstellingza == "99999999", vektmszinstellingza := vektmszinstellingprest]
dt_mszact_overlapped[, vektmszinstellingprest := NULL]

arrow::write_parquet(dt_mszact_overlapped, "data/processed/mszact_categorized_overlapped.parquet")
rm(dt_mszact_overlapped)
gc()

# merge non-OZP prestaties and activiteiten
dt_mszact[, vektmszzorgactiviteit := NULL]

dt_mszprest_DBC <- merge_with_validate(
  dt_mszprest_DBC,
  dt_mszact,
  by = c("vektmszkoppelidprestza", "rinpersoon", "vektmszbeginjaarprest"),
  all.x=T,
  validate = "one_to_many"
)
are_equal(nrow(dt_mszprest_DBC), nrow(dt_mszact))

# REVIEW: max() is slow, you can use any(oper_verr == 1) instead
# for DBCs, we reduce the rows back to the n prestaties OZPs, and derive category based on ranking:
dt_mszprest_DBC <- dt_mszprest_DBC[, .(
  oper_verr = max(oper_verr, na.rm=T),
  ovg_ther_ver = max(ovg_ther_ver, na.rm=T),
  ovg_zpk = max(ovg_zpk, na.rm=T)),
  by = .(rinpersoon, vektmszbegindatumprest, vektmszdbczorgproduct, vektmszvergoedbedragzvw, 
         vektmszsettingzpk, vektmszinstellingprest, id_prestatie, vektmszspecialismediagnosecombinatie)
]

# validate we now have rows that equal prestaties now
assert_that(nrow(dt_mszprest_DBC) == n_prestaties_DBC)

# convert the three dummy cols to a single "zpk_category" col based on ranking
dt_mszprest_DBC[, zpk_category := fcase(
  oper_verr == 1, "oper_verr",
  ovg_ther_ver == 1 & oper_verr != 1, "ovg_ther_ver",
  (ovg_ther_ver != 1 & oper_verr != 1), "ovg_zpk"
)][, c("oper_verr", "ovg_ther_ver", "ovg_zpk") := NULL]

# assign dbc's without activiteiten to zpk_overig
dt_mszprest_DBC[is.na(zpk_category), ':='(
  zpk_category = "ovg_zpk"
)]

# print dist by categories for check
dt_mszprest_DBC[, .(.N), by = "zpk_category"]

# finally, overlap with last 1000 days
dt_mszprest_DBC <- calculate_costs_by_bin_size(
  dt_mszprest_DBC,
  dt_overlijden_with_matched[, .SD, .SDcols = c("rinpersoon", "cohort", "gbadatumoverlijden", "died", "doodsoorzaak", "sample_id")],
  cost_columns = NULL,
  cost_date_col = "vektmszbegindatumprest",
  bin_size = "months33"
)

# Drop NA rows (people who had no matched prestaties)
dt_mszprest_DBC <- dt_mszprest_DBC[!is.na(zpk_category)]

# Write
arrow::write_parquet(dt_mszprest_DBC, "data/processed/mszprest_DBC_overlapped.parquet")

rm(dt_mszprest_DBC, dt_mszact)
gc()

#### 3: categorize & overlap OZPs ####
dt_mszprest_OZP <- merge_with_validate(
  dt_mszprest_OZP,
  reflijst_zorgactiviteiten,
  all.x=T,
  by.x = "vektmszdeclaratiecode",
  by.y = "mszzorgactiviteit",
  validate = "many_to_one"
)

# handle NA/9999 declaratiecodes by assigning them to "overig zpk", then remove the declaratiecode column
dt_mszprest_OZP[!vektmszdeclaratiecode %in% unique(reflijst_zorgactiviteiten$mszzorgactiviteit), ':=' (ovg_zpk = 1, ovg_ther_ver = 0, oper_verr = 0)]
dt_mszprest_OZP[, vektmszdeclaratiecode := NULL]

# convert the three dummy cols back to one zpk_category col
dt_mszprest_OZP[, zpk_category := fcase(
  oper_verr == 1, "oper_verr",
  ovg_ther_ver == 1 & oper_verr != 1, "ovg_ther_ver",
  (ovg_ther_ver != 1 & oper_verr != 1), "ovg_zpk"
)][, c("oper_verr", "ovg_ther_ver", "ovg_zpk") := NULL]

# print dist by categories
dt_mszprest_OZP[, .(.N), by = "zpk_category"]

# finally, overlap with last 1000 days
dt_mszprest_OZP <- calculate_costs_by_bin_size(
  dt_mszprest_OZP,
  dt_overlijden_with_matched[, .SD, .SDcols = c("rinpersoon", "cohort", "gbadatumoverlijden", "died", "doodsoorzaak", "sample_id")],
  cost_columns = NULL,
  cost_date_col = "vektmszbegindatumprest",
  bin_size = "months33"
)

# Drop NA rows (people who had no matched prestaties)
dt_mszprest_OZP <- dt_mszprest_OZP[!is.na(zpk_category)]

arrow::write_parquet(dt_mszprest_OZP, "data/processed/mszprest_OZP_overlapped.parquet")
