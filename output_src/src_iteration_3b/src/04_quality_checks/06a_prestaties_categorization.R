#### TODO ####

#### Initialize ####
rm(list=ls())
gc()

source("src/00_inputs.R")

#### top 50 codes ####
dt_mszprest_top50 <- as.data.table(openxlsx2::read_xlsx("output/iteration_3a/GEENOUTPUT_Achtergrondinformatie/top50_activiteiten.xlsx"))

# take sample cohort: overleden/2019
dt_overlijden_with_matched_subset <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")[cohort == "2019"& died == "Overleden"]
rinpersoon_set_subset <- unique(dt_overlijden_with_matched_subset$rinpersoon)

dt_mszprest_top50_subset <- dt_mszprest_top50[cohort == "2019"& died == "Overleden"]

# test population size
assertthat::assert_that(abs(uniqueN(dt_overlijden_with_matched_subset$sample_id) - unique(dt_mszprest_top50_subset$n_totaal_population)) <= 5)

# get intermediate datasets, and verify correct calculations
msz_zpk_activiteiten <- r_parquet_get_dt("data/processed/msz_zpk_activiteiten.parquet")
msz_zpk_activiteiten_overlapped <- r_parquet_get_dt("data/processed/msz_zpk_activiteiten_overlapped.parquet")
msz_zpk_activiteiten_subset <- msz_zpk_activiteiten[rinpersoon %in% rinpersoon_set_subset]
msz_zpk_activiteiten_overlapped_subset <- msz_zpk_activiteiten_overlapped[rinpersoon %in% rinpersoon_set_subset]

split_by_code <- split(msz_zpk_activiteiten_overlapped_subset[cohort == "2019" & t == -1], by = "vektmszdeclaratiecode")

for (dt_split in split_by_code) {
  code <- dt_split$vektmszdeclaratiecode[1]
  dt_split <- dt_split[died == "Overleden"]
  dt_split_comparison <- dt_split[died == "In_leven"]
  dt_mszprest_top50_subset_filter <- dt_mszprest_top50_subset[bin_size == "30d" & ranked_by == "ovg_zpk_30d_2019_Overleden_n_totaal_gebruikers"]
  
  if(!is.na(code) & code %in% unique(dt_mszprest_top50_subset_filter$vektmszdeclaratiecode)){
    # browser()
    # verify n_totaal_gebruikers 
    count_output <- dt_mszprest_top50_subset_filter[as.character(vektmszdeclaratiecode) == code]$n_totaal_gebruikers 
    count_manual <- uniqueN(dt_split[vektmszdeclaratiecode == code]$sample_id)
    print(glue("count output: {count_output} count manual: {count_manual}"))
    assertthat::assert_that(abs(count_output - count_manual) <= 10, msg = glue("count output: {count_output} count manual: {count_manual}"))
    
    # verify sum costs
    costs_output <- dt_mszprest_top50_subset_filter[as.character(vektmszdeclaratiecode) == code]$sum_totaal_groep
    count_manual <- sum(as.numeric(dt_split[vektmszdeclaratiecode == code]$vektmszvergoedbedragzvw), na.rm=T)
    print(glue("costs output: {costs_output} costs manual: {costs_output}"))
    assertthat::assert_that(abs(costs_output - count_manual) <= 10)
    
    # verify median costs
    median_output <- dt_mszprest_top50_subset_filter[as.character(vektmszdeclaratiecode) == code]$median_cost_per_declaratie
    median_manual <- median(as.numeric(dt_split[vektmszdeclaratiecode == code]$vektmszvergoedbedragzvw), na.rm=T)
    print(glue("median output: {median_output} costs manual: {median_manual}"))
    assertthat::assert_that(abs(median_output - median_manual) <= 10)
    
    # verify n_instellingen
    n_instellingen_output <- dt_mszprest_top50_subset_filter[as.character(vektmszdeclaratiecode) == code]$n_instellingen 
    n_instellingen_manual <- uniqueN(dt_split[vektmszdeclaratiecode == code]$vektmszinstellingprest)
    print(glue("n_instellingen output: {n_instellingen_output} n_instellingen manual: {n_instellingen_manual}"))
    assertthat::assert_that(abs(n_instellingen_output - n_instellingen_manual) <= 10, msg = glue("n_instellingen output: {n_instellingen_output} n_instellingen manual: {n_instellingen_manual}"))

  }
}

#### aggregations ####


dt_mszprest_agg <- as.data.table(openxlsx2::read_xlsx("output/iteration_3a/GEENOUTPUT_Achtergrondinformatie/zpk_categorieen_tellingen.xlsx"))

# take sample cohort: In_leven/2023
dt_overlijden_with_matched_subset <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")[cohort == "2023" & died == "In leven"]
rinpersoon_set_subset <- unique(dt_overlijden_with_matched_subset$rinpersoon)

dt_mszprest_agg_subset <- dt_mszprest_agg[cohort == "2023" & died == "In leven"]

# test population size
assertthat::assert_that(abs(uniqueN(dt_overlijden_with_matched_subset$sample_id) - unique(dt_mszprest_agg_subset$n_totaal_population)) <= 5)

# test whether groups add up
dt_mszprest_agg_subset_split <- split(dt_mszprest_agg_subset, by = c("cohort", "died", "zpk_category", "bin_size"))

for (dt_split in dt_mszprest_agg_subset_split) {
  if("9" %in% unique(dt_split$vektmszsettingzpk)) {

    assertthat::assert_that(abs(
      sum(dt_split[vektmszsettingzpk != "all"]$n_totaal_declaraties) - sum(dt_split[vektmszsettingzpk == "all"]$n_totaal_declaraties)
    ) <= 10)
  }
}

# check quickly with overlapped dt
# take sample cohort: overleden/2019
dt_overlijden_with_matched_subset <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")[cohort == "2023"& died == "Overleden"]
rinpersoon_set_subset <- unique(dt_overlijden_with_matched_subset$rinpersoon)

# test population size
assertthat::assert_that(abs(uniqueN(dt_overlijden_with_matched_subset$sample_id) - unique(dt_mszprest_top50_subset$n_totaal_population)) <= 5)

# get intermediate datasets, and verify correct calculations
msz_zpk_activiteiten <- r_parquet_get_dt("data/processed/msz_zpk_activiteiten.parquet")
msz_zpk_activiteiten_overlapped <- r_parquet_get_dt("data/processed/msz_zpk_activiteiten_overlapped.parquet")
msz_zpk_activiteiten_subset <- msz_zpk_activiteiten[rinpersoon %in% rinpersoon_set_subset]
msz_zpk_activiteiten_overlapped_subset <- msz_zpk_activiteiten_overlapped[rinpersoon %in% rinpersoon_set_subset]

dt_mszprest_agg_subset <- dt_mszprest_agg[cohort == "2023" & died == "Overleden"]

assertthat::assert_that(abs(
  dt_mszprest_agg_subset[died=="Overleden" & cohort == "2023" & zpk_category == "ovg_ther_ver" & bin_size == "1000d" & vektmszsettingzpk == "all"]$n_totaal_gebruikers - 
    uniqueN(msz_zpk_activiteiten_overlapped_subset[died=="Overleden" & cohort == "2023" & zpk_category == "ovg_ther_ver"]$sample_id)
) <= 10)


