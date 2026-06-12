# REVIEW

#### marco ####
## 00_inputs.R
# 221: calculate_costs_by_bin_size()
# Consider only keeping the columns you supply as cost_date_col (with other used columns)
# and dropping columns you do not want to keep

# 239: costs_dt_clean <- copy(costs_dt)
# Consider dropping that line if the original file is not used

# 240: overlijden_dt_clean <- copy(overlijden_dt)
# Consider dropping that line if the original file is not used

# 03b_clean_continuous_healthcosts
# 76: by = .(rinpersoon, gbadatumoverlijden, cohort, died, t, bin_start, bin_end),
# If you do not use bin_start and bin_end later, consider dropping it 
# to reduce memory load

# 98: "vektmszvergoedbedragav"
# Consider dropping vektmszvergoedbedragav 

# 122: dt_mszprest_chunk[vektmszvergoedbedragav == "M", vektmszvergoedbedragav := NA]
# Consider just setting vektmszvergoedbedragav to numeric

# 129: cost_columns = c("vektmszvergoedbedragzvw", "vektmszvergoedbedragav"),
# # Consider dropping vektmszvergoedbedragav 

#### Stas ####

## 01_clean_demographic_characteristics.R
# 34 - in 9096 we have scripts/datasets in the data folder called DEMOG,
# which contain parquet files with variables like herkomst already added. We don't
# have these files yet in 9097 but could be worth it to reduce compute time or shorten scripts

# 183 - 292: I believe all of these loads from stapeling can be done with the new
# read_demog_stapeling function, consider changing to make less verbose

# 378: same comment as in line 34; the demog dataset already contains seswoa so could be useful

## 03a_clean_annual_healthscosts.R
# 124: "Drop people with no observed costs" - are we sure we want to do this?

# lines 131-135; so currently we take all data for all years that are needed for 
# 1000 days, and then prorate them according to actual n of needed days. I think 
# this works fine, but probably it's more accurate if we only prorate the costs in 
# the specific years of which we need to take proportions? E.g., for someone who 
# died in 2023, we take proportions of the years 2020 and 2023, but take all costs 
# of 2021-2022. I don't believe this is currently how it's done but I could be reading
# the code incorrectly

