# Project: Laatste 1000 dagen
# Author:  Marco Griep
# Goal: Create descriptives for huisarts data
# Output: Microdata

#### initialize ####
rm(list = ls())
gc()

source("src/00_inputs.R")
dt_overlijden_with_matched <- r_parquet_get_dt("data/raw/overlijden_with_matched_add_demog.parquet")

#### population descriptives of people with ACP ####
dt_desc_ACP <- rbindlist(lapply(
  setdiff(
    names(dt_overlijden_with_matched), 
    c("rinpersoon", "sample_id", "gbadatumoverlijden", "used_any_acp_2years", "died")
    ),
  function(col) {
    dt_desc_ACP_col <- dt_overlijden_with_matched[, 
      .(
        n_users_acp_consults_2years = sum(used_any_acp_2years == 1, na.rm=T),
        n_population = fnunique(sample_id)
        ), 
      by = c(col, "died")
    ][, group := get(col)][, (col) := NULL][, split_by := col]
    
  }
))

# mask zeroes, and save the descriptives table:
dt_desc_ACP[, n_users_acp_consults_2years := round(n_users_acp_consults_2years, -1)]
dt_desc_ACP <- dt_desc_ACP[n_users_acp_consults_2years != 0]
openxlsx2::write_xlsx(dt_desc_ACP, glue("{output_folder}ACP_population_descriptives.xlsx"))


# # create barcharts
# splits <- unique(dt_desc_ACP$split_by)
# 
# for (s in splits) {
#   dt_sub <- dt_desc_ACP[split_by == s]
#   
#   p <- ggplot(dt_sub, aes(x = reorder(group, - perc_used_acp_2years), y = perc_used_acp_2years)) +
#     geom_col(fill="steelblue") +
#     geom_text(aes(label = n_users_acp_consults_2years), vjust = -0.5, size = 4) +
#     facet_wrap(~died, 1, 2) +
#     labs(
#       title = s,
#       x = "Group", 
#       y = "Percentage Used (%)"
#     ) + 
#     scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
#     theme_minimal() +
#     theme(
#       plot.title = element_text(face = "bold", size = 14),
#       axis.text.x = element_text(angle = 45, hjust = 1)
#     )
#   ggsave(
#     glue("data/processed/plots_ACP/{s}.png")
#   )
# }

rm(dt_desc_ACP)
gc()

# get average age, by temporarily adding age to the overlijden_with_matched
dt_gbapersoon <- r_parquet_get_dt("data/raw/gbapersoon.parquet")
assert_that(is.numeric(dt_gbapersoon$rinpersoon))
dt_gbapersoon <- dt_gbapersoon[rinpersoon %in% unique(dt_overlijden_with_matched$rinpersoon)]
dt_gbapersoon[, month_of_birth := as.character(month_of_birth)]
dt_gbapersoon[nchar(month_of_birth) == "1", month_of_birth := paste0("0", as.character(month_of_birth))]
dt_gbapersoon[, birthdate := as.Date(paste0(year_of_birth, month_of_birth, "01"), format = "%Y%m%d")]

dt_overlijden_with_matched <- merge_with_validate(
  dt_overlijden_with_matched,
  dt_gbapersoon[, .SD, .SDcols = c("rinpersoon", "birthdate")],
  by = "rinpersoon",
  all.x=T,
  validate = "many_to_one", 
  require_match = "left"
)

dt_overlijden_with_matched[, age := as.numeric((gbadatumoverlijden - birthdate) / 365.25)]

average_age <- dt_overlijden_with_matched[used_any_acp_2years == 1, .(
  age_acp_user = mean(age),
  n_population = fnunique(sample_id)
), by = .(died, geslacht)]

dt_overlijden_with_matched[, c("birthdate", "age") := NULL]

# save for output
openxlsx2::write_xlsx(average_age, glue("{output_folder}acp_average_ages.xlsx"))

rm(dt_gbapersoon, average_age)
gc()

#### Create distribution for new regression var ####
dt_huisarts_monthly_agg <- r_parquet_get_dt("data/processed/huisartsdecl_monthly.parquet")

# aggregate for two years
dt_huisarts_twoyears_agg <- dt_huisarts_monthly_agg[, .(
  n_consults = sum(n_consults, na.rm=T),
  hadeclvergoedbedrag = sum(hadeclvergoedbedrag, na.rm=T)
), by = c(names(dt_overlijden_with_matched))]

#### create categories based on distribution n_consults ####
# define rins with usage
usage_rins <- dt_huisarts_twoyears_agg[n_consults > 0, unique(rinpersoon)]

# create distributions by t, plot
for (col in c("n_consults", "hadeclvergoedbedrag")) {
  dt_dist <- dt_huisarts_monthly_agg[, .(
    quantile_5 = quantile(get(col), 0.05, na.rm=T),
    quantile_25 = quantile(get(col), 0.25, na.rm=T),
    median = quantile(get(col), 0.5, na.rm=T),
    quantile_75 = quantile(get(col), 0.75, na.rm=T),
    quantile_95 = quantile(get(col), 0.95, na.rm=T)
  ), by = .(t, died)]
  
  # one for all
  p <- ggplot(dt_dist, aes(x = factor(t), fill = died)) +
    geom_boxplot(
      aes(
        ymin = quantile_5,
        lower = quantile_25,
        middle = median,
        upper = quantile_75,
        ymax = quantile_95
      ),
      stat="identity",
      width = 0.8
    ) + 
    scale_x_discrete(breaks = function(x) x[seq(0, length(x), by = 3)]) +
    labs(
      title = glue("Distribution over t for {col}, cohort 2023"),
      x = "Maand tot overlijden", y = NULL
    )
  
  ggsave(
    glue("data/processed/plots_ACP/dist_{col}.png")
  )
  
  # one for usage
}

# a single one for 24 months
for (col in c("n_consults", "hadeclvergoedbedrag")) {
  dt_dist <- dt_huisarts_twoyears_agg[, .(
    quantile_5 = quantile(get(col), 0.05, na.rm=T),
    quantile_25 = quantile(get(col), 0.25, na.rm=T),
    median = quantile(get(col), 0.5, na.rm=T),
    quantile_75 = quantile(get(col), 0.75, na.rm=T),
    quantile_95 = quantile(get(col), 0.95, na.rm=T)
  ), by = .(died)]

  # one for all
  p <- ggplot(dt_dist, aes(x = factor(died))) +
    geom_boxplot(
      aes(
        ymin = quantile_5,
        lower = quantile_25,
        middle = median,
        upper = quantile_75,
        ymax = quantile_95
      ),
      stat="identity",
      width = 0.8
    ) + 
    # scale_x_discrete(breaks = function(x) x[seq(0, length(x), by = 3)]) +
    labs(
      title = glue("Distribution for two years for {col}, cohort 2023"),
      x = "Maand tot overlijden", y = NULL
    )
  
  ggsave(
    glue("data/processed/plots_ACP/dist_{col}_2years.png")
  )
}

huisarts_consult_categories <- list(
  "no consults in past two years" = 0,
  "low (1-9 consults in past two years)" = 1:9,
  "moderate (10-19 consults in past two years)" = 10:19,
  "high (20-39 consults in past two years)" = 20:39,
  "very high (40+ consults in past two years)" = 40:99999
) # based on histograms

lookup <- as.data.table(stack(huisarts_consult_categories))
setnames(lookup, "ind", "huisarts_consults_cat")

# create col to merge into dt_overlijden_with_matched
dt_huisarts_twoyears_agg_cat <- merge_with_validate(
  dt_huisarts_twoyears_agg,
  lookup,
  by.x="n_consults",
  by.y = "values",
  all.x=T,
  validate = "many_to_one",
  require_match = "left"
)[, hadeclvergoedbedrag := NULL]

# merge back into overlijden
dt_overlijden_with_matched <- merge_with_validate(
  dt_overlijden_with_matched,
  dt_huisarts_twoyears_agg_cat[, .SD, .SDcols = c("sample_id", "huisarts_consults_cat")],
  by="sample_id",
  all.x=T,
  validate = "one_to_one",
  require_match = "right"
)
arrow::write_parquet(dt_overlijden_with_matched, "data/raw/overlijden_with_matched_add_demog_add_huisarts.parquet")


## NEW: add the category to the msz costs
if (!"huisarts_consults_cat" %in% names(arrow::open_dataset("data/processed/vektmszkosten_monthly.parquet"))) {
  dt_mszprest <- r_parquet_get_dt("data/processed/vektmszkosten_monthly.parquet")
  
  dt_mszprest <- merge_with_validate(
    dt_mszprest,
    dt_overlijden_with_matched[, .SD, .SDcols = c("sample_id", "huisarts_consults_cat")],
    by = "sample_id",
    all.x=T,
    validate = "many_to_one",
    require_match = "left"
  )
  
  arrow::write_parquet(dt_mszprest, "data/processed/vektmszkosten_monthly.parquet")
}




