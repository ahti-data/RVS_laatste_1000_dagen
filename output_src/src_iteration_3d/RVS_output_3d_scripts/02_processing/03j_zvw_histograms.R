# Project: Laatste 1000 dagen
# Author:  Marco Griep
# Goal: Create descriptives for huisarts data
# Output: Microdata

#### initialize ####
rm(list = ls())
gc()

### 

source("src/00_inputs.R")
dt_overlijden_with_matched <- r_parquet_get_dt("data/raw/overlijden_with_matched_add_demog.parquet")


#### calculate distribution ####
get_zvw_distribution <- function(costs_col, zvw_dt, groupby_var) {
  dt_sub <- zvw_dt[, .SD, .SDcols = c("rinpersoon", groupby_var, costs_col)]
  setnames(dt_sub, costs_col, "cost_value")
  
  positive_costs <- dt_sub[cost_value > 0.1, cost_value]
  x_cap <- as.numeric(quantile(positive_costs, 0.99))
  under_x_costs <- positive_costs[positive_costs <= x_cap]
  bin_breaks <- unique(c(-1, pretty(under_x_costs, n = 40), Inf))
  bin_breaks[2] <- 0.1
  
  # browser()
  
  bin_labels <- character(length(bin_breaks) - 1)
  # bin_labels[1] <- "0"
  
  for (i in 1:length(bin_labels)) {
    start_val <- format(bin_breaks[i], scientific = F, trim = T)
    end_val <- format(bin_breaks[i+1], scientific = F, trim = T)
    bin_labels[i] <- paste0(start_val, " - ", end_val)
  }
  dt_sub[, cost_bin := cut(cost_value, breaks = bin_breaks, labels = bin_labels, right = FALSE)]
  levels(dt_sub$cost_bin)[1] <- "0"
  
  dt_bins <- dt_sub[, .(
    n_population = .N
    ), by = c(groupby_var, "cost_bin")]
  dt_bins[, cost_type := costs_col]
  setorder(dt_bins, cost_bin)
  
  # add mean and median to rows
  dt_averages <- dt_sub[, .(
    mean_costs_all = mean(cost_value, na.rm=T),
    mean_costs_users = mean(cost_value[cost_value > 0], na.rm=T),
    median_costs_all = median(cost_value, na.rm=T),
    median_costs_users = median(cost_value[cost_value > 0], na.rm=T)
  ), by = groupby_var]
  
  dt_bins <- merge_with_validate(
    dt_bins, 
    dt_averages,
    validate = "many_to_one",
    require_match = "left"
  )
  

  # for all persons
  p <- ggplot(dt_bins, aes(x = cost_bin, y = n_population, fill = cohort)) +
    geom_col(position = "dodge") +
    labs(
      title = sprintf("Distribution of %s", costs_col), 
      x = "Cost Interval",
      y = "Frequency (Count)",
      fill = groupby_var
    ) + 
    facet_grid(cohort~died) + 
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  dir <- "data/processed/zvw_hist"
  dir.create(dir, recursive = T, showWarnings = F)
  ggsave(glue("{dir}/{costs_col}_hist.png"))
  
  # browser()
  
  return(dt_bins)
}

dt_zvw <- r_parquet_get_dt("data/processed/zvw_1000_dagen.parquet")

cost_cols <- c("zvwktotaal", "zvwkziekenhuis")
groupby_cols <- c("cohort", "died")

distributions <- rbindlist(lapply(cost_cols, function(cost_col) {
  return(get_zvw_distribution(cost_col, dt_zvw[died == "Overleden"], groupby_cols))
  }))


# save, already output-ready
openxlsx2::write_xlsx(distributions, glue("{output_folder}zvwk_distributions.xlsx"))
