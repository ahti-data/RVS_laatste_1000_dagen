# Project: Laatste 1000 dagen
# Authors: Stanislav Avdeev & Marco Griep
# Goal: Make plots of average costs and usage in the last 1000 days
# Output: Plots and aggregated data
# Last edited: 13 March 2026

# To do:
# Review the code
# Make internal and external checks

# By 17 March
# No split for September cohort
# Total and by the cause of death
# Save all outputs in Excel 

# Done:
# Change at least 5 ATC4 medications in a year 
# Create new DBC codes

# LATER:
# Update parquet files 

#### initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(ggplot2)
options(scipen = 999)

path <- glue("./output/totaal/")
dir.create(path, recursive = T, showWarnings = F)

#### plotting functions ####
aggregate_binned_costs <- function(plot_df, cost_columns, cost_columns_gebruikt) {
  
  # Make it per person
  df_binned_kosten <- plot_df[, lapply(.SD, sum, na.rm = T),
                              by = .(rinpersoon, gbadatumoverlijden, cohort, died),
                              .SDcols = cost_columns
  ]
  df_binned_gebruikt <- plot_df[, lapply(.SD, max, na.rm = T),
                                by = .(rinpersoon, gbadatumoverlijden, cohort, died),
                                .SDcols = cost_columns_gebruikt
  ]
  
  df <- merge(df_binned_kosten, df_binned_gebruikt,
              all = T,
              by = c("rinpersoon", "gbadatumoverlijden", "cohort", "died")
  )
  return(df)
}


plot_months_to_death <- function(agg_df, cost_columns, path) {
  # Reshape to wide
  agg_wide_df <- dcast(
    agg_df[type %in% c(
      "q05_per_persoon", "q25_per_persoon",
      "mediaan_per_persoon", "q75_per_persoon",
      "q95_per_persoon"
    )],
    cohort + died + t + name ~ type,
    value.var = "value"
  )

  for (cost_col_name in cost_columns) {
    print(glue("creating boxplot months to death for {cost_col_name}"))

    plot_title <- ifelse(
      grepl("hadeclvergoedbedrag", cost_col_name),
      glue("{cost_col_name} in de laatste 720 dagen"),
      glue("{cost_col_name} in de laatste 1000 dagen")
    )

    p <- ggplot(agg_wide_df[name == cost_col_name], aes(x = factor(t), fill = died)) +
      geom_boxplot(
        aes(
          ymin = q05_per_persoon,
          lower = q25_per_persoon,
          middle = mediaan_per_persoon,
          upper = q75_per_persoon,
          ymax = q95_per_persoon
        ),
        stat = "identity",
        width = 0.8
      ) +
      scale_x_discrete(breaks = function(x) x[seq(0, length(x), by = 3)]) +
      facet_wrap(~cohort, nrow = 1, scales = "free_x") +
      labs(
        title = plot_title,
        x = "Maand tot overlijden", y = NULL
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )

    ggsave(glue("{path}/{cost_col_name}_months_to_death.png"), p,
      scale = 0.6, width = 18, height = 10, dpi = 300
    )
  }
  rm(agg_wide_df)
  gc()
}

plot_months_to_death_gebruikt <- function(agg_df, cost_columns, path) {
  for (cost_col_name in cost_columns) {
    print(glue("creating boxplot months to death for {cost_col_name}"))

    plot_title <- ifelse(
      grepl("hadeclvergoedbedrag", cost_col_name),
      glue("{cost_col_name} in de laatste 720 dagen"),
      glue("{cost_col_name} in de laatste 1000 dagen")
    )

    p <- ggplot(agg_df[type == "gemiddelde_per_persoon" &
      name == cost_col_name], aes(x = factor(t), y = value, fill = died)) +
      geom_col(position = "dodge", width = 0.8) +
      scale_x_discrete(breaks = function(x) x[seq(0, length(x), by = 3)]) +
      facet_wrap(~cohort, nrow = 1, scales = "free_x") +
      labs(
        title = plot_title,
        x = "Maand tot overlijden", y = NULL
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )

    ggsave(glue("{path}/{cost_col_name}_months_to_death.png"), p,
      scale = 0.6, width = 14, height = 10, dpi = 300
    )
  }
}

plot_boxplot_costs_by_cohort <- function(cohort_df, cost_columns, path) {
  for (cost_col_name in cost_columns) {
    whiskers_by_group <- tapply(
      cohort_df[[cost_col_name]][cohort_df[[cost_col_name]] > 0],
      cohort_df[cohort_df[[cost_col_name]] > 0]$died, function(x) {
        boxplot.stats(x)$stats[c(1, 5)]
      }
    )
    lims_box <- range(unlist(whiskers_by_group))

    # Boxplot without outliers
    print(glue("creating boxplot for total 1000 days for {cost_col_name}"))
    plot_title <- ifelse(
      grepl("hadeclvergoedbedrag", cost_col_name),
      glue("Totale {cost_col_name} per gebruiker in de laatste 720 dagen"),
      glue("Totale {cost_col_name} per gebruiker in de laatste 1000 dagen")
    )

    p <- ggplot(cohort_df[cohort_df[[cost_col_name]] > 0], 
                aes(x = died, y = .data[[cost_col_name]])) +
      geom_boxplot(outlier.shape = NA) +
      facet_wrap(~cohort, nrow = 1, scales = "free_x") +
      coord_cartesian(ylim = lims_box) +
      labs(
        title = plot_title,
        x = NULL, y = NULL
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggsave(glue("{path}/{cost_col_name}_boxplot.png"), p,
      scale = 0.6, width = 14, height = 10, dpi = 300
    )

    # Density plot
    lim_density <- quantile(cohort_df[[cost_col_name]][cohort_df[[cost_col_name]] > 0],
      probs = 0.99, na.rm = T
    )

    print(glue("creating density plot for total 1000 days for {cost_col_name}"))

    plot_title <- ifelse(
      grepl("hadeclvergoedbedrag", cost_col_name),
      glue("Totale {cost_col_name} per gebruiker in de laatste 720 dagen, < 99th perc"),
      glue("Totale {cost_col_name} per gebruiker in de laatste 1000 dagen, < 99th perc")
    )

    p <- ggplot(
      cohort_df[cohort_df[[cost_col_name]] > 0 & cohort_df[[cost_col_name]] < lim_density],
      aes(x = .data[[cost_col_name]], fill = died)
    ) +
      geom_density(alpha = 0.4) +
      facet_wrap(~cohort, nrow = 1) +
      labs(
        title = plot_title,
        x = NULL, y = NULL
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )

    ggsave(glue("{path}/{cost_col_name}_density.png"), p,
      scale = 0.6, width = 14, height = 10, dpi = 300
    )
  }
  rm(whiskers_by_group, lims_box, lim_density, ann_text_t_test, ann_text_ks_test)
  gc()
}

plot_total_gebruikt <- function(plot_df, cost_columns_gebruikt, path) {
  agg_df <- make_aggregated_data(
    plot_df,
    group_var = c("cohort", "died"),
    columns = cost_columns_gebruikt
  )

  for (cost_col_name in cost_columns_gebruikt) {
    print(glue("creating barcharts total gebruikt for {cost_col_name}"))

    plot_title <- ifelse(
      grepl("hadeclvergoedbedrag", cost_col_name),
      glue("Ooit {unique(agg_df[name == cost_col_name]$name)} in de laatste 720 dagen"),
      glue("Ooit {unique(agg_df[name == cost_col_name]$name)} in de laatste 3 jaren")
    )

    p <- ggplot(agg_df[type == "gemiddelde_per_persoon" &
      name == cost_col_name], aes(x = died, y = value, fill = died)) +
      geom_col() +
      facet_wrap(~cohort, nrow = 1, scales = "free_x") +
      labs(
        title = plot_title,
        x = NULL, y = NULL
      ) +
      scale_fill_brewer(palette = "Set2") +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggsave(glue("{path}/{cost_col_name}.png"), p,
      scale = 0.6, width = 14, height = 10, dpi = 300
    )
  }
  rm(agg_df, ann_text_t_test)
  gc()
}

make_all_plots <- function(filepath_binned_dataset, 
                           cols_kosten = NULL, 
                           cols_kosten_gebruikt = NULL,
                           is_annual = FALSE) {
  cols_kosten_gebruikt <- if (is.null(cols_kosten_gebruikt)) {
    paste0("gebruikt_", cols_kosten)
  } else {
    cols_kosten_gebruikt
  }
  
  df <- r_parquet_get_dt(filepath_binned_dataset)
  
  print(names(df))
  
  # convert t to negative, if not done before
  if(mean(df$t, na.rm=T) > 0){
    df[, t := -1 * t]
  }
  
  # take last 24 months for huisarts
  if (grepl("huisarts", filepath_binned_dataset)) {
    df <- df[t >= -24]
  }
  setdiff(names(df), c(cols_kosten, cols_kosten_gebruikt))
  
  # monthly plots
  if (!is_annual) {
    df_agg <- make_aggregated_data(df, group_var = c("cohort", "died", "t"))
    if (!is.null(cols_kosten)){
      plot_months_to_death(df_agg, cols_kosten, path)
    }
    plot_months_to_death_gebruikt(df_agg, cols_kosten_gebruikt, path)
  }
  
  # total costs 1000 days
  if (!is.null(cols_kosten)){
  df_agg <- aggregate_binned_costs(df, cols_kosten, cols_kosten_gebruikt)
  plot_boxplot_costs_by_cohort(df_agg, cols_kosten, path)
  plot_total_gebruikt(df_agg, cols_kosten_gebruikt, path)
  } else {
    df_agg <- aggregate_binned_costs(df, cols_kosten_gebruikt)
    plot_total_gebruikt(df_agg, cols_kosten_gebruikt, path)
  }
  
  rm(df, df_agg)
  gc()
}


#### create plots zvw ####
make_all_plots(
  filepath = "./data/processed/zvw_1000_dagen.parquet",
  cols_kosten <- cost_columns_zvw,
  is_annual = TRUE
)


#### Create plots medicijn ####
df <- r_parquet_get_dt("./data/processed/medicijn.parquet")

cols_medicijn_gebruikt <- names(df)[grepl("^(gebruikt|heeft)", names(df))]
setdiff(names(df), cols_medicijn_gebruikt)

# Total usage 1000 days
plot_total_gebruikt(df, cols_medicijn_gebruikt, path)
gc()


#### create plot msz eerstelijn diagnostics ####
filepath_msz_prestatie <- "./data/processed/msz_prestatie_1000_dagen.parquet"
columns_msz_prestatie <- names(arrow::open_dataset(filepath_msz_prestatie))
cost_columns_msz_prestatie <- columns_msz_prestatie[grepl("^(kosten)", 
                                                          columns_msz_prestatie)]
gebruikt_columns_msz_prestatie <- columns_msz_prestatie[grepl("^(heeft)", 
                                                               columns_msz_prestatie)]
make_all_plots(
  filepath = filepath_msz_prestatie,
  cols_kosten = cost_columns_msz_prestatie,
  cols_kosten_gebruikt = gebruikt_columns_msz_prestatie
)


#### create plot msz tweedelijn diagnostic ####
filepath_msz_activiteiten <- "./data/processed/msz_activiteiten_1000_dagen.parquet"
columns_msz_activiteiten <- names(arrow::open_dataset(filepath_msz_activiteiten))
cost_columns_msz_activiteiten <- columns_msz_activiteiten[grepl("^(kosten)", 
                                                                columns_msz_activiteiten)]
gebruikt_columns_msz_activiteiten <- columns_msz_activiteiten[grepl("^(heeft)", 
                                                                columns_msz_activiteiten)]
make_all_plots(
  filepath = filepath_msz_activiteiten,
  cols_kosten <- NULL,
  cols_kosten_gebruikt = gebruikt_columns_msz_activiteiten
)


#### create plot msz ####
make_all_plots(
  filepath = "./data/processed/vektmszkosten_monthly.parquet",
  cols_kosten <- cost_columns_msz,
)


#### create plots huisartsdecltab ####
# make_all_plots(
#   filepath = "./data/processed/huisarts_monthly.parquet",
#   cols_kosten <- cost_columns_msz,
# )


#### create plots wlzzintab ####
make_all_plots(
  filepath = "./data/processed/wlzkosten_monthly.parquet",
  cols_kosten <- cost_columns_wlz,
)


#### create plots wijkverpleging ####
make_all_plots(
  filepath = "./data/processed/wvpkosten_monthly.parquet",
  cols_kosten <- cost_columns_wvp,
)

