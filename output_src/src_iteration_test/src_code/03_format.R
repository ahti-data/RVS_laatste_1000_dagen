source("./src/setup.R")

output_dir <- file.path("data", "processed", "02_analyzed")

## Target / Comp files

profile_depth <- "LG"

#### comp output ####

for (zorgtype in unique_zorgtypes) {
  output_years <- lapply(years, function(y) {
    output_components <- lapply(1:6, function(counter) {
      base_pc <- glue(
        "count_comp_{y}_{counter}_{zorgtype}_",
        "{profile_depth}_pc.xlsx"
      )
      base_use <- glue(
        "count_comp_{y}_{counter}_{zorgtype}_",
        "{profile_depth}_use.xlsx"
      )

      temp_pc <- setDT(readxl::read_xlsx(file.path(output_dir, base_pc)))
      temp_use <- setDT(readxl::read_xlsx(file.path(output_dir, base_use)))

      outcome_cols <- names(temp_pc)[grepl("target|comp", names(temp_pc))]

      pc <- temp_pc[, (outcome_cols) := lapply(.SD, `*`, N),
        .SDcols = outcome_cols
      ]
      use <- temp_use[, (outcome_cols) := lapply(.SD, `*`, N),
        .SDcols = outcome_cols
      ]

      wrangle_data <- function(data) {
        data_melt <- melt(data, id.vars = c("N", "code", "level"))
        data_melt[, type := "None"]
        data_melt[grepl("target", variable), type := "Target"]
        data_melt[grepl("comp", variable), type := "Comp"]
        data_melt[, variable := gsub("_comp|_target", "", variable)]

        data_melt <- data_melt[!is.na(data_melt$code) &
          !(data_melt$code == "----"), ]

        data_cast <- dcast(data_melt,
          formula = N + code + variable + level ~ type,
          value.var = "value"
        )
        return(data_cast)
      }

      pc_cast <- wrangle_data(pc)
      setnames(
        pc_cast, c("Comp", "Target"),
        c("n_comp", "n")
      )

      use_cast <- wrangle_data(use)
      setnames(
        use_cast, c("Comp", "Target"),
        c("n_individuals_comp", "n_individuals")
      )

      comb <- merge(pc_cast, use_cast)
      comb[, year := y]

      ## TEMPORARY; TO BE MOVED TO 02_ANALYSIS LATER: REMOVE ROAZ
      comb <- comb[level != "roaz"]

      # remove forbidden gems
      gem_to_remove <- c("0654", "0677", "0703", "1714", "0716", "0717")
      comb <- comb[!code %in% gem_to_remove]


      return(comb)
    })

    return(rbindlist(output_components))
  })

  names(output_years) <- as.character(years)



  openxlsx::write.xlsx(
    output_years,
    glue("./data/processed/03_formatted/full_output_inc_comp_{profile_depth}_{zorgtype}.xlsx")
  )

  ## Make output-ready:
  ## ROund all counts of actually observed things to 10s
  ## Remove all data for regions with less than 10 observed individuals in outcome
  ## Also round to tens whenever the comp is equal to the target
  output_years_output_ready <- lapply(output_years, function(o) {
    o[, c("N", "n", "n_individuals") := lapply(.SD, DescTools::RoundTo, 10),
      .SDcols = c("N", "n", "n_individuals")
    ]

    o[n_individuals < 10, c("n", "n_individuals", "n_comp", "n_individuals_comp")
    := NA]
    o[
      level == "nl" | grepl("onbekend", code),
      n_comp := DescTools::RoundTo(n_comp, 10)
    ]
    o[
      level == "nl" | grepl("onbekend", code),
      n_individuals_comp := DescTools::RoundTo(n_individuals_comp, 10)
    ]
    o <- o[N >= 10, ]
    return(o)
  })

  names(output_years_output_ready) <- as.character(years)

  openxlsx::write.xlsx(
    output_years_output_ready,
    glue("./output/output_3/full_output_inc_comp_{profile_depth}_{zorgtype}_output.xlsx")
  )
}


#### Descriptives files ####
metrics <- c("n", "n_ind")
sheets <- readxl::excel_sheets(glue("./data/processed/02_analyzed/demog_sets_n_2018_all.xlsx"))

## determine all levels
all_levels_demog <- list()
all_levels_code_level <- c()

for (y in years) {
  for (metric in metrics) {
      for (sheet_name in sheets) {
        temp_raw <- readxl::read_xlsx(glue("./data/processed/02_analyzed/demog_sets_{metric}_{y}_all.xlsx"), sheet = sheet_name)
        temp_raw <- as.data.table(temp_raw)[, year := y]
        temp_melt <- as.data.table(melt(temp_raw, id.vars = c(sheet_name, "code", "level")))
        temp_melt[, temp_comb_code_level := paste(code, level, sep = "_SPLIT_")]
  
        if (sheet_name %in% demog_splits) {
          all_levels_demog[[sheet_name]] <- union(all_levels_demog[[sheet_name]], unique(temp_melt[[sheet_name]]))
        }
        all_levels_code_level <- union(all_levels_code_level, unique(temp_melt$temp_comb_code_level))
      }
    }
  }

all_levels_demog[["profile_LG"]] <- union(all_levels_demog[["profile_LG"]], c("11_Mannen", "99_Vrouwen"))


for (zorgtype in unique_zorgtypes) {
  for (metric in metrics) {
    sheet_data <- lapply(sheets, function(s) {
      temp <- rbindlist(lapply(years, function(y) {
        temp_raw <- readxl::read_xlsx(glue("./data/processed/02_analyzed/demog_sets_{metric}_{y}_{zorgtype}.xlsx"),
          sheet = s
        )
        return(as.data.table(temp_raw)[, year := y])
      }))
  
      temp_melt <- as.data.table(melt(temp, id.vars = c(s, "code", "level", "year"))) # [, year := y]
      return(temp_melt)
    })
  
    names(sheet_data) <- sheets
  
    for (sheet_name in sheets) {
      agg_sheet_data <- sheet_data[[sheet_name]]
  
      # split swab/icd10
      agg_sheet_data_icd10 <- split(agg_sheet_data[grepl("icd10", variable)], by = "year")
      agg_sheet_data_swab <- split(agg_sheet_data[grepl("swab", variable)], by = "year")
  
      # create missing unique combinations
      agg_sheet_data_icd10_all <- lapply(
        X = agg_sheet_data_icd10,
        FUN = function(dt) {
          # create singular column for combining
          dt[, temp_comb_code_level := paste(code, level, sep = "_SPLIT_")]
          dt[, ":="(code = NULL, level = NULL)]
  
          # define all levels
          levels_all <- list()
          levels_all[[sheet_name]] <- all_levels_demog[[sheet_name]]
  
          print(length(all_levels_demog[[sheet_name]]))
          print(uniqueN(dt[[sheet_name]]))
  
          levels_all[["temp_comb_code_level"]] <- all_levels_code_level
          print(length(all_levels_code_level))
          print(uniqueN(dt$temp_comb_code_level))
  
          levels_all[["variable"]] <- unique(dt$variable)
          print(length(unique(dt$variable)))
  
          levels_all[["year"]] <- unique(dt$year)
  
          grp <- c(sheet_name, "temp_comb_code_level", "variable", "year")
  
          print(names(dt))
          print(unique(dt$year))
          print(names(levels_all))
          # create missing unique combs
          print(glue("adding missing unique combs for {metric}, {sheet_name}, icd10, {nrow(dt)} rows"))
          dt <- add_missing_unique_combs(
            dt,
            grp = grp,
            levels_all = levels_all
          )
          print(glue("unique combs added, {nrow(dt)} rows"))
  
          # add the columns back
          dt[, c("code", "level") := tstrsplit(temp_comb_code_level, "_SPLIT_", fixed = TRUE)]
          dt[, temp_comb_code_level := NULL]
  
          return(dt)
        }
      )
  
      agg_sheet_data_swab_all <- lapply(
        X = agg_sheet_data_swab,
        FUN = function(dt) {
          # create singular column for combining
          dt[, temp_comb_code_level := paste(code, level, sep = "_SPLIT_")]
          dt[, ":="(code = NULL, level = NULL)]
  
          # define all levels
          levels_all <- list()
          levels_all[[sheet_name]] <- all_levels_demog[[sheet_name]]
  
          print(length(all_levels_demog[[sheet_name]]))
          print(uniqueN(dt[[sheet_name]]))
  
          levels_all[["temp_comb_code_level"]] <- all_levels_code_level
          print(length(all_levels_code_level))
          print(uniqueN(dt$temp_comb_code_level))
  
          levels_all[["variable"]] <- unique(dt$variable)
          print(length(unique(dt$variable)))
  
          levels_all[["year"]] <- unique(dt$year)
  
          grp <- c(sheet_name, "temp_comb_code_level", "variable", "year")
  
          # create missing unique combs
          print(glue("adding missing unique combs for {metric}, {sheet_name}, icd10, {nrow(dt)} rows"))
  
          dt <- add_missing_unique_combs(
            dt,
            grp = grp,
            levels_all = levels_all
          )
          print(glue("unique combs added, {nrow(dt)} rows"))
  
          # add the columns back
          dt[, c("code", "level") := tstrsplit(temp_comb_code_level, "_SPLIT_", fixed = TRUE)]
          dt[, temp_comb_code_level := NULL]
  
          return(dt)
        }
      )
  
      # write before formatting output
      # openxlsx::write.xlsx(agg_sheet_data_icd10_all, glue("./output/output_3/univariate_counts_{metric}_icd10_{sheet_name}.xlsx"))
      # openxlsx::write.xlsx(agg_sheet_data_swab_all, glue("./output/output_3/univariate_counts_{metric}_swab_{sheet_name}.xlsx"))
  
      # Round to 0 and remove forbidden gems
      sheet_data_output_idc10 <- lapply(agg_sheet_data_icd10_all, function(sd) {
        sd$value[sd$value < 10] <- NA
        sd$value <- DescTools::RoundTo(sd$value, 10)
  
        # remove forbidden gems
        gem_to_remove <- c("0654", "0677", "0703", "1714", "0716", "0717")
        sd <- sd[!code %in% gem_to_remove]
        return(sd)
      })
      sheet_data_output_swab <- lapply(agg_sheet_data_swab_all, function(sd) {
        sd$value[sd$value < 10] <- NA
        sd$value <- DescTools::RoundTo(sd$value, 10)
  
        # remove forbidden gems
        gem_to_remove <- c("0654", "0677", "0703", "1714", "0716", "0717")
        sd <- sd[!code %in% gem_to_remove]
        return(sd)
      })
  
      output_csv_ready_icd10 <- rbindlist(lapply(sheet_data_output_idc10, function(dt) {
        dt[, variable := as.character(variable)]
        return(dt)
      }))
      print(unique(output_csv_ready_icd10$profile_LG))
  
      output_csv_ready_swab <- rbindlist(lapply(sheet_data_output_swab, function(dt) {
        dt[, variable := as.character(variable)]
        return(dt)
      }))
      print(unique(output_csv_ready_swab$profile_LG))
  
      fwrite(output_csv_ready_icd10, glue("./data/processed/03_formatted/univariate_counts_{metric}_icd10_{sheet_name}_{zorgtype}_output.csv"))
      fwrite(output_csv_ready_swab, glue("./data/processed/03_formatted/univariate_counts_{metric}_swab_{sheet_name}_{zorgtype}_output.csv"))
    }
  }
}

# finally, merge together the sheets
for (demog_name in demog_splits) {
  data_by_metric <- lapply(metrics, function(metric){
    all_data_by_zorgtypes <- lapply(unique_zorgtypes, function(zorgtype) {
      dt_icd10 <- fread(glue("data/processed/03_formatted/univariate_counts_{metric}_icd10_{demog_name}_{zorgtype}_output.csv"))
      dt_swab <- fread(glue("data/processed/03_formatted/univariate_counts_{metric}_swab_{demog_name}_{zorgtype}_output.csv"))
      dt_all <- rbindlist(list(dt_icd10, dt_swab), use.names=T)
      
      setnames(dt_all, "value", glue("{metric}_{zorgtype}"))
      
      return(dt_all)
    })
    
    merged_data_metric_demog <- Reduce(function(x, y) merge(x, y, by = c(demog_name, "year", "variable", "code", "level"), all = T), all_data_by_zorgtypes)
    
    return(merged_data_metric_demog)
  })
  
  combinations <- expand.grid(metric=metrics, zorgtype=unique_zorgtypes)
  all_value_col_names <- paste(combinations$metric, combinations$zorgtype, sep = "_")
  
  merged_data_demog <- Reduce(function(x, y) merge(x, y, 
                                                   by = setdiff(c(names(x), names(y)), all_value_col_names), 
                              all = T), data_by_metric)
  
  fwrite(merged_data_demog, glue("output/output_3/univariate_counts_{demog_name}.csv"))
}

#### format total hospitalizations ####
# total_n_hospitalizations <- list()
# for (y in years) {
#   n_hospitalizations <- as.data.table(fread(glue("data/output/total_n_admissions_{y}.csv")))
#   n_hospitalizations[, year := y]
# 
#   total_n_hospitalizations[[y]] <- n_hospitalizations
# }
# total_n_hospitalizations <- rbindlist(total_n_hospitalizations)
# openxlsx::write.xlsx(total_n_hospitalizations, glue("./output/output_3/total_n_admissions.xlsx"))
