source("src/setup.R")

for (zorgtype in unique_zorgtypes) {
  for (yr in years) {
    ## Read year-level data
    df_sheet <- arrow::read_parquet(
      glue("./data/processed/01_merged/merged_dt_{yr}_{zorgtype}.parquet")
    )
    setDT(df_sheet)
  
    ## Make the 'missing' levels more informative and add NL totals as aggregation
    df_sheet[is.na(gem) | gem == "----" | gem == "", gem := "GGD onbekend"]
    df_sheet[is.na(ggd) | ggd == "----" | ggd == "", ggd := "GGD onbekend"]
    df_sheet[is.na(amr) | amr == "----" | amr == "", amr := "AMR onbekend"]
    df_sheet[, nl := "NL"]
  
    ## Remove all names that might lead to syntax errors from the outcome columns
    setnames(
      df_sheet, names(df_sheet),
      gsub("[^A-Za-z0-9_]", "_", names(df_sheet))
    )
  
    ## Make profile LG
    ## NOTE: may want to create multiple and run a for loop over the profile depths
    df_sheet[, profile_LG := paste0(leeftijd_cat, "_", geslacht)]
  
    profile_depth_indicators <- c("LG")
    gc()

    for (profile_depth_indicator in profile_depth_indicators) {
      ## Select outcomes of interest
      outcomes_target <- names(df_sheet)[grepl("swab|icd10", names(df_sheet))]

      ## Split into batches to make comp output manageable to run
      outcomes_target_split <- split(
        outcomes_target, cut(seq_along(outcomes_target), 6, labels = F)
      )

      ## Make comp output using all individuals as comp and as target sets
      for (outcome_split in 1:length(outcomes_target_split)) {
        results <- make_comp(
          df_sheet,
          target_rins = df_sheet$rinpersoon,
          comp_rins = df_sheet$rinpersoon,
          outcomes_target = outcomes_target_split[[outcome_split]],
          profile_depth = profile_depth_indicator,
          agg_cols = c("gem", "ggd", "amr", "nl", "prov_naam"),
          costs_per_user = F
        )

        write_results(results, outcomes_target_split[[outcome_split]],
          file_name = glue(
            "count_comp_{yr}_{outcome_split}_{zorgtype}_",
            "{profile_depth_indicator}"
          ),
          path = "./data/processed/02_analyzed/"
        )
      }
    }

    ## Make basic descriptives without comps subsetting by covariates
    outcomes_target <- names(df_sheet)[grepl("swab|icd10", names(df_sheet))]
    demog_sets_n <- lapply(
      demog_splits, function(d) {
        rbindlist(lapply(
          c("gem", "ggd", "amr", "nl", "prov_naam"), function(a) {
            if (d == "all") {
              temp <- df_sheet[
                , lapply(.SD, sum, na.rm = T),
                by = c(a),
                .SDcols = outcomes_target
              ][, all := "all"]
            } else {
              temp <- df_sheet[
                , lapply(.SD, sum, na.rm = T),
                by = c(d, a),
                .SDcols = outcomes_target
              ]
            }
            setnames(temp, a, "code")
            temp[, level := a]
            return(temp)
            }
        ))
      }
    )

    names(demog_sets_n) <- demog_splits

    openxlsx::write.xlsx(
      demog_sets_n, glue("./data/processed/02_analyzed/demog_sets_n_{yr}_{zorgtype}.xlsx")
    )

    demog_sets_n_ind <- lapply(
      demog_splits, function(d) {
        rbindlist(lapply(
          c("gem", "ggd", "amr", "nl", "prov_naam"), function(a) {
            if (d == "all") {
              temp <- df_sheet[
                , lapply(.SD, function(x) sum(x > 0, na.rm = T)),
                by = c(a),
                .SDcols = outcomes_target
                ][, all := "all"]
            } else {
              temp <- df_sheet[
                , lapply(.SD, function(x) sum(x > 0, na.rm = T)),
                by = c(d, a),
                .SDcols = outcomes_target
              ]
            }
            setnames(temp, a, "code")
            temp[, level := a]
            return(temp)
          }
        ))
      }
    )

    names(demog_sets_n_ind) <- demog_splits

    openxlsx::write.xlsx(
      demog_sets_n_ind, glue("./data/processed/02_analyzed/demog_sets_n_ind_{yr}_{zorgtype}.xlsx")
    )
  }
}
