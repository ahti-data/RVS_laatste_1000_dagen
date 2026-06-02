source("./src/setup.R")

## Read LBZ set with mapped swab codes.

for (zorgtype in unique_zorgtypes) {
  lbz_comb <- arrow::read_parquet(glue("./data/processed/00_cleaned/lbz_data_{zorgtype}_18_23.parquet"))
  
  ## Wrangle lbz_comb from a long list of individuals spells
  ## into a dummy-coded dataset with columns for each icd10 and swap group,
  ## unique individuals as rows and the number of occurrences as the value.
  outcome_cols <- c("icd10_disease", "swab")
  
  ## Melt into a long set at the rinpersoon x year level.
  lbz_comb_melt <- melt(lbz_comb,
                        id.vars = c("rinpersoon", "year"),
                        measure.vars = outcome_cols,
                        variable.name = "var",
                        value.name = "level"
  )
  
  ## Cast into wide where the number of rows is entered as the values
  lbz_comb_cast <- dcast(
    lbz_comb_melt[, .N, by = .(rinpersoon, var, level, year)],
    rinpersoon + year ~ var + level,
    value.var = "N",
    fill = 0
  )
  
  rm(lbz_comb_melt, lbz_comb)
  gc()
  
  format_regions <- function(data) {
    ## Make missing gem codes "----"
    data[is.na(gem), gem := "----"]
    
    ## Make gemeentecodes a four length string with leading zeroes
    data[, gem := stringr::str_pad(gem, 4, "left", "0")]
    
    ## Add GGD, AMR from GIN. assuming all gemeentecodes are from 2022.
    GIN_2022 <- readxl::read_xlsx(
      "K:/Utilities/HULPbestanden/GebiedeninNederland/GIN2022.xlsx"
    )
    GIN_2022 <- as.data.table(GIN_2022)
    gin_rel_cols <- c(
      "gemeenten|Code", "gemeenten|Naam",
      "GGD-regio's|Code",
      "GGD-regio's|Naam",
      "Provincies|Naam"
    )
    
    GIN_2022_rel <- GIN_2022[, ..gin_rel_cols]
    
    ## Add restgroup "----" to the crosswalk.
    GIN_2022_rel <- rbind(
      GIN_2022_rel,
      as.list(rep("----", ncol(GIN_2022_rel)))
    )
    
    ## Ensure all gemeenten feature in the crosswalk.
    assertthat::assert_that(
      length(unique(data$gem[
        !(data$gem %in% GIN_2022_rel$`gemeenten|Code`)
      ])) == 0
    )
    
    ## Make names more compact.
    setnames(GIN_2022_rel, gin_rel_cols, c(
      "gem", "gem_naam", "ggd", "ggd_naam",
      "prov_naam"
    ))
    
    ## Add GIN to the dataset.
    data <- merge(data, GIN_2022_rel, by = "gem", all.x = T)
    
    ## Add ROAZ and AMR based on crosswalk provided by client.
    roaz_cw <- readxl::read_xlsx(
      "./data/raw/Indeling ROAZ regio-AMR-regio_2024.xlsx"
    )
    roaz_cw <- setDT(roaz_cw)
    
    ## Overwrite merged ROAZ regios
    roaz_cw[
      ROAZ_regio %in% c("SpoedZorgNet", "Netwerk Acute Zorg Noordwest"),
      ROAZ_regio := "Netwerk Acute Zorg Noord-Holland / Flevoland"
    ]
    
    ## Check if there are ever gemeenten that are mapped to multiple roaz or amr
    gem_roaz_cw <- roaz_cw[, c("Gem_nr", "ROAZ_regio", "AMR Zorgnetwerk")]
    
    gem_roaz_cw_count <- gem_roaz_cw[
      , .(
        unique_roaz = length(unique(ROAZ_regio)),
        unique_amr = length(unique(`AMR Zorgnetwerk`))
      ),
      by = "Gem_nr"
    ]
    
    assertthat::assert_that(all(gem_roaz_cw_count$unique_roaz ==
                                  gem_roaz_cw_count$unique_roaz[1]))
    assertthat::assert_that(all(gem_roaz_cw_count$unique_amr ==
                                  gem_roaz_cw_count$unique_amr[1]))
    
    ## Make names more compact
    setnames(
      gem_roaz_cw, c("Gem_nr", "ROAZ_regio", "AMR Zorgnetwerk"),
      c("gem", "roaz", "amr")
    )
    gem_roaz_cw[, gem := stringr::str_pad(gem, 4, "left", "0")]
    
    ## Only keep the first occurrence of each gemeente to roaz / amr
    gem_roaz_cw_first_occurrence <- gem_roaz_cw[
      , .(
        roaz = first(roaz),
        amr = first(amr)
      ),
      by = gem
    ]
    
    ## Merge roaz / amr to data
    data <- merge(data,
                  gem_roaz_cw_first_occurrence,
                  by = "gem",
                  all.x = T
    )
    
    ## Manual additions because some municipalities fused
    data[
      gem == "1980",
      roaz := "Netwerk Acute Zorg Noordwest"
    ]
    data[
      gem == "1982",
      roaz := "Netwerk Acute Zorg Brabant"
    ]
    data[
      gem == "1991",
      roaz := "Netwerk Acute Zorg Brabant"
    ]
    
    data[
      gem == "1980",
      amr := "AMR Zorgnetwerk NH - FL"
    ]
    data[
      gem == "1982",
      amr := "Rezisto"
    ]
    data[
      gem == "1991",
      amr := "Rezisto"
    ]

    ## Ensure all gemeenten other than "----" have a ROAZ regio.
    # assertthat::assert_that(
    #   !any(is.na(data$roaz[
    #     !(data$gem == "----")
    #   ]))
    # )
    
    ## Ensure all gemeenten other than "----" have an AMR regio.
    assertthat::assert_that(
      !any(is.na(data$amr[
        !(data$gem == "----")
      ]))
    )
    return(data)
  }
  
  ## Read demog years and full join yearly LBZ data (merging in batching makes
  ## this sort of ok to run..)
  unique_rins <- unique(lbz_comb_cast$rinpersoon)
  demog_data <- rbindlist(lapply(years_demog, function(y){
    print(glue::glue("Getting data for demog {y}"))
    ds <- arrow::open_dataset(
      glue("H:/data/demog/{y}/rin_demog.parquet")
    )
    
    df <- ds |>
      filter(rinpersoon %in% unique_rins) |>
      select(all_of(rel_cols_demog)) |>
      collect()
    
    df[, year := y]
    
    return(df)
    }))
  
  demog_data <- haven::zap_labels(demog_data)
  demog_data <- format_data(demog_data)
  demog_data[, leeftijd_cat := floor(leeftijd / 10)]
  
  
  for (y in years - 1) { ## We load T-1 so all people are present at the start of T
    demog_data_year <- demog_data[year == y]
    demog_data_year <- align_gm_to_target_year(
      demog_data_year,
      target_year = "2022", source_years = c(y)
    )
    demog_data_year$gem <- NULL
    setnames(demog_data_year, "gem_new", "gem")
    
    ## Move year up 1 because we read t-1 but want to merge to outcomes in t
    demog_data_year[, year := y + 1]
    
    ## Read outcomes
    temp_lbz <- lbz_comb_cast[year == y + 1, ]
    temp_lbz[, year := NULL] ## Remove to avoid duplicate columns in merge
    
    setkey(demog_data_year, "rinpersoon")
    setkey(temp_lbz, "rinpersoon")
    
    comb <- merge(temp_lbz, demog_data_year, all.x = TRUE)
    
    unmatched_rins <- setdiff(
      unique(temp_lbz$rinpersoon),
      unique(demog_data_year$rinpersoon)
    )
    
    #try to match the rins to the same year, for those unmatched
    demog_data_year <- demog_data[year == y+1]
    demog_data_year <- align_gm_to_target_year(
      demog_data_year,
      target_year = "2022", source_years = c(y+1)
    )
    demog_data_year$gem <- NULL
    setnames(demog_data_year, "gem_new", "gem")
    
    
    comb_matched <- comb[!is.na(geslacht)]
    temp_lbz_unmatched <- temp_lbz[rinpersoon %in% unique(comb[is.na(geslacht)]$rinpersoon)]
    
    comb_unmatched <- merge(temp_lbz_unmatched, demog_data_year, all.x = TRUE)
    
    comb <- rbindlist(list(comb_matched, comb_unmatched))

    print(y)
    print("rows without geslacht:")
    print(nrow(comb[is.na(geslacht)]))
    print("rinpersonen that can't be matched:")
    print(sum(!unique(temp_lbz$rinpersoon) %in% unique(demog_data_year$rinpersoon)))
    
    ## Report how many LBZ opnames were not matchable to the demog.
    ## These are people that migrated back to the Netherlands or were born
    ## after the 1st of January of T.
    print(glue::glue(
      "{round(mean(is.na(comb$gem)) * 100, 2)}% of opnames not merged"
    ))
    
    ## Ensure all LBZ are covered in the combined data (full join)
    assertthat::assert_that(all(temp_lbz$rinpersoon %in%
                                  comb$rinpersoon))
    
    ## Format regions
    comb <- format_regions(comb)
    
    ## Write per year to edit folder.
    arrow::write_parquet(
      comb,
      glue("./data/processed/01_merged/merged_dt_{y+1}_{zorgtype}.parquet")
    )
    rm(temp_lbz, comb)
    gc()
  }
}

