# # Project: Laatste 1000 dagen
# # Author: Stanislav Avdeev & Marco Griep
# # Goal: Find the DBC codes in the last 1000 days
# # Output: A list of DBC codes and frequencies
# # Last edited: 20 April 2026
# 
# #### Initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")

dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)

# No need to use chunks with heavy server, if chuncks are returned, return _{i} below
# Load in filtered prestaties, chunk because of large size
#n_chunks <- 10
#rinpersoon_set_chunks <- split(rinpersoon_set, cut(seq_along(rinpersoon_set),
#                                                   n_chunks, labels = FALSE))
#for (i in seq_along(rinpersoon_set_chunks)) {
#  rinpersoon_set_chunk <- rinpersoon_set_chunks[[i]]
#  print(glue("Currently processing chunk {i}/{n_chunks} for mszprestaties"))

for (i in 1) {
  rinpersoon_set_chunk <- rinpersoon_set

  dt_msz_prestatie <- list()
  for (yr in years) {
    print(glue("Currently processing {yr} for mszprestaties"))

    file_path <- get_newest_parquet_check(
      folder_g_parquet = NULL,
      folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files/",
      folder_g_sav = 'G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/',
      string_pattern_parquet = yr,
      string_pattern_sav = yr,
      stop_on_mismatch = F)
    file_path = tools::file_path_sans_ext(file_path)

    ds <- arrow::open_dataset(file_path)
    dt <- ds |>
      # @filter
      filter(RINPERSOON %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(cols_to_select_msz_prestatie)])) |>
      collect()
    dt <- format_data(dt)
    stopifnot(any(dt$rinpersoon < 100000000))

    # Create specialist and diagnosis codes
    dt[, ':='(spec = substr(vektmszspecialismediagnosecombinatie, 1, 4), ## REVIEW: is this col numeric? otherwise substr may cause issues with codes that have leading zeroes. Same for other similar lines below
              diag = substr(vektmszspecialismediagnosecombinatie, 12, 15))]

    #### Save the names of DBC codes for heup and AAA ####
    dbc_names <- rio::import(
      'K:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/codelijst variabeleVEKTMSZDBCZorgproduct.xlsx')
    names(dbc_names) <- c('vektmszdbczorgproduct', 'vektmszdbczorgproduct_naam')
    dbc_names <- format_data(dbc_names, rin_num = F)
    dbc_names[, vektmszdbczorgproduct := gsub('"', '', vektmszdbczorgproduct)]
    dt <- merge(dt, dbc_names, all.x = T, by = c('vektmszdbczorgproduct'))

    # Check missings
    message('Share of missing names of DBC codes: ',
            mean(ifelse(is.na(dt$vektmszdbczorgproduct_naam), 1, 0)))

    # Keep only a subset
    dt[, ':='(is_aaa_kijkoperatie = as.integer(grepl('kijkoperatie', vektmszdbczorgproduct_naam, ignore.case = T)),

              is_aaa_operatie = as.integer(grepl('operatie', vektmszdbczorgproduct_naam, ignore.case = T) &
                                         !(grepl('kijkoperatie', vektmszdbczorgproduct_naam, ignore.case = T))),

              is_heup = as.integer(grepl('heup', vektmszdbczorgproduct_naam, ignore.case = T)),

              is_heup_operatie = as.integer(grepl('operatie|prothese', vektmszdbczorgproduct_naam, ignore.case = T)),

              is_heup_protese = as.integer(grepl('prothese', vektmszdbczorgproduct_naam, ignore.case = T))
              )]

    dt[, ':='(
      heeft_aaa_totaal = as.integer(spec == '0303' & diag %in% c('0405', '0406')),

      heeft_aaa_kijkoperatie = as.integer(spec == '0303' & diag %in% c('0405', '0406') &
                                      is_aaa_kijkoperatie == 1),

      heeft_aaa_operatie = as.integer(spec == '0303' & diag %in% c('0405', '0406')  &
                                        is_aaa_operatie == 1),


      heeft_aaa_0405_totaal = as.integer(spec == '0303' & diag == '0405'),

      heeft_aaa_0405_kijkoperatie = as.integer(spec == '0303' & diag == '0405'  &
                                                 is_aaa_kijkoperatie == 1),

      heeft_aaa_0405_operatie = as.integer(spec == '0303' & diag == '0405' &
                                             is_aaa_operatie == 1),


      heeft_aaa_0406_totaal = as.integer(spec == '0303' & diag == '0406'),

      heeft_aaa_0406_kijkoperatie = as.integer(spec == '0303' & diag == '0406'  &
                                                 is_aaa_kijkoperatie == 1),

      # @outcome 4
      heeft_aaa_0406_operatie = as.integer(spec == '0303' & diag == '0406' &
                                             is_aaa_operatie == 1),


      heeft_heup_totaal = as.integer(
        (spec == '0303' & diag %in% c('0217', '0218', '0219', '0240')) |
          (spec == '0305' & diag %in% c('3017', '3018', '3019', '3020'))),

      heeft_heup_operatie = as.integer(
        ((spec == '0303' & diag %in% c('0217', '0218', '0219', '0240')) |
          (spec == '0305' & diag %in% c('3017', '3018', '3019', '3020')))  &
          (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_prothese = as.integer(
        ((spec == '0303' & diag %in% c('0217', '0218', '0219', '0240')) |
          (spec == '0305' & diag %in% c('3017', '3018', '3019', '3020'))) &
          is_heup_protese == 1),


      heeft_heup_femur_bekken_totaal = as.integer(
        (spec == '0303' & diag %in% c('0217', '0218', '0219')) |
          (spec == '0305' & diag %in% c('3017', '3019', '3020'))),

      heeft_heup_femur_bekken_operatie = as.integer(
        ((spec == '0303' & diag %in% c('0217', '0218', '0219')) |
          (spec == '0305' & diag %in% c('3017', '3019', '3020'))) &
          (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_femur_bekken_prothese = as.integer(
        ((spec == '0303' & diag %in% c('0217', '0218', '0219')) |
          (spec == '0305' & diag %in% c('3017', '3019', '3020'))) &
          is_heup_protese == 1),


      heeft_heup_femur_totaal = as.integer(
        (spec == '0303' & diag %in% c('0218', '0219')) |
          (spec == '0305' & diag %in% c('3019', '3020'))),

      heeft_heup_femur_operatie = as.integer(
        ((spec == '0303' & diag %in% c('0218', '0219')) |
          (spec == '0305' & diag %in% c('3019', '3020'))) &
          (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_femur_prothese = as.integer(
        ((spec == '0303' & diag %in% c('0218', '0219')) |
          (spec == '0305' & diag %in% c('3019', '3020'))) &
          is_heup_protese == 1),


      heeft_heup_femur_proximaal_totaal = as.integer(
        (spec == '0303' & diag %in% c('0218')) |
          (spec == '0305' & diag %in% c('3019'))),

      heeft_heup_femur_proximaal_operatie = as.integer(
        ((spec == '0303' & diag %in% c('0218')) |
          (spec == '0305' & diag %in% c('3019'))) &
          (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_femur_proximaal_prothese = as.integer(
        ((spec == '0303' & diag %in% c('0218')) |
          (spec == '0305' & diag %in% c('3019'))) &
          is_heup_protese == 1),


      heeft_heup_0303_0217_totaal = as.integer(spec == '0303' & diag == '0217'),

      heeft_heup_0303_0217_operatie = as.integer(spec == '0303' & diag == '0217' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),
      # @outcome 5
      heeft_heup_0303_0217_prothese = as.integer(spec == '0303' & diag == '0217' &
                                                   is_heup_protese == 1),


      heeft_heup_0303_0218_totaal = as.integer(spec == '0303' & diag == '0218'),

      heeft_heup_0303_0218_operatie = as.integer(spec == '0303' & diag == '0218' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0303_0218_prothese = as.integer(spec == '0303' & diag == '0218' &
                                                   is_heup_protese == 1),


      heeft_heup_0303_0219_totaal = as.integer(spec == '0303' & diag == '0219'),

      heeft_heup_0303_0219_operatie = as.integer(spec == '0303' & diag == '0219' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0303_0219_prothese = as.integer(spec == '0303' & diag == '0219' &
                                                   is_heup_protese == 1),


      heeft_heup_0303_0240_totaal = as.integer(spec == '0303' & diag == '0240'),

      heeft_heup_0303_0240_operatie = as.integer(spec == '0303' & diag == '0240' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0303_0240_prothese = as.integer(spec == '0303' & diag == '0240' &
                                                   is_heup_protese == 1),


      heeft_heup_0305_3017_totaal = as.integer(spec == '0305' & diag == '3017'),

      heeft_heup_0305_3017_operatie = as.integer(spec == '0305' & diag == '3017' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0305_3017_prothese = as.integer(spec == '0305' & diag == '3017' &
                                                   is_heup_protese == 1),


      heeft_heup_0305_3018_totaal = as.integer(spec == '0305' & diag == '3018'),

      heeft_heup_0305_3018_operatie = as.integer(spec == '0305' & diag == '3018' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0305_3018_prothese = as.integer(spec == '0305' & diag == '3018' &
                                                   is_heup_protese == 1),


      heeft_heup_0305_3019_totaal = as.integer(spec == '0305' & diag == '3019'),

      heeft_heup_0305_3019_operatie = as.integer(spec == '0305' & diag == '3019' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0305_3019_prothese = as.integer(spec == '0305' & diag == '3019' &
                                                   is_heup_protese == 1),


      heeft_heup_0305_3020_totaal = as.integer(spec == '0305' & diag == '3020'),

      heeft_heup_0305_3020_operatie = as.integer(spec == '0305' & diag == '3020' &
                                                   (is_heup_protese == 1 | (is_heup_operatie == 1 & is_heup == 1))),

      heeft_heup_0305_3020_prothese = as.integer(spec == '0305' & diag == '3020' &
                                                   is_heup_protese == 1),
      
	  
      # @outcome 6
      heeft_add_on_ic = as.integer(vektmszdeclaratietype == '15')
      )]

    dt[, c('vektmszspecialismediagnosecombinatie', 'spec', 'diag', 'is_heup', 'vektmszdeclaratietype',
           'is_aaa_kijkoperatie', 'is_aaa_operatie', 'is_heup_operatie', 'is_heup_protese') := NULL]
    

    # Pre-select before merging for memory reduction
    cols_to_keep <- names(dt)[grepl("^(heeft_)", names(dt))]
    dt[, has_any_code := rowSums(.SD, na.rm = T), .SDcols = cols_to_keep]
    dt <- dt[has_any_code > 0][, has_any_code := NULL]

    # Keep DBC codes only in the last 1000 days
    dt <- merge(dt_overlijden_with_matched[as.numeric(rinpersoon) %in% rinpersoon_set_chunk,
                                           .(rinpersoon, gbadatumoverlijden, cohort, died)],
                dt,
                by = 'rinpersoon', all.x = T, allow.cartesian = T)
    dt[, vektmszbegindatumprest := as.Date(vektmszbegindatumprest, format = "%Y%m%d")]
    dt <- dt[vektmszbegindatumprest <= gbadatumoverlijden &
               vektmszbegindatumprest >= gbadatumoverlijden - 1000] #REVIEW: is gbadatumoverlijden Date type? I think so, but otherwise this would cause issues
    dt[, c('gbadatumoverlijden', 'cohort', 'died') := NULL]
    dt_msz_prestatie[[yr]] <- dt
    rm(dt)
    gc()
  }

  dt_msz_prestatie <- rbindlist(dt_msz_prestatie, use.names = T)
  arrow::write_parquet(dt_msz_prestatie, "data/raw/mszprestatie_check_codes.parquet")


  #### Create columns of costs per condition ####
  cols_msz_gebruikt <- names(dt_msz_prestatie)[grepl("^(heeft|gebruikt)",
                                                     names(dt_msz_prestatie))]
  cols_msz_kosten <- sub('heeft_', 'kosten_', cols_msz_gebruikt)

  dt_msz_prestatie[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]
  dt_msz_prestatie[, (cols_msz_kosten) := lapply(.SD, function(x)
    x * vektmszvergoedbedragzvw),
    .SDcols = cols_msz_gebruikt]

  dt_msz_prestatie[, 'vektmszvergoedbedragzvw' := NULL]
  print(setdiff(names(dt_msz_prestatie), cols_msz_kosten))
  gc()

  # Rename usage to the number of times used as they are summed later in calculate_costs_by_bin_size()
  setnames(dt_msz_prestatie, cols_msz_gebruikt, sub('^heeft_', 'n_', cols_msz_gebruikt))

  # Calculate the costs separately due to memory issues
  prestatie_vars <- setdiff(names(dt_msz_prestatie), c('rinpersoon', 'vektmszbegindatumprest'))
  prestatie_butches <- split(prestatie_vars, cut(seq_along(prestatie_vars),
                                                 breaks = 10, labels = F))

  # Calculate costs in bins
  results <- vector('list', length(prestatie_butches))
  for (prestatie_var in seq_along(prestatie_butches)){
    cat('Running batch: ', prestatie_var)
    results[[prestatie_var]] <- calculate_costs_by_bin_size(
      dt_msz_prestatie[, c('rinpersoon', 'vektmszbegindatumprest',
                           prestatie_butches[[prestatie_var]]),
                       with = F],
      dt_overlijden_with_matched[as.numeric(rinpersoon) %in% rinpersoon_set_chunk],
      cost_columns = prestatie_butches[[prestatie_var]],
      cost_date_col = "vektmszbegindatumprest",
      bin_size = "months33",
      aggregate_groupby_cols = names(dt_overlijden_with_matched)
    )
  }
  rm(dt_msz_prestatie)
  gc()

  dt_msz_prestatie_monthly <- Reduce(function(x, y) merge(x, y, by = c(
    names(dt_overlijden_with_matched), 't'),
    all = T),
    results)
  rm(results)
  gc()

  # Save
  setindex(dt_msz_prestatie_monthly, NULL)
  arrow::write_parquet(dt_msz_prestatie_monthly,
                       glue::glue("./data/processed/msz_prestatie_monthly.parquet")) # _{i}
  rm(dt_msz_prestatie_monthly)
  gc()


  #### Find MSZ activities ####
  zpk_codes <- rio::import("K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods")
  zpk_codes <- format_data(zpk_codes, rin_num = F)
  zpk_codes <- zpk_codes[, .(vektmszzorgactiviteit = mszzorgactiviteit,
                             heeft_zpk_4 = as.integer(zpkcode == 4),
                             heeft_zpk_7 = as.integer(zpkcode == 7),
                             heeft_zpk_8 = as.integer(zpkcode == 8)
  )]

  dt_msz_activiteiten <- list()
  for (yr in years) {
    print(glue("Currently processing {yr} for mszactiviteiten"))

    file_path <- get_newest_parquet_check(
      folder_g_parquet = NULL,
      folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZActiviteiten/parquet_files/",
      folder_g_sav = 'G:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/',
      string_pattern_parquet = yr,
      string_pattern_sav = yr,
      stop_on_mismatch = F)
    file_path = tools::file_path_sans_ext(file_path)

    ds <- arrow::open_dataset(file_path)
    dt <- ds |>
      # @filter
      filter(RINPERSOON %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(cols_to_select_msz_activiteiten)])) |>
      collect()
    dt <- format_data(dt)# REVIEW: consider renaming to dt_activiteiten or similar, for readability
    stopifnot(any(dt$rinpersoon < 100000000))

    # Get prestaties
    file_path <- get_newest_parquet_check(
      folder_g_parquet = NULL,
      folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files/",
      folder_g_sav = 'G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/',
      string_pattern_parquet = yr,
      string_pattern_sav = yr,
      stop_on_mismatch = F)
    file_path = tools::file_path_sans_ext(file_path)

    ds <- arrow::open_dataset(file_path)
    dt_prestatie <- ds |>
      # @filter
      filter(RINPERSOON %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% c('rinpersoon',
                                  'vektmszkoppelidprestza', 'vektmszbeginjaarprest',
                                  'vektmszbegindatumprest', 'vektmszdeclaratiecode',
                                  'vektmszdeclaratietype')])) |>
      collect()
    dt_prestatie <- format_data(dt_prestatie)
    stopifnot(any(dt_prestatie$rinpersoon < 100000000))

    # Merge activities with prestaties to get the declaration type, i.e. DBCs
    dt <- merge(dt, dt_prestatie, all.x = T,
                by = c('rinpersoon', 'vektmszkoppelidprestza', 'vektmszbeginjaarprest'))

    # REVIEW: ^^ by merging prestaties (right) into activiteiten (left), the small
    # portion of DBCs that are not mergeable with the activiteiten (the very small portion, as discussed yesterday)
    # will now be dropped, rather than placed into an "overig" category. Not sure how this applies
    # to your analysis below, perhaps this doesn't matter, but just a small thing

    dt[, c('vektmszkoppelidprestza', 'vektmszbeginjaarprest',
          'vektmszbegindatumprest', 'vektmszdeclaratiecode') := NULL]

    message('\n Share of missing declation codes: ',
        mean(ifelse(is.na(dt$vektmszdeclaratietype), 1, 0)), '\n')

    # Now get the rest of prestatie file, i.e. OZPs
    dt_prestatie <- dt_prestatie[
      !substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")
    ] # REVIEW: I need to check/change this in my code as well, but if this column is character type and has leading zeroes, might cause issues

    # Synchronize
    dt_prestatie <- dt_prestatie[, .(rinpersoon,
                                     vektmszzorgactiviteit = vektmszdeclaratiecode,
                                     vektmszzorgactiviteitdatum = vektmszbegindatumprest,
                                     vektmszdeclaratietype = vektmszdeclaratietype
    )]

    # Bind activitieten and ozp's from prestaties
    dt <- rbindlist(list(dt, dt_prestatie), use.names = T)
    rm(dt_prestatie)

    # Merge the diagnostic codes
    dt <- merge(dt, zpk_codes, all.x = T, by = 'vektmszzorgactiviteit') # REVIEW: might want to add a check here to see if the merge is going well

    message('\n Share of missing zpk_codes: ',
        mean(ifelse(is.na(dt$heeft_zpk_4), 1, 0)), '\n')

    # Add the names of zorgproducten
    dbc_names <- rio::import(
      'K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods')
    dbc_names <- format_data(dbc_names, rin_num = F)
    dbc_names <- dbc_names[, .(vektmszzorgactiviteit = mszzorgactiviteit,
                               mszzorgactiviteitomschrijving)]

    dt <- merge(dt, dbc_names, all.x = T, by = c('vektmszzorgactiviteit'))
    message('\n Share of missing descriptions: ',
        mean(ifelse(is.na(dt$mszzorgactiviteitomschrijving), 1, 0)), '\n')

    # Keep only a subset of activities relevant for beeldvorming
    dt <- dt[heeft_zpk_4 == 1 | heeft_zpk_7 == 1 | heeft_zpk_8 == 1]

    # Create
    dt[heeft_zpk_4 == 1 | heeft_zpk_7 == 1,
       beeldvorming_hoofdcategorie := dplyr::case_when(
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('begeleiding', ignore_case = T)) ~ 'overig',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('punctie|biopsie', ignore_case = T)) ~ 'punctie_biopsie',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('MRI', ignore_case = T)) ~ 'mri',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('CT', ignore_case = F)) &
           !(stringr::str_detect(mszzorgactiviteitomschrijving,
                                 stringr::regex('OCT|SPECT |PET-CT', ignore_case = T))) ~ 'ct_scan',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('SPECT|PET', ignore_case = F)) ~ 'pet_spect',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('scopie', ignore_case = T)) &
           !(stringr::str_detect(mszzorgactiviteitomschrijving,
                                 stringr::regex('microscopie', ignore_case = T))) ~ 'scopie',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('echo', ignore_case = T)) ~ 'echo',
         stringr::str_detect(mszzorgactiviteitomschrijving,
                             stringr::regex('radiol', ignore_case = T)) ~ 'radiologie',
         TRUE ~ 'overig'
       )]

    # Add ZPK 8
    dt[heeft_zpk_8 == 1, beeldvorming_hoofdcategorie := 'zpk_8']
    dt[, c('heeft_zpk_4', 'heeft_zpk_7', 'heeft_zpk_8',
           'mszzorgactiviteitomschrijving') := NULL]

    # Split beeldvorming into total, first, and second line
    cats_to_split <- unique(dt[complete.cases(beeldvorming_hoofdcategorie)]$beeldvorming_hoofdcategorie)
    eerste_lijn <- c('11', '20')

    for (cat in cats_to_split) {
      new_col <- paste0("heeft_", cat)
      new_col_eerste <- paste0("heeft_eerste_", cat)
      new_col_tweede <- paste0("heeft_tweede_", cat)

      dt[, (new_col) := as.integer(
        beeldvorming_hoofdcategorie == cat)]
      
      dt[, (new_col_eerste) := as.integer(
        beeldvorming_hoofdcategorie == cat &
          vektmszdeclaratietype %in% eerste_lijn)]

      dt[, (new_col_tweede) := as.integer(
        beeldvorming_hoofdcategorie == cat &
          !(vektmszdeclaratietype %in% eerste_lijn))]
    }

    # Keep MSZ activitieten only in the last 1000 days
    dt <- merge(dt_overlijden_with_matched[as.numeric(rinpersoon) %in% rinpersoon_set_chunk,
                                           .(rinpersoon, gbadatumoverlijden, cohort, died)],
                dt,
                by = 'rinpersoon', all.x = T, allow.cartesian = T)
    dt <- fast_to_date(dt, 'vektmszzorgactiviteitdatum')
    dt <- dt[vektmszzorgactiviteitdatum <= gbadatumoverlijden &
               vektmszzorgactiviteitdatum >= gbadatumoverlijden - 1000]

    dt[, c('vektmszzorgactiviteit', 'vektmszdeclaratietype', 'beeldvorming_hoofdcategorie',
           'gbadatumoverlijden', 'cohort', 'died') := NULL]
    dt_msz_activiteiten[[yr]] <- dt
    rm(dt)
    gc()
  }

  dt_msz_activiteiten <- rbindlist(dt_msz_activiteiten, use.names = T)
  cols_msz_gebruikt <- names(dt_msz_activiteiten)[grepl("^(heeft|gebruikt)",
                                                        names(dt_msz_activiteiten))]
  setnames(dt_msz_activiteiten, cols_msz_gebruikt, sub('^heeft_', 'n_', cols_msz_gebruikt))

  # Calculate the usage separately due to memory issues
  activiteiten_vars <- setdiff(names(dt_msz_activiteiten), c('rinpersoon', 'vektmszzorgactiviteitdatum'))
  if (length(activiteiten_vars) > 3) {
    print(length(activiteiten_vars))
    activiteiten_butches <- split(activiteiten_vars, cut(seq_along(activiteiten_vars),
                                                         breaks = 2, labels = F))
    
  } else {
    activiteiten_butches <- list(activiteiten_vars)
  }

  # Calculate usage in bins
  results <- vector('list', length(activiteiten_butches))
  for (activiteiten_var in seq_along(activiteiten_butches)){
    print(cat('Running batch: ', activiteiten_var))
    results[[activiteiten_var]] <- calculate_costs_by_bin_size(
      dt_msz_activiteiten[, c('rinpersoon', 'vektmszzorgactiviteitdatum',
                           activiteiten_butches[[activiteiten_var]]),
                       with = F],
      dt_overlijden_with_matched[as.numeric(rinpersoon) %in% rinpersoon_set_chunk],
      cost_columns = activiteiten_butches[[activiteiten_var]],
      cost_date_col = "vektmszzorgactiviteitdatum",
      bin_size = "months33",
      aggregate_groupby_cols = names(dt_overlijden_with_matched)
    )
  }

  rm(dt_msz_activiteiten)
  gc()

  dt_msz_activiteiten_monthly <- Reduce(function(x, y) merge(x, y, by = c(
    names(dt_overlijden_with_matched), 't'),
    all = T),
    results)
  rm(results)
  gc()

  # Save
  setindex(dt_msz_activiteiten_monthly, NULL)
  arrow::write_parquet(dt_msz_activiteiten_monthly,
                       glue::glue("./data/processed/msz_activiteiten_monthly.parquet")) #_{i}
  rm(dt_msz_activiteiten_monthly)
  gc()
}

rm(dt_overlijden_with_matched)
gc()

####
# Create a list of operaties based on the code descriptions
# counts_list <- rio::import('./output/dbc_codes_heup_aaa.xlsx')
# setDT(counts_list)
# counts_list[, ':='(heup_heup_totaal = as.integer(substr(.id, 1, 10) == 'heeft_heup'),
#
#              heup_heup_operatie = as.integer(substr(.id, 1, 10) == 'heeft_heup' &
#                grepl('operatie|prothese', vektmszdbczorgproduct_naam, ignore.case = T)),
#
#              heup_heup_prothese = as.integer(substr(.id, 1, 10) == 'heeft_heup' &
#                grepl('prothese', vektmszdbczorgproduct_naam, ignore.case = T)),
#
#              heup_aaa = as.integer(substr(.id, 1, 9) == 'heeft_aaa'),
#
#              heup_aaa_kijkoperatie = as.integer(substr(.id, 1, 9) == 'heeft_aaa' &
#                                           grepl('kijkoperatie', vektmszdbczorgproduct_naam, ignore.case = T)),
#
#              heup_aaa_operatie = as.integer(substr(.id, 1, 9) == 'heeft_aaa' &
#                                           grepl('operatie', vektmszdbczorgproduct_naam, ignore.case = T) &
#                                           !(grepl('kijkoperatie', vektmszdbczorgproduct_naam, ignore.case = T)))
#              )]
#
# openxlsx::write.xlsx(list(
#   counts_list = counts_list
# ), file = './output/dbc_codes_heup_aaa_categorised.xlsx')
####
