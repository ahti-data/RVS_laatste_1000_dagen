# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Find the DBC codes in the last 1000 days
# Output: A list of DBC codes and frequencies
# Last edited: 20 April 2026

#### Initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(dplyr)

dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched_add_demog.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)

# No need to use chunks with heavy server, if chuncks are returned, return _{i} below
# Load in filtered prestaties, chunk because of large size
#n_chunks <- 1
#rinpersoon_set_chunks <- split(rinpersoon_set, cut(seq_along(rinpersoon_set), 
#                                                   n_chunks, labels = FALSE))

#for (i in seq_along(rinpersoon_set_chunks)) {
#  rinpersoon_set_chunk <- rinpersoon_set_chunks[[i]]
#  print(glue("Currently processing chunk {i}/{n_chunks} for mszprestaties"))

for (i in 1) {
  rinpersoon_set_chunk <- rinpersoon_set
  
  #### Find MSZ prestaties ####
  # Load the table of hip fracturies codes from Caren
  #heup_chemo_codes <- rio::import(
  #  './data/external/Zorgproducten Tabel selectie laatste 1000 dagen.xlsx')
  #heup_chemo_codes <- format_data(heup_chemo_codes, rin_num = F)
  #heup_chemo_codes <- heup_chemo_codes[, .(
  #  vektmszdbczorgproduct = sprintf('%09d', zorgproductcode), 
  #  heeft_heupprothese = heupprothese,
  #  heeft_oncolgie_chemo = as.integer(immunotherapie == 1),
  #  heeft_oncolgie_immuno = as.integer(immunotherapie == 2))]
  
  zpk_codes <- rio::import("K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods")
  zpk_codes <- format_data(zpk_codes, rin_num = F)
  zpk_codes <- zpk_codes[, .(vektmszdeclaratiecode = sprintf('%06d', mszzorgactiviteit), 
                             heeft_zpk_4_7_11 = as.integer(zpkcode %in% c(4, 7:11)),
                             heeft_zpk_4 = as.integer(zpkcode == 4),
                             heeft_zpk_5 = as.integer(zpkcode == 5),
                             heeft_zpk_7 = as.integer(zpkcode == 7),
                             heeft_zpk_8 = as.integer(zpkcode == 8)
                             #heeft_zpk_9 = as.integer(zpkcode == 9),
                             #heeft_zpk_10 = as.integer(zpkcode == 10),
                             #heeft_zpk_11 = as.integer(zpkcode == 11)
  )]
  
  code_categories <- rio::import('./data/external/Zorgproducten_met_categorie.xlsx')
  code_categories <- format_data(code_categories, rin_num = F)
  code_categories <- code_categories[, .(vektmszdbczorgproduct = zorgproductcode,
                                         categorie = categorie,
                                         heeft_ambulant = as.integer(categorie == 'Ambulant'),
                                         heeft_dag = as.integer(categorie == 'Dag'),
                                         heeft_dag_klin = as.integer(categorie == 'Dag/ Klin'),
                                         heeft_kind = as.integer(categorie == 'Kind'),
                                         heeft_klinische_opname = as.integer(categorie == 'Klinische opname'),
                                         heeft_operatie_alle = as.integer(categorie == 'Operatie alle'),
                                         heeft_overig = as.integer(categorie == 'Overig'),
                                         heeft_overig_nieuwv = as.integer(categorie == 'Overig_Nieuwv'),
                                         heeft_polikliniek_consult = as.integer(categorie == 'Polikliniek/Consult'),
                                         heeft_uitval_standaard = as.integer(categorie == 'Uitval standaard'))]
  
  dt_codes <- list()
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
      filter(as.numeric(RINPERSOON) %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(cols_to_select_msz_prestatie)])) |>
      collect()
    dt <- format_data(dt)
    
    # Create specialist and diagnosis codes
    dt[, ':='(spec = substr(vektmszspecialismediagnosecombinatie, 1, 4),
              diag = substr(vektmszspecialismediagnosecombinatie, 12, 15))]
    
    # Merge names
    dbc_names <- rio::import(
      'K:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/codelijst variabeleVEKTMSZDBCZorgproduct.xlsx')
    names(dbc_names) <- c('vektmszdbczorgproduct', 'vektmszdbczorgproduct_naam')
    dbc_names <- format_data(dbc_names, rin_num = F)
    dbc_names[, vektmszdbczorgproduct := gsub('"', '', vektmszdbczorgproduct)]
    dt <- merge(dt, dbc_names, all.x = T, by = c('vektmszdbczorgproduct'))
    
    # Keep only a subset
    dt[, ':='(
      heeft_aaa_totaal = as.integer(
        spec == '0303' & diag == '0406'),
      
      heeft_aaa_kijkoperatie = as.integer(
        spec == '0303' & diag == '0406' & 
          grepl('kijkoperatie', vektmszdbczorgproduct_naam, ignore.case = T)),
      
      heeft_aaa_operatie = as.integer(
        spec == '0303' & diag == '0406' &
          grepl('operatie', vektmszdbczorgproduct_naam, ignore.case = T) &
          !(grepl('kijkoperatie', vektmszdbczorgproduct_naam, ignore.case = T))),
      
      #heeft_aaa_0328_3320_totaal = as.integer(
      #  spec == '0328' & diag == '3320'),
      
      heeft_heup_totaal = as.integer(
        (spec == '0303' & diag == '0218') |
          (spec == '0303' & diag == '0219') |
          (spec == '0305' & diag == '3019') |
          (spec == '0305' & diag == '3020')),
      
      heeft_heup_operatie = as.integer(
        (spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038',
          '199299043', '199299044')) |
          
          (spec == '0303' & diag == '0219' & vektmszdbczorgproduct %in% c(
            '199299052', '199299053', '199299054')) |
          
          (spec == '0305' & diag == '3019' & vektmszdbczorgproduct %in% c(
            '199299009', '199299025', '199299026', '199299037', '199299038',
            '199299043', '199299044')) |
          
          (spec == '0305' & diag == '3020' & vektmszdbczorgproduct %in% c(
            '199299052', '199299053', '199299054'))),
      
      heeft_heup_prothese = as.integer(
        (spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038')) |
          
          (spec == '0305' & diag == '3019' & vektmszdbczorgproduct %in% c(
            '199299009', '199299025', '199299026', '199299037', '199299038'))),
      
      heeft_heup_0303_0218_totaal = as.integer(
        spec == '0303' & diag == '0218'),
      
      heeft_heup_0303_0218_operatie = as.integer(
        spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038',
          '199299043', '199299044')),
      
      heeft_heup_0303_0218_prothese = as.integer(
        spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038')),
      
      heeft_heup_0303_0219_totaal = as.integer(
        spec == '0303' & diag == '0219'),
      
      heeft_heup_0303_0219_operatie = as.integer(
        spec == '0303' & diag == '0219' & vektmszdbczorgproduct %in% c(
          '199299052', '199299053', '199299054')),
      
      heeft_heup_0305_3019_totaal = as.integer(
        spec == '0305' & diag == '3019'),
      
      heeft_heup_0305_3019_operatie = as.integer(
        spec == '0305' & diag == '3019' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038',
          '199299043', '199299044')),
      
      heeft_heup_0305_3019_prothese = as.integer(
        spec == '0305' & diag == '3019' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038')),
      
      heeft_heup_0305_3020_totaal = as.integer(
        spec == '0305' & diag == '3020'),
      
      heeft_heup_0305_3020_operatie = as.integer(
        spec == '0305' & diag == '3020' & vektmszdbczorgproduct %in% c(
          '199299052', '199299053', '199299054')),
      
      # delirium_0329 = as.integer(
      #  spec == '0329' & diag %in% '0002'),
      # 
      # delirium_0313 = as.integer(
      #  spec == '0313' & diag %in% '0092'),
      # 
      # delirium_0335 = as.integer(
      #  spec == '0335' & diag %in% '0243'),
      
      heeft_add_on_ic = as.integer(
        vektmszdeclaratietype == '15'))]
    
    #dt <- merge(dt, heup_chemo_codes, by = 'vektmszdbczorgproduct', all.x = T)
    #dt <- merge(dt, zpk_codes, by = 'vektmszdeclaratiecode', all.x = T)
    dt <- merge(dt, code_categories, by = 'vektmszdbczorgproduct', all.x = T)
    
    # cat('\n Share of missing zpk_codes: ', 
    #     mean(ifelse(is.na(dt[vektmszdbczorgproduct == '999999999']$heeft_zpk_4_7_11), 1, 0)), '\n')
    # 
    # cat('\n Share of missing zpk_codes: ', 
    #     mean(ifelse(is.na(dt[vektmszdbczorgproduct != '999999999']$heeft_zpk_4_7_11), 1, 0)), '\n')
    
    cat('\n Share of missing categories: ', 
        mean(ifelse(is.na(dt[vektmszdbczorgproduct != '999999999']$categorie), 1, 0)), '\n')
    
    # # Create an indicator of first line diagnostic
    # dt[, heeft_eerstelijn := as.integer(vektmszdeclaratietype %in% c('11', '20'))]
    # cols_zpk <- names(zpk_codes)[grepl("^(heeft_)", names(zpk_codes))]
    # cols_zpk_eerstelijnsdiagnostiek <- sub('heeft_', 'heeft_eerstelijn_', 
    #                                        cols_zpk)
    # dt[, (cols_zpk_eerstelijnsdiagnostiek) := lapply(.SD, function(x) 
    #   x * heeft_eerstelijn), 
    #   .SDcols = cols_zpk]
    # 
    # # Create an indicator for overig tweedelijn diagnostic
    # dt[, heeft_tweedelijn := as.integer(!(vektmszdeclaratietype %in% c('11', '20')))]
    # cols_zpk_overig_tweedelijn <- sub('heeft_', 'heeft_overig_tweedelijn_', 
    #                                   cols_zpk)
    # dt[, (cols_zpk_overig_tweedelijn) := lapply(.SD, function(x) 
    #   x * heeft_tweedelijn), 
    #   .SDcols = cols_zpk]
    
    dt[, heeft_ozp_overig := as.integer(vektmszdeclaratietype %in% 
                                          c('10', '11', '12', '13', '14', '16',
                                            '17', '20', '25', '30'))]
    
    # Delete unused columns
    #dt[, cols_zpk := NULL]
    dt[, c('vektmszspecialismediagnosecombinatie', 'vektmszdbczorgproduct_naam',
           'vektmszdeclaratiecode', 'vektmszdeclaratietype',
           'heeft_eerstelijn', 'heeft_tweedelijn', 'diag', 'spec') := NULL]
    
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
               vektmszbegindatumprest >= gbadatumoverlijden - 1000]
    #dt[, gbadatumoverlijden := NULL]
    
    # Save the codes for identifying the counts of top 20 codes for operatie
    dt_codes[[yr]] <- dt[heeft_operatie_alle == 1, 
                         .(rinpersoon, gbadatumoverlijden, cohort, died, categorie,
                           vektmszdbczorgproduct, vektmszbegindatumprest, vektmszvergoedbedragzvw,
                           vektmszinstellingprest)]
    
    dt[, c('vektmszdbczorgproduct', 'cohort', 'died') := NULL]
    dt_msz_prestatie[[yr]] <- dt
    rm(dt)
    gc()
  }
  
  # Make counts of the top 20 codes in the last 30 days and 1000 days
  dt_codes <- rbindlist(dt_codes, use.names = T)
  dt_codes[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]
  
  
  #### Top 20 operation codes in the last 1000 days ####
  dt_top_20_operatie_codes_1000 <- dt_codes[
    , .(n_totaal_gebruikers = uniqueN(rinpersoon),
        n_totaal_declaraties = .N,
        sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm = T)), 
    by = .(vektmszdbczorgproduct, cohort, died)][order(cohort, died, -n_totaal_gebruikers)]
  dt_top_20_operatie_codes_1000[, sum_per_gebruiker := sum_totaal_groep / n_totaal_gebruikers]
  dt_top_20_operatie_codes_1000[, period := 'laatste_1000_dagen']
  
  top_20_codes <- dt_top_20_operatie_codes_1000[died == 'Overleden'][
    order(cohort, -n_totaal_gebruikers)][
      , head(.SD, 20), by = cohort][, .(vektmszdbczorgproduct, cohort)]
  
  # Find the prevalence and costs of the same 20 codes in the last 30 days
  dt_top_20_operatie_codes_1000_30 <- dt_codes[
    vektmszbegindatumprest >= gbadatumoverlijden - 30 &
      vektmszdbczorgproduct %in% 
      unique(top_20_codes$vektmszdbczorgproduct)
    , .(n_totaal_gebruikers = uniqueN(rinpersoon),
        n_totaal_declaraties = .N,
        sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm = T)), 
    by = .(vektmszdbczorgproduct, cohort, died)][order(cohort, died, -n_totaal_gebruikers)]
  dt_top_20_operatie_codes_1000_30[, sum_per_gebruiker := sum_totaal_groep / n_totaal_gebruikers]
  dt_top_20_operatie_codes_1000_30[, period := 'laatste_30_dagen']
  
  dt_top_20_operatie_codes_1000 <- rbindlist(list(dt_top_20_operatie_codes_1000,
                                                  dt_top_20_operatie_codes_1000_30),
                                             use.names = T)
  rm(dt_top_20_operatie_codes_1000_30)
  
  # Keep only top 20 codes among deceased people 
  dt_top_20_operatie_codes_1000 <- merge(top_20_codes, dt_top_20_operatie_codes_1000,
                                         by = c('vektmszdbczorgproduct', 'cohort'),
                                         all.x = T)
  rm(top_20_codes)
  
  # Add for CBS checks the number of hospitals and the share of the costs of the largest hospital
  institutions <- dt_codes[, .(n_instellingen = uniqueN(vektmszinstellingprest)), 
                           by = .(vektmszdbczorgproduct, cohort)]
  code_institutions <- dt_codes[, .(kosten_instellingen = sum(vektmszvergoedbedragzvw, na.rm = T)), 
                                by = .(vektmszdbczorgproduct, cohort, vektmszinstellingprest)]
  code_institutions <- code_institutions[, share_of_costs_largest_instellingen := 
                                           kosten_instellingen / sum(kosten_instellingen, na.rm = T),
                                         by = .(vektmszdbczorgproduct, cohort)][
                                           order(vektmszdbczorgproduct, cohort, 
                                                 -share_of_costs_largest_instellingen)][
                                                   , head(.SD, 1), by = .(vektmszdbczorgproduct, cohort)][
                                                     , c('kosten_instellingen', 'vektmszinstellingprest') := NULL
                                                   ]
  
  # Merge these checks
  dt_top_20_operatie_codes_1000 <- merge(dt_top_20_operatie_codes_1000, institutions,
                                         by = c('vektmszdbczorgproduct', 'cohort'),
                                         all.x = T)
  dt_top_20_operatie_codes_1000 <- merge(dt_top_20_operatie_codes_1000, code_institutions,
                                         by = c('vektmszdbczorgproduct', 'cohort'),
                                         all.x = T)
  setorder(dt_top_20_operatie_codes_1000, cohort, died, period, -n_totaal_gebruikers)
  dt_top_20_operatie_codes_1000 <- dt_top_20_operatie_codes_1000[n_totaal_gebruikers >= 10]
  
  # All codes should be offered in at least 3 hospitals and not a single 
  # hospital can contribute more than 50% costs
  stopifnot(all(dt_top_20_operatie_codes_1000$n_instellingen >= 3))
  stopifnot(all(dt_top_20_operatie_codes_1000$share_of_costs_largest_instellingen < 0.5))
  
  
  #### Top 20 operation codes in the last 30 days ####
  dt_top_20_operatie_codes_30 <- dt_codes[vektmszbegindatumprest >= gbadatumoverlijden - 30
                                          , .(n_totaal_gebruikers = uniqueN(rinpersoon),
                                              n_totaal_declaraties = .N,
                                              sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm = T)), 
                                          by = .(vektmszdbczorgproduct, cohort, died)][order(cohort, died, -n_totaal_gebruikers)]
  dt_top_20_operatie_codes_30[, sum_per_gebruiker := sum_totaal_groep / n_totaal_gebruikers]
  dt_top_20_operatie_codes_30[, period := 'laatste_30_dagen']
  
  top_20_codes <- dt_top_20_operatie_codes_30[died == 'Overleden'][
    order(cohort, -n_totaal_gebruikers)][
      , head(.SD, 20), by = cohort][, .(vektmszdbczorgproduct, cohort)]
  
  # Find the prevalence and costs of the same 20 codes in the last 1000 days
  dt_top_20_operatie_codes_30_1000 <- dt_codes[vektmszdbczorgproduct %in% 
                                                 unique(top_20_codes$vektmszdbczorgproduct)
                                               , .(n_totaal_gebruikers = uniqueN(rinpersoon),
                                                   n_totaal_declaraties = .N,
                                                   sum_totaal_groep = sum(vektmszvergoedbedragzvw, na.rm = T)), 
                                               by = .(vektmszdbczorgproduct, cohort, died)][order(cohort, died, -n_totaal_gebruikers)]
  dt_top_20_operatie_codes_30_1000[, sum_per_gebruiker := sum_totaal_groep / n_totaal_gebruikers]
  dt_top_20_operatie_codes_30_1000[, period := 'laatste_1000_dagen']
  
  dt_top_20_operatie_codes_30 <- rbindlist(list(dt_top_20_operatie_codes_30,
                                                dt_top_20_operatie_codes_30_1000),
                                           use.names = T)
  rm(dt_top_20_operatie_codes_30_1000)
  
  # Keep only top 20 codes among deceased people 
  dt_top_20_operatie_codes_30 <- merge(top_20_codes, dt_top_20_operatie_codes_30,
                                       by = c('vektmszdbczorgproduct', 'cohort'),
                                       all.x = T)
  rm(top_20_codes)
  
  # Merge these checks
  dt_top_20_operatie_codes_30 <- merge(dt_top_20_operatie_codes_30, institutions,
                                       by = c('vektmszdbczorgproduct', 'cohort'),
                                       all.x = T)
  dt_top_20_operatie_codes_30 <- merge(dt_top_20_operatie_codes_30, code_institutions,
                                       by = c('vektmszdbczorgproduct', 'cohort'),
                                       all.x = T)
  setorder(dt_top_20_operatie_codes_30, cohort, died, period, -n_totaal_gebruikers)
  dt_top_20_operatie_codes_30 <- dt_top_20_operatie_codes_30[n_totaal_gebruikers >= 10]
  
  # All codes should be offered in at least 3 hospitals and not a single 
  # hospital can contribute more than 50% costs
  stopifnot(all(dt_top_20_operatie_codes_30$n_instellingen >= 3))
  stopifnot(all(dt_top_20_operatie_codes_30$share_of_costs_largest_instellingen < 0.5))
  
  # Save top 20 codes
  openxlsx::write.xlsx(list(
    top_20_codes_1000_dagen = dt_top_20_operatie_codes_1000,
    top_20_codes_30_dagen = dt_top_20_operatie_codes_30
  ), file = './output/iteration_2/top_20_codes_operatie.xlsx')
  rm(dt_codes, dt_top_20_operatie_codes_1000, dt_top_20_operatie_codes_30,
     institutions, code_institutions)
  
  dt_msz_prestatie <- rbindlist(dt_msz_prestatie, use.names = T)
  
  # Save the first date of diagnosis 
  for (outcome in c('heeft_aaa_totaal', 'heeft_heup_totaal')){
    setorder(dt_msz_prestatie, rinpersoon, vektmszbegindatumprest)
    dt_msz_prestatie_first <- dt_msz_prestatie[
      get(outcome) == 1, .(rinpersoon, gbadatumoverlijden, vektmszbegindatumprest)]
    dt_msz_prestatie_first <- dt_msz_prestatie_first[, .SD[1], by = .(rinpersoon, gbadatumoverlijden)]
    
    setindex(dt_msz_prestatie_first, NULL)
    arrow::write_parquet(dt_msz_prestatie_first, 
                         glue::glue("./data/raw/msz_prestatie_first_{outcome}.parquet")) # _{i}
    rm(dt_msz_prestatie_first)
    gc()
  }
  
  dt_msz_prestatie[, gbadatumoverlijden := NULL]
  
  # Create columns of costs per condition
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
  setnames(zpk_codes, 'vektmszdeclaratiecode', 'vektmszzorgactiviteit')
  dt_codes <- list()
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
      filter(as.numeric(RINPERSOON) %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(cols_to_select_msz_activiteiten)])) |>
      collect()
    dt <- format_data(dt)
    
    # Rename
    #cols_tweedelijnsdiagnostiek <- names(zpk_codes)[grepl("^(heeft_)", 
    #                                                      names(zpk_codes))]
    #setnames(dt, cols_tweedelijnsdiagnostiek,
    #         sub('^heeft_', 'heeft_tweedelijn_', cols_tweedelijnsdiagnostiek))
    
    # Get ozp's 
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
      filter(as.numeric(RINPERSOON) %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% c('rinpersoon', 'vektmszinstellingprest', 
                                  'vektmszbegindatumprest', 'vektmszdeclaratiecode')])) |>
      collect()
    dt_prestatie <- format_data(dt_prestatie)
    
    dt_prestatie <- dt_prestatie[
      !substr(vektmszdeclaratiecode, 1, 2) %in% c("14", "15", "16", "17")
    ]
    
    # Synchronize
    dt_prestatie <- dt_prestatie[, .(rinpersoon, 
                                     vektmszzorgactiviteit = vektmszdeclaratiecode,
                                     vektmszinstellingza = vektmszinstellingprest, 
                                     vektmszzorgactiviteitdatum = vektmszbegindatumprest
    )]
    
    # Bind activitieten and ozp's from prestaties
    dt <- rbindlist(list(dt, dt_prestatie), use.names = T)
    rm(dt_prestatie)
    
    # Add the names of zorgproducten
    dbc_names <- rio::import(
      'K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods')
    dbc_names <- format_data(dbc_names, rin_num = F)
    dbc_names <- dbc_names[, .(vektmszzorgactiviteit = mszzorgactiviteit, 
                               mszzorgactiviteitomschrijving)]
    
    dt <- merge(dt, dbc_names, all.x = T, by = c('vektmszzorgactiviteit'))
    cat('\n Share of missing descriptions: ', 
        mean(ifelse(is.na(dt$mszzorgactiviteitomschrijving), 1, 0)), '\n')
    
    
    # Merge the diagnostic codes
    dt <- merge(dt, zpk_codes, all.x = T, by = 'vektmszzorgactiviteit')
    
    cat('\n Share of missing zpk_codes: ', 
        mean(ifelse(is.na(dt$heeft_zpk_4_7_11), 1, 0)), '\n')
    
    # Keep only diagnostic activities
    dt <- dt[heeft_zpk_4_7_11 == 1]
    dt[, heeft_zpk_4_7_11 := NULL]
    
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
    dt[, mszzorgactiviteitomschrijving := NULL]
    
    # Add ZPK 8
    dt[heeft_zpk_8 == 1, beeldvorming_hoofdcategorie := 'zpk_8']
    dt[, c('heeft_zpk_4', 'heeft_zpk_7', 'heeft_zpk_8') := NULL]
    
    # Create new varaibles
    cats_to_split <- unique(dt[complete.cases(beeldvorming_hoofdcategorie)]$beeldvorming_hoofdcategorie)
    for (cat in cats_to_split) {
      new_col_name <- paste0("heeft_", cat)
      dt[, (new_col_name) := 0]
      dt[beeldvorming_hoofdcategorie == cat, (new_col_name) := 1]
    }
    
    # Keep MSZ activitieten only in the last 1000 days 
    dt <- merge(dt_overlijden_with_matched[as.numeric(rinpersoon) %in% rinpersoon_set_chunk, 
                                           .(rinpersoon, gbadatumoverlijden, cohort, died)],
                dt, 
                by = 'rinpersoon', all.x = T, allow.cartesian = T)
    dt[, vektmszzorgactiviteitdatum := as.Date(vektmszzorgactiviteitdatum, format = "%Y%m%d")]
    dt <- dt[vektmszzorgactiviteitdatum <= gbadatumoverlijden & 
               vektmszzorgactiviteitdatum >= gbadatumoverlijden - 1000]
    
    # Save the codes for identifying the counts of top 20 codes 
    dt_codes[[yr]] <- dt[complete.cases(beeldvorming_hoofdcategorie), 
                         .(rinpersoon, gbadatumoverlijden, cohort, died,
                           beeldvorming_hoofdcategorie, vektmszzorgactiviteit, 
                           vektmszzorgactiviteitdatum, vektmszinstellingza)]
    
    dt[, c('vektmszzorgactiviteit', 'vektmszinstellingza', 'beeldvorming_hoofdcategorie',
           'gbadatumoverlijden', 'cohort', 'died') := NULL]
    dt_msz_activiteiten[[yr]] <- dt
    rm(dt)
    gc()
  }
  
  # Make counts of the top 20 codes in the last 30 days and 1000 days
  dt_codes <- rbindlist(dt_codes, use.names = T)
  
  
  #### Top 20 operation codes in the last 1000 days ####
  dt_top_20_activit_codes_1000 <- dt_codes[
    , .(n_totaal_gebruikers = uniqueN(rinpersoon),
        n_totaal_declaraties = .N), 
    by = .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort, died)][
      order(beeldvorming_hoofdcategorie, cohort, died, -n_totaal_gebruikers)]
  dt_top_20_activit_codes_1000[, period := 'laatste_1000_dagen']
  
  top_20_codes <- dt_top_20_activit_codes_1000[died == 'Overleden'][
    order(beeldvorming_hoofdcategorie, cohort, -n_totaal_gebruikers)][
      , head(.SD, 20), by = .(beeldvorming_hoofdcategorie, cohort)][
        , .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort)]
  
  # Find the prevalence and costs of the same 20 codes in the last 30 days
  dt_top_20_activit_codes_1000_30 <- dt_codes[
    vektmszzorgactiviteitdatum >= gbadatumoverlijden - 30 &
      vektmszzorgactiviteit %in% 
      unique(top_20_codes$vektmszzorgactiviteit)
    , .(n_totaal_gebruikers = uniqueN(rinpersoon),
        n_totaal_declaraties = .N), 
    by = .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort, died)][
      order(beeldvorming_hoofdcategorie, cohort, died, -n_totaal_gebruikers)]
  dt_top_20_activit_codes_1000_30[, period := 'laatste_30_dagen']
  
  dt_top_20_activit_codes_1000 <- rbindlist(list(dt_top_20_activit_codes_1000,
                                                 dt_top_20_activit_codes_1000_30),
                                            use.names = T)
  rm(dt_top_20_activit_codes_1000_30)
  
  # Keep only top 20 codes among deceased people 
  dt_top_20_activit_codes_1000 <- merge(top_20_codes, dt_top_20_activit_codes_1000,
                                        by = c('vektmszzorgactiviteit', 'beeldvorming_hoofdcategorie', 'cohort'),
                                        all.x = T)
  rm(top_20_codes)
  
  # Add for CBS checks the number of hospitals and the share of the costs of the largest hospital
  institutions <- dt_codes[, .(n_instellingen = uniqueN(vektmszinstellingza)), 
                           by = .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort)]
  
  # Merge these checks
  dt_top_20_activit_codes_1000 <- merge(dt_top_20_activit_codes_1000, institutions,
                                        by = c('vektmszzorgactiviteit', 'beeldvorming_hoofdcategorie', 'cohort'),
                                        all.x = T)
  setorder(dt_top_20_activit_codes_1000, beeldvorming_hoofdcategorie, cohort, died, period, -n_totaal_gebruikers)
  dt_top_20_activit_codes_1000 <- dt_top_20_activit_codes_1000[n_totaal_gebruikers >= 10]
  
  # All codes should be offered in at least 3 hospitals 
  dt_top_20_activit_codes_1000 <- dt_top_20_activit_codes_1000[n_instellingen >= 3]
  
  #### Top 20 operation codes in the last 30 days ####
  dt_top_20_activit_codes_30 <- dt_codes[vektmszzorgactiviteitdatum >= gbadatumoverlijden - 30
                                         , .(n_totaal_gebruikers = uniqueN(rinpersoon),
                                             n_totaal_declaraties = .N), 
                                         by = .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort, died)][
                                           order(beeldvorming_hoofdcategorie, cohort, died, -n_totaal_gebruikers)]
  dt_top_20_activit_codes_30[, period := 'laatste_30_dagen']
  
  top_20_codes <- dt_top_20_activit_codes_30[died == 'Overleden'][
    order(beeldvorming_hoofdcategorie, cohort, -n_totaal_gebruikers)][
      , head(.SD, 20), by = .(beeldvorming_hoofdcategorie, cohort)][
        , .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort)]
  
  # Find the prevalence and costs of the same 20 codes in the last 1000 days
  dt_top_20_activit_codes_30_1000 <- dt_codes[vektmszzorgactiviteit %in% 
                                                unique(top_20_codes$vektmszzorgactiviteit)
                                              , .(n_totaal_gebruikers = uniqueN(rinpersoon),
                                                  n_totaal_declaraties = .N), 
                                              by = .(vektmszzorgactiviteit, beeldvorming_hoofdcategorie, cohort, died)][
                                                order(beeldvorming_hoofdcategorie, cohort, died, -n_totaal_gebruikers)]
  dt_top_20_activit_codes_30_1000[, period := 'laatste_1000_dagen']
  
  dt_top_20_activit_codes_30 <- rbindlist(list(dt_top_20_activit_codes_30,
                                               dt_top_20_activit_codes_30_1000),
                                          use.names = T)
  rm(dt_top_20_activit_codes_30_1000)
  
  # Keep only top 20 codes among deceased people 
  dt_top_20_activit_codes_30 <- merge(top_20_codes, dt_top_20_activit_codes_30,
                                      by = c('vektmszzorgactiviteit', 'beeldvorming_hoofdcategorie', 'cohort'),
                                      all.x = T)
  rm(top_20_codes)
  
  # Merge these checks
  dt_top_20_activit_codes_30 <- merge(dt_top_20_activit_codes_30, institutions,
                                      by = c('vektmszzorgactiviteit', 'beeldvorming_hoofdcategorie', 'cohort'),
                                      all.x = T)
  setorder(dt_top_20_activit_codes_30, beeldvorming_hoofdcategorie, cohort, died, period, -n_totaal_gebruikers)
  dt_top_20_activit_codes_30 <- dt_top_20_activit_codes_30[n_totaal_gebruikers >= 10]
  
  # All codes should be offered in at least 3 hospitals 
  dt_top_20_activit_codes_30 <- dt_top_20_activit_codes_30[n_instellingen >= 3]
  
  # Save top 20 codes
  openxlsx::write.xlsx(list(
    top_20_codes_1000_dagen = dt_top_20_activit_codes_1000,
    top_20_codes_30_dagen = dt_top_20_activit_codes_30
  ), file = './output/iteration_2/top_20_codes_activit.xlsx')
  rm(dt_codes, dt_top_20_activit_codes_1000, dt_top_20_activit_codes_30,
     institutions)
  
  # Create columns of costs per condition
  dt_msz_activiteiten <- rbindlist(dt_msz_activiteiten, use.names = T)
  cols_msz_gebruikt <- names(dt_msz_activiteiten)[grepl("^(heeft|gebruikt)", 
                                                        names(dt_msz_activiteiten))]
  setnames(dt_msz_activiteiten, cols_msz_gebruikt, sub('^heeft_', 'n_', cols_msz_gebruikt))
  
  # Calculate the costs separately due to memory issues
  activiteiten_vars <- setdiff(names(dt_msz_activiteiten), c('rinpersoon', 'vektmszzorgactiviteitdatum'))
  activiteiten_butches <- split(activiteiten_vars, cut(seq_along(activiteiten_vars), 
                                                       breaks = 10, labels = F))
  
  # Calculate costs in bins
  results <- vector('list', length(activiteiten_butches)) 
  for (activiteiten_var in seq_along(activiteiten_butches)){
    cat('Running batch: ', activiteiten_var)
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
  
  # Convert to 0 and 1
  #dt_msz_activiteiten_monthly[, (cols_msz_gebruikt) := lapply(.SD, function(x) 
  #  as.integer(x > 0)),
  #  .SDcols = activiteiten_vars
  #]
  
  # Save
  setindex(dt_msz_activiteiten_monthly, NULL)
  arrow::write_parquet(dt_msz_activiteiten_monthly, 
                       glue::glue("./data/processed/msz_activiteiten_monthly.parquet")) #_{i}
  rm(dt_msz_activiteiten_monthly)
  gc()
}

rm(dt_overlijden_with_matched)
gc()

# Bind all datasets
dt_rin <- list()
for (outcome in c('heeft_aaa_totaal', 'heeft_heup_totaal')){
  print(outcome)
  data_list <- list()
  
  for (i in 1){ # :n_chunks
    df <- r_parquet_get_dt(glue::glue("./data/raw/msz_prestatie_first_{outcome}.parquet")) #_{i}
    data_list[[i]] <- df
    rm(df)
    gc()
  }
  data_list <- rbindlist(data_list, use.names = T)
  setindex(data_list, NULL)
  #arrow::write_parquet(data_list, 
  #                     glue::glue("./data/processed/msz_prestatie_first_{outcome}.parquet"))
  dt_rin[[outcome]] <- data_list[, .(rinpersoon)]
  rm(data_list)
  gc()
}

dt_rin <- rbindlist(dt_rin, use.names = T)
dt_rin <- unique(dt_rin)
setindex(dt_rin, NULL)
arrow::write_parquet(dt_rin, "./data/processed/msz_prestatie_first_rin.parquet")
rm(dt_rin)
gc()

# con <- DBI::dbConnect(duckdb::duckdb())
# for (dataset in c('msz_prestatie_monthly', 'msz_activiteiten_monthly')){
#   out_file <- glue("./data/processed/{dataset}.parquet")
#   
#   DBI::dbExecute(
#     con,
#     glue("
#          COPY (
#          SELECT *
#          FROM read_parquet('./data/processed/{dataset}_*.parquet', union_by_name = true)
#          )
#          TO '{out_file}' 
#         (FORMAT PARQUET, COMPRESSION ZSTD)  
#          ")
#   )
# }
# 
# for (dataset in c('msz_prestatie_monthly', 'msz_activiteiten_monthly')){
#   print(dataset)
#   for (i in 1:n_chunks){
#     print(i)
#     unlink(glue("./data/processed/{dataset}_{i}.parquet"))
#   }
# }


# # Save the aggregated counts of DBC codes
# dt_msz_agg <- merge(dt_msz, dt_overlijden_with_matched,
#                     by = c('rinpersoon', 'gbadatumoverlijden'),
#                     all.x = T,
#                     allow.cartesian = T)
# 
# # Add the names of zorgproducten
# dbc_names <- rio::import(
#   'K:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/codelijst variabeleVEKTMSZDBCZorgproduct.xlsx')
# names(dbc_names) <- c('vektmszdbczorgproduct', 'vektmszdbczorgproduct_naam')
# dbc_names <- format_data(dbc_names, rin_num = F)
# dbc_names[, vektmszdbczorgproduct := gsub('"', '', vektmszdbczorgproduct)]
# dt_msz_agg <- merge(dt_msz_agg, dbc_names, all.x = T, by = c('vektmszdbczorgproduct'))
# #dbc_names <- unique(dt_msz_agg[, .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, spec, diag)])
# #dbc_names <- dbc_names[vektmszdbczorgproduct != vektmszdbczorgproduct_naam]
# #dt_msz_agg$vektmszdbczorgproduct_naam <- NULL
# #dt_msz_agg <- merge(dt_msz_agg, dbc_names, all.x = T,
# #                    by = c('vektmszdbczorgproduct', 'spec', 'diag'))
# 
# abdom_aorta_0303 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                                  abdom_aorta_0303 == 1, .N,
#                                by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# abdom_aorta_0328 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                                  abdom_aorta_0328 == 1, .N,
#                                by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# opereren_van_heupfracturen_0303 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                                                 opereren_van_heupfracturen_0303 == 1, .N,
#                                               by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# opereren_van_heupfracturen_0305 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                                                 opereren_van_heupfracturen_0305 == 1, .N,
#                                               by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# opereren_van_heupfracturen_8418 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                                                 opereren_van_heupfracturen_8418 == 1, .N,
#                                               by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# opereren_van_heupfracturen_spec <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                                                 heeft_heupprothese == 1, .N,
#                                               by = .(spec, diag)][order(spec, diag, -N)]
# delirium_0329 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                               delirium_0329 == 1, .N,
#                             by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# delirium_0313 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                               delirium_0313 == 1, .N,
#                             by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# delirium_0335 <- dt_msz_agg[died == 'Overleden' & cohort == 2023 &
#                               delirium_0335 == 1, .N,
#                             by = .(vektmszdbczorgproduct, vektmszdbczorgproduct_naam, diag)][order(vektmszdbczorgproduct, diag, -N)]
# 
# # Save
# wb <- openxlsx::createWorkbook()
# openxlsx::addWorksheet(wb, 'abdom_aorta_0303')
# openxlsx::addWorksheet(wb, 'abdom_aorta_0328')
# openxlsx::addWorksheet(wb, 'opereren_van_heupfracturen_0303')
# openxlsx::addWorksheet(wb, 'opereren_van_heupfracturen_0305')
# openxlsx::addWorksheet(wb, 'opereren_van_heupfracturen_8418')
# openxlsx::addWorksheet(wb, 'opereren_van_heupfracturen_spec')
# openxlsx::addWorksheet(wb, 'delirium_0329')
# openxlsx::addWorksheet(wb, 'delirium_0313')
# openxlsx::addWorksheet(wb, 'delirium_0335')
# 
# openxlsx::writeData(wb, 'abdom_aorta_0303', abdom_aorta_0303)
# openxlsx::writeData(wb, 'abdom_aorta_0328', abdom_aorta_0328)
# openxlsx::writeData(wb, 'opereren_van_heupfracturen_0303', opereren_van_heupfracturen_0303)
# openxlsx::writeData(wb, 'opereren_van_heupfracturen_0305', opereren_van_heupfracturen_0305)
# openxlsx::writeData(wb, 'opereren_van_heupfracturen_8418', opereren_van_heupfracturen_8418)
# openxlsx::writeData(wb, 'opereren_van_heupfracturen_spec', opereren_van_heupfracturen_spec)
# openxlsx::writeData(wb, 'delirium_0329', delirium_0329)
# openxlsx::writeData(wb, 'delirium_0313', delirium_0313)
# openxlsx::writeData(wb, 'delirium_0335', delirium_0335)
# 
# openxlsx::saveWorkbook(wb, './output/vektmszdbczorgproduct.xlsx', overwrite = T)
# rm(dt_msz_agg)
# gc()

