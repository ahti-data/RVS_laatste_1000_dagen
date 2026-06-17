# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev
# Goal: Find the DBC codes in the last 1000 days
# Output: A list of DBC codes and frequencies
# Last edited: 18 March 2026

#### Initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(dplyr)

dt_overlijden_with_matched <- r_parquet_get_dt(
  "./data/raw/overlijden_with_matched.parquet")
rinpersoon_set <- unique(dt_overlijden_with_matched$rinpersoon)

# Load in filtered prestaties, chunk because of large size
n_chunks <- 50
rinpersoon_set_chunks <- split(rinpersoon_set, cut(seq_along(rinpersoon_set), 
                                                   n_chunks, labels = FALSE))

for (i in seq_along(rinpersoon_set_chunks)) {
  rinpersoon_set_chunk <- rinpersoon_set_chunks[[i]]
  print(glue("Currently processing chunk {i}/{n_chunks} for mszprestaties"))
  
  #### Find MSZ prestaties ####
  # Load the table of hip fracturies codes from Caren
  heup_chemo_codes <- rio::import(
    'H:/laatste_1000_dagen/data/external/Zorgproducten Tabel selectie laatste 1000 dagen.xlsx')
  heup_chemo_codes <- format_data(heup_chemo_codes, rin_num = F)
  heup_chemo_codes <- heup_chemo_codes[, .(
    vektmszdbczorgproduct = sprintf('%09d', zorgproductcode), 
    heeft_heupprothese = heupprothese,
    heeft_oncolgie_chemo = as.integer(immunotherapie == 1),
    heeft_oncolgie_immuno = as.integer(immunotherapie == 2))]
  
  zpk_codes <- rio::import('H:/Aanvullende gegevens/ReflijstZorgactiviteiten.xlsx')
  zpk_codes <- format_data(zpk_codes, rin_num = F)
  zpk_codes <- zpk_codes[, .(vektmszdeclaratiecode = sprintf('%06d', mszzorgactiviteit), 
                             heeft_zpk_4_7_11 = as.integer(zpkcode %in% c(4, 7:11)),
                             heeft_zpk_4 = as.integer(zpkcode == 4),
                             heeft_zpk_7 = as.integer(zpkcode == 7),
                             heeft_zpk_8 = as.integer(zpkcode == 8),
                             heeft_zpk_9 = as.integer(zpkcode == 9),
                             heeft_zpk_10 = as.integer(zpkcode == 10),
                             heeft_zpk_11 = as.integer(zpkcode == 11)
  )]
  
  dt_msz_prestatie <- data.table()
  for (yr in years) {
    print(glue("Currently processing {yr} for mszprestaties"))
    
    filepath <- get_newest_parquet_check(
      folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files",
      folder_g_parquet = "G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data/",
      folder_g_sav = "G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB",
      string_pattern_parquet = yr,
      string_pattern_sav = yr,
    )
    
    ds <- arrow::open_dataset(filepath)
    dt <- ds |>
      filter(RINPERSOON %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(cols_to_select_msz_prestatie)])) |>
      collect()
    dt <- format_data(dt)
    
    # Create specialist and diagnosis codes
    dt[, ':='(spec = substr(vektmszspecialismediagnosecombinatie, 1, 4),
              diag = substr(vektmszspecialismediagnosecombinatie, 12, 15))]
    
    # Keep only a subset
    dt[, ':='(
      heeft_aaa_0303_0406_totaal = as.integer(
        spec == '0303' & diag == '0406'),
      
      heeft_aaa_0303_0406_operatie = as.integer(
        spec == '0303' & diag == '0406' &
          vektmszdbczorgproduct %in% c(
            '099699054', '099699055', '099699088', 
            '099699104', '099699112', '099699113')),
      
      heeft_aaa_0328_3320_totaal = as.integer(
        spec == '0328' & diag == '3320'),
      
      heeft_heup_0303_0218_totaal = as.integer(
        spec == '0303' & diag == '0218'),
      
      heeft_heup_0303_0218_operatie = as.integer(
        spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038',
          '199299043', '199299044'
        )),
      
      heeft_heup_0303_0218_prothese = as.integer(
        spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038'
        )),
      
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
          '199299043', '199299044'
        )),
      
      heeft_heup_0305_3019_prothese = as.integer(
        spec == '0305' & diag == '3019' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038'
        )),
      
      heeft_heup_0305_3020_totaal = as.integer(
        spec == '0305' & diag == '3020'),
      
      heeft_heup_0305_3020_operatie = as.integer(
        spec == '0305' & diag == '3020' & vektmszdbczorgproduct %in% c(
          '199299052', '199299053', '199299054'
        )),
      
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
    
    dt <- merge(dt, heup_chemo_codes, by = 'vektmszdbczorgproduct', all.x = T)
    dt <- merge(dt, zpk_codes, by = 'vektmszdeclaratiecode', all.x = T)
    
    cat('\n Share of missing zpk_codes: ', 
        mean(ifelse(is.na(dt$heeft_zpk_4_7_11), 1, 0)), '\n')
    
    # Create an indicator of first line diagnostic
    dt[, heeft_eerstelijn := as.integer(vektmszdeclaratietype %in% c('11', '20'))]
    cols_zpk <- names(zpk_codes)[grepl("^(heeft_)", names(zpk_codes))]
    cols_zpk_eerstelijnsdiagnostiek <- sub('heeft_', 'heeft_eerstelijn_', 
                                           cols_zpk)
    dt[, (cols_zpk_eerstelijnsdiagnostiek) := lapply(.SD, function(x) 
      x * heeft_eerstelijn), 
      .SDcols = cols_zpk]
    
    # Create an indicator for overig tweedelijn diagnostic
    dt[, heeft_tweedelijn := as.integer(!(vektmszdeclaratietype %in% c('11', '20')))]
    cols_zpk_overig_tweedelijn <- sub('heeft_', 'heeft_overig_tweedelijn_', 
                                      cols_zpk)
    dt[, (cols_zpk_overig_tweedelijn) := lapply(.SD, function(x) 
      x * heeft_tweedelijn), 
      .SDcols = cols_zpk]
    
    # Delete unused columns
    dt[, c(cols_zpk, 
           'vektmszspecialismediagnosecombinatie', 'vektmszdbczorgproduct', 
           'vektmszdeclaratiecode', 'vektmszdeclaratietype',
           'heeft_eerstelijn', 'heeft_tweedelijn', 'diag', 'spec') := NULL]
    
    # Pre-select before merging for memory reduction
    cols_to_keep <- names(dt)[grepl("^(heeft_)", names(dt))]
    dt[, has_any_code := rowSums(.SD, na.rm = T), .SDcols = cols_to_keep]
    dt <- dt[has_any_code > 0][, has_any_code := NULL]
    
    # Keep DBC codes only in the last 1000 days 
    dt <- merge(dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set_chunk, 
                                           .(rinpersoon, gbadatumoverlijden)],
                dt, 
                by = 'rinpersoon', all.x = T, allow.cartesian = T)
    dt[, vektmszbegindatumprest := as.Date(vektmszbegindatumprest, format = "%Y%m%d")]
    dt <- dt[vektmszbegindatumprest <= gbadatumoverlijden & 
               vektmszbegindatumprest >= gbadatumoverlijden - 1000]
    dt[, gbadatumoverlijden := NULL]
    
    dt_msz_prestatie <- rbindlist(list(dt_msz_prestatie, dt), use.names = T)
    rm(dt)
    gc()
  }
  
  # Create columns of costs per condition
  cols_msz_gebruikt <- names(dt_msz_prestatie)[grepl("^(heeft|gebruikt)", 
                                                     names(dt_msz_prestatie))]
  cols_msz_kosten <- sub('heeft_', 'kosten_', cols_msz_gebruikt)
  
  dt_msz_prestatie[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]
  dt_msz_prestatie[, (cols_msz_kosten) := lapply(.SD, function(x) 
    x * vektmszvergoedbedragzvw), 
    .SDcols = cols_msz_gebruikt]
  
  dt_msz_prestatie[, c(cols_msz_gebruikt, 'vektmszvergoedbedragzvw') := NULL]
  print(setdiff(names(dt_msz_prestatie), cols_msz_kosten))
  gc()
  
  # Calculate the costs separately due to memory issues
  costs <- setdiff(names(dt_msz_prestatie), c('rinpersoon', 'vektmszbegindatumprest'))
  cost_butches <- split(costs, cut(seq_along(costs), breaks = 3, labels = F))
  
  # Calculate costs in bins
  results <- vector('list', length(cost_butches)) 
  for (costs_i in seq_along(cost_butches)){
    cat('Running batch: ', costs_i)
    results[[costs_i]] <- calculate_costs_by_bin_size(
      dt_msz_prestatie[, c('rinpersoon', 'vektmszbegindatumprest', 
                           cost_butches[[costs_i]]),
                       with = F],
      dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set_chunk],
      cost_columns = cost_butches[[costs_i]],
      cost_date_col = "vektmszbegindatumprest",
      bin_size = "months33"
    )
  }
  
  dt_msz_prestatie_monthly <- Reduce(function(x, y) merge(x, y, by = c(
    'rinpersoon', 'gbadatumoverlijden', 't', 'cohort', 'died', 'doodsoorzaak'),
    all = T),
    results)
  rm(results)
  gc()
  
  # Create usage variables
  dt_msz_prestatie_monthly[, (cols_msz_gebruikt) := lapply(.SD, function(x) 
    as.integer(x > 0)),
    .SDcols = cols_msz_kosten
  ]
  
  # Save
  setindex(dt_msz_prestatie_monthly, NULL)
  arrow::write_parquet(dt_msz_prestatie_monthly, 
                       glue::glue("./data/raw/msz_prestatie_1000_dagen_{i}.parquet"))
  rm(dt_msz_prestatie_monthly, dt_msz_prestatie)
  gc()
  
  
  #### Find MSZ activities ####
  setnames(zpk_codes, 'vektmszdeclaratiecode', 'vektmszzorgactiviteit')
  dt_msz_activiteiten <- data.table()
  for (yr in years) {
    print(glue("Currently processing {yr} for mszactiviteiten"))
    
    filepath <- get_newest_parquet_check(
      folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZActiviteiten/parquet_files",
      folder_g_parquet = "G:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/GECONVERTEERDE DATA/",
      folder_g_sav = "G:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB",
      string_pattern_parquet = yr,
      string_pattern_sav = yr,
    )
    
    ds <- arrow::open_dataset(filepath)
    dt <- ds |>
      filter(RINPERSOON %in% rinpersoon_set_chunk) |>
      select(all_of(names(ds)[
        tolower(names(ds)) %in% tolower(cols_to_select_msz_activiteiten)])) |>
      collect()
    dt <- format_data(dt)
    
    # Merge the diagnostic codes
    dt <- merge(dt, zpk_codes, all.x = T, by = 'vektmszzorgactiviteit')
    dt[, vektmszzorgactiviteit := NULL]
    
    cat('\n Share of missing zpk_codes: ', 
        mean(ifelse(is.na(dt$heeft_zpk_4_7_11), 1, 0)), '\n')
    
    # Keep only diagnostic activities
    dt <- dt[heeft_zpk_4_7_11 == 1]
    
    # Rename
    cols_tweedelijnsdiagnostiek <- names(zpk_codes)[grepl("^(heeft_)", 
                                                          names(zpk_codes))]
    setnames(dt, cols_tweedelijnsdiagnostiek,
             sub('^heeft_', 'heeft_tweedelijn_', cols_tweedelijnsdiagnostiek))
    
    # Keep MSZ activitieten only in the last 1000 days 
    dt <- merge(dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set_chunk, 
                                           .(rinpersoon, gbadatumoverlijden)],
                dt, 
                by = 'rinpersoon', all.x = T, allow.cartesian = T)
    dt[, vektmszzorgactiviteitdatum := as.Date(vektmszzorgactiviteitdatum, format = "%Y%m%d")]
    dt <- dt[vektmszzorgactiviteitdatum <= gbadatumoverlijden & 
               vektmszzorgactiviteitdatum >= gbadatumoverlijden - 1000]
    dt[, gbadatumoverlijden := NULL]
    
    dt_msz_activiteiten <- rbindlist(list(dt_msz_activiteiten, dt), use.names = T)
    rm(dt)
    gc()
  }
  
  # Create columns of costs per condition
  cols_msz_gebruikt <- names(dt_msz_activiteiten)[grepl("^(heeft|gebruikt)", 
                                                        names(dt_msz_activiteiten))]
  print(setdiff(names(dt_msz_activiteiten), cols_msz_gebruikt))
  gc()
  
  # Calculate usage in bins
  dt_msz_activiteiten_monthly <- calculate_costs_by_bin_size(
    dt_msz_activiteiten,
    dt_overlijden_with_matched[rinpersoon %in% rinpersoon_set_chunk],
    cost_columns = cols_msz_gebruikt,
    cost_date_col = "vektmszzorgactiviteitdatum",
    bin_size = "months33"
  )
  gc()
  
  # Convert to 0 and 1
  dt_msz_activiteiten_monthly[, (cols_msz_gebruikt) := lapply(.SD, function(x) 
    as.integer(x > 0)),
    .SDcols = cols_msz_gebruikt
  ]
  
  # Save
  setindex(dt_msz_activiteiten_monthly, NULL)
  arrow::write_parquet(dt_msz_activiteiten_monthly, 
                       glue::glue("./data/raw/msz_activiteiten_1000_dagen_{i}.parquet"))
  rm(dt_msz_activiteiten_monthly, dt_msz_activiteiten)
  gc()
}
rm(dt_overlijden_with_matched)
gc()

# Bind all datasets
con <- DBI::dbConnect(duckdb::duckdb())
for (dataset in c('msz_prestatie_1000_dagen', 'msz_activiteiten_1000_dagen')){
  out_file <- glue("./data/processed/{dataset}.parquet")
  
  DBI::dbExecute(
    con,
    glue("
         COPY (
         SELECT *
         FROM read_parquet('./data/raw/{dataset}_*.parquet', union_by_name = true)
         )
         TO '{out_file}' 
        (FORMAT PARQUET, COMPRESSION ZSTD)  
         ")
  )
}

for (dataset in c('msz_prestatie_1000_dagen', 'msz_activiteiten_1000_dagen')){
  print(dataset)
  for (i in seq_along(rinpersoon_set_chunks)){
    print(i)
    unlink(glue("./data/raw/{dataset}_{i}.parquet"))
  }
}


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

