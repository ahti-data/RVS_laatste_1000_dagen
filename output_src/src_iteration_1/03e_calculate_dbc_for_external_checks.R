#### Initialize ####
rm(list = ls())
gc()
source("./src/00_inputs.R")
library(dplyr)

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

for (yr in c(2017, 2019, 2023)) {
  print(glue("Currently processing {yr} for mszprestaties"))
  
  filepath <- get_newest_parquet_check(
    folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files",
    folder_g_parquet = "G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data/",
    folder_g_sav = "G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB",
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
  )
  
  if (yr > 2017){
    dt <- arrow::read_parquet(filepath, col_select = c(
      'RINPERSOON',  'VEKTMSZDeclaratietype',
      'VEKTMSZVergoedbedragZVW'
    ))
    dt <- format_data(dt)
    
    # Keep only a subset
    dt[, ':='(heeft_add_on_ic = as.integer(
      vektmszdeclaratietype == '15'))]
    dt[, kosten_add_on_ic := heeft_add_on_ic * as.numeric(vektmszvergoedbedragzvw)]
    dt[, ':='(vektmszdeclaratietype = NULL, vektmszvergoedbedragzvw = NULL)]
  } else {
  
  dt <- arrow::read_parquet(filepath, col_select = c(
    'RINPERSOON', 
    'VEKTMSZSpecialismeDiagnoseCombinatie', 
    'VEKTMSZDBCZorgproduct', 
    'VEKTMSZDeclaratiecode', 'VEKTMSZDeclaratietype',
    'VEKTMSZVergoedbedragZVW'
  ))
  dt <- format_data(dt)
  
  # Create specialist and diagnosis codes
  dt[, ':='(spec = substr(vektmszspecialismediagnosecombinatie, 1, 4),
            diag = substr(vektmszspecialismediagnosecombinatie, 12, 15))]
  dt$vektmszspecialismediagnosecombinatie <- NULL
  
    # Keep only a subset
    dt[, ':='(
      heeft_aaa = as.integer(
        (spec == '0303' & diag == '0406') | 
          (spec == '0328' & diag == '3320')),
      
      heeft_aaa_operatie = as.integer(
        spec == '0303' & diag %in% c('0406', '0405', '0403') &
          vektmszdbczorgproduct %in% c(
            '099699054', '099699055', '099699088', 
            '099699104', '099699112', '099699113')),
      
      
      heeft_heup_totaal = as.integer(
        (spec == '0303' & diag == '0218') |
          (spec == '0303' & diag == '0219') |
          (spec == '0305' & diag == '3019') |
          (spec == '0305' & diag == '3020')),
      
      heeft_heup_prothese = as.integer(
        (spec == '0303' & diag == '0218' & vektmszdbczorgproduct %in% c(
          '199299009', '199299025', '199299026', '199299037', '199299038')) |
          (spec == '0305' & diag == '3019' & vektmszdbczorgproduct %in% c(
            '199299009', '199299025', '199299026', '199299037', '199299038')
          )),
      
      heeft_add_on_ic = as.integer(
        vektmszdeclaratietype == '15'))]
    dt$vektmszdbczorgproduct <- NULL
    gc()
    
    dt <- merge(dt, zpk_codes, by = 'vektmszdeclaratiecode', all.x = T)
    dt$vektmszdeclaratiecode <- NULL
    gc()
    
    cat('\n Share of missing zpk_codes: ', 
        mean(ifelse(is.na(dt$heeft_zpk_4_7_11), 1, 0)), '\n')
    
    # Create an indicator of first line diagnostic
    #dt[, heeft_eerstelijn := as.integer(vektmszdeclaratietype %in% c('11', '20'))]
    dt[, heeft_eerstelijn := 1]
    cols_zpk <- names(zpk_codes)[grepl("^(heeft_)", names(zpk_codes))]
    cols_zpk_eerstelijnsdiagnostiek <- sub('heeft_', 'heeft_eerstelijn_', 
                                           cols_zpk)
    dt[, (cols_zpk_eerstelijnsdiagnostiek) := lapply(.SD, function(x) 
      x * heeft_eerstelijn), 
      .SDcols = cols_zpk]
    
    # Create an indicator for overig tweedelijn diagnostic
    dt[, heeft_overig_tweedelijn_zpk_8_10_11 := as.integer(!(vektmszdeclaratietype %in% c('11', '20')) &
                                                             (heeft_zpk_8 == 1 | 
                                                                heeft_zpk_10 == 1 |
                                                                heeft_zpk_11 == 1))]
    
    
    # cols_zpk_overig_tweedelijn <- sub('heeft_', 'heeft_overig_tweedelijn_', 
    #                                   cols_zpk)
    # dt[, (cols_zpk_overig_tweedelijn) := lapply(.SD, function(x) 
    #   x * heeft_tweedelijn), 
    #   .SDcols = cols_zpk]
    
    # Delete unused columns
    dt[, c(cols_zpk, 
           'vektmszdeclaratietype',
           'heeft_eerstelijn', 'diag', 'spec') := NULL]
    
    # Create columns of costs per condition
    cols_msz_gebruikt <- names(dt)[grepl("^(heeft|gebruikt)", 
                                         names(dt))]
    cols_msz_kosten <- sub('heeft_', 'kosten_', cols_msz_gebruikt)
    
    dt[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]
    dt[, (cols_msz_kosten) := lapply(.SD, function(x) 
      x * vektmszvergoedbedragzvw), 
      .SDcols = cols_msz_gebruikt]
    dt$vektmszvergoedbedragzvw <- NULL
    gc()
  }
  
  # Aggreagte data to person level
  cols_msz <- setdiff(names(dt), 'rinpersoon')
  dt <- dt[, lapply(.SD, sum, na.rm = T),
           by = .(rinpersoon),
           .SDcols = cols_msz
  ]
  gc()
  
  # Save
  setindex(dt, NULL)
  arrow::write_parquet(dt, 
                       glue::glue("./data/processed/msz_prestatie_{yr}.parquet"))
  rm(dt)
  gc()
}


#### Find MSZ activities ####
setnames(zpk_codes, 'vektmszdeclaratiecode', 'vektmszzorgactiviteit')
for (yr in c(2017)) {
  print(glue("Currently processing {yr} for mszactiviteiten"))
  
  filepath <- get_newest_parquet_check(
    folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZActiviteiten/parquet_files",
    folder_g_parquet = "G:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/GECONVERTEERDE DATA/",
    folder_g_sav = "G:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB",
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
  )
  
  dt <- arrow::read_parquet(filepath, col_select = 
                              c('RINPERSOON', 'VEKTMSZZorgactiviteit'))
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
  
  cols_msz_gebruikt <- setdiff(names(dt), c('rinpersoon'))
  dt <- dt[, lapply(.SD, sum, na.rm = T),
           by = .(rinpersoon),
           .SDcols = cols_msz_gebruikt
  ]
  gc()
  
  # Save
  setindex(dt, NULL)
  arrow::write_parquet(dt, 
                       glue::glue("./data/processed/msz_activiteiten_{yr}.parquet"))
  rm(dt)
  gc()
}




# for (dataset in c('msz_prestatie_1000_dagen', 'msz_activiteiten_1000_dagen')){
#   print(dataset)
#   for (i in seq_along(rinpersoon_set_chunks)){
#     print(i)
#     unlink(glue("./data/raw/{dataset}_{i}.parquet"))
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

