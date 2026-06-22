# Project: Laatste 1000 dagen
# Author: Stanislav Avdeev & Marco Griep
# Goal: Set inputs used in other files
# Output: None
# Last edited: 23 February 2026

#### initialize ####
library(data.table)
library(glue)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(collapse)
library(assertthat)

source("H:/utils/backup/20260605/m_functions.R")
source("H:/utils/backup/20260605/demog_functions.R")

#### filepaths & output destinations ####
current_iteration_name <- "3c"
output_folder <- glue("output/iteration_{current_iteration_name}/")


#### parameters ####
years <- 2016:2023
cols_to_select_stapeling_31_12 <- c("burgstaat", "stedgem") # 'geslacht', 'leeftijd'

cols_to_label_stapeling_31_12 <- c("burgstaat", "stedgem") # 'geslacht'

cols_to_select_stapeling_01_01 <- c(
  "hbopl", "hgopl", "huishsamstsocec",
  "percsm", "inkpers", "provincie" # "belanginkbronpers", 
)

cols_to_label_stapeling_01_01 <- c(
  "hbopl", "hgopl", "huishsamstsocec" # "belanginkbronpers"
)

cols_to_select_overlijden <- c("RINPERSOON", "GBADatumOverlijden")

cols_to_select_doodoorz <- c("RINPERSOON", "UCCODE") # , "NNDLocationCode"

cols_to_select_gbapersoon <- c(
  "RINPERSOON", "GBAGESLACHT", "GBAGEBOORTEJAAR",
  "GBAGEBOORTEMAAND", "GBAGENERATIE", 'GBAHERKOMSTGROEPERING'
)

cols_to_select_gbahuishoudensbus <- c("RINPERSOON", "DATUMAANVANGHH", "DATUMEINDEHH")

cols_to_select_zorgkosten <- c(
  "RINPERSOON", "ZVWKTOTAAL", "ZVWKZIEKENHUIS",
  "ZVWKFARMACIE", "ZVWKWYKVERPLEGING", "ZVWKHULPMIDDEL",
  "ZVWKHUISARTS", "NOPZVWKHUISARTSINSCHRIJF",
  "NOPZVWKHUISARTSCONSULT", "NOPZVWKHUISARTSOVERIG"
)

cols_to_select_msz_prestatie <- c('RINPERSOON', 
                                  'VEKTMSZSpecialismeDiagnoseCombinatie', 
                                  'VEKTMSZDBCZorgproduct', 'VEKTMSZBegindatumPrest',
                                  #'VEKTMSZDeclaratiecode', 'VEKTMSZDeclaratietype',
                                  'VEKTMSZVergoedbedragZVW'
                                  #'VEKTMSZInstellingPrest',
                                 # 'VEKTMSZZorgtype'
                                  )

cols_to_select_msz_activiteiten <- c('RINPERSOON', 
                                     'VEKTMSZKoppelIDPrestZa', 'VEKTMSZBeginjaarPrest',
                                     'VEKTMSZZorgactiviteit', 
                                     'VEKTMSZZorgactiviteitdatum'
                                     #'VEKTMSZInstellingZa'
                                     )

cols_to_select_medicijn <- c('RINPERSOON', 'ATC4')

cols_to_select_huisarts <- c("RINPERSOON", "HADECLPrestatiecode")

cols_to_select_wlz <- c("RINPERSOON", "BEGINWLZZIN")

# cost columns by dataset
cost_columns_zvw <- c("zvwktotaal", "zvwkziekenhuis", "zvwkfarmacie",
                      "zvwkwykverpleging", "zvwkhulpmiddel", "zvwkhuisarts",
                      "nopzvwkhuisartsinschrijf", "nopzvwkhuisartsconsult",
                      "nopzvwkhuisartsoverig", "zvwkggzzpmtotaal")
cost_columns_msz <- c("vektmszvergoedbedragzvw", "vektmszvergoedbedragav")
cost_columns_huisarts <- c("hadeclvergoedbedrag", "hadeclvergoedbedrag_inschrijving", "hadeclvergoedbedrag_consulatations", "hadeclvergoedbedrag_others")
cost_columns_wlz <- c("bedragwlzzin", "bedragwlzzin_onbekend", "bedragwlzzin_verblijf", 
                      "bedragwlzzin_verblijf_inc_behandeling", "bedragwlzzin_volledig_pakket_thuis",
                      "bedragwlzzin_verblijf_exc_behandeling", "bedragwlzzin_mod_pakket_thuis")
cost_columns_wvp <- c("bedragzvwwvp")


cpi <- data.table(
  year = c(2015:2024),
  cpi = c(
    100.00, 100.32, 101.70, 103.44, 106.16,
    107.51, 110.39, 121.43, 126.09, 130.31
  )
)
base_2023 <- cpi[year == 2023, cpi]
cpi[, cpi := cpi / base_2023]



#### functions ####


make_aggregated_data <- function(df, group_var = "leeftijd_cat", columns = NULL) {
  col_names <- if (is.null(columns)) {
    names(df)
  } else {
    columns
  }
  
  # Calculate means and medians
  cols_binary <- col_names[grepl("^(heeft|gebruikt|referred|has|used)", col_names)]
  if (length(cols_binary) == 0) {
    cols_binary <- col_names[grepl("heeft|gebruikt|referred|has|used", col_names)]
  }
  cols_cont <- col_names[grepl("^(zvw|nopzvw|kosten|n_|vektmszvergoedbedrag|hadeclvergoedbedrag|bedrag)", col_names)]
  cols_other <- setdiff(col_names, c(cols_binary, cols_cont))
  cols_binary <- setdiff(cols_binary, group_var)
  cols_cont <- setdiff(cols_cont, group_var)
  cols_other <- unique(c(cols_other, group_var))
  
  cat("\n Binary variables: ", paste(cols_binary, collapse = ", "), "\n")
  cat("\n Continuous variables: ", paste(cols_cont, collapse = ", "), "\n")
  cat("\n Other variables: ", paste(cols_other, collapse = ", "), "\n")
  
  # Binary variables
  if (length(cols_binary) == 0) {
    cat("No binary columns found")
    df_agg_binary <- df[, .(n_totaal = round(uniqueN(sample_id), -1)),
                        by = group_var
    ]
  } else {
    df_agg_binary <- df[, c(
      setNames(
        lapply(.SD, function(x) as.numeric(round(mean(x, na.rm = T), 5))),
        paste0(names(.SD), "_gemiddelde_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) round(uniqueN(rinpersoon[x > 0]), -1)),
        paste0(names(.SD), "_n_totaal_gebruikers")
      ),
      .(n_totaal = round(uniqueN(rinpersoon), -1))
    ),
    by = group_var,
    .SDcols = cols_binary
    ]
  }
  
  # Continuous variables
  if (length(cols_cont) == 0) {
    cat("No continuous columns found")
    df_agg_cont <- unique(df[, ..group_var])
  } else {
    df_agg_cont <- df[, c(
      setNames(
        lapply(.SD, function(x) as.numeric(mean(x, na.rm = T))),
        paste0(names(.SD), "_gemiddelde_per_persoon")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(mean(x[x != 0], na.rm = T))),
        paste0(names(.SD), "_gemiddelde_per_gebruiker")
      ),
      # setNames(
      #   lapply(.SD, function(x) as.numeric(median(x, na.rm = T))),
      #   paste0(names(.SD), "_mediaan_per_persoon")
      # ),
      # setNames(
      #   lapply(.SD, function(x) as.numeric(median(x[x != 0], na.rm = T))),
      #   paste0(names(.SD), "_mediaan_per_gebruiker")
      # ),
      # setNames(
      #   lapply(.SD, function(x) as.numeric(quantile(x, 0.05, na.rm = T))),
      #   paste0(names(.SD), "_q05_per_persoon")
      # ),
      # setNames(
      #   lapply(.SD, function(x) as.numeric(quantile(x, 0.25, na.rm = T))),
      #   paste0(names(.SD), "_q25_per_persoon")
      # ),
      # setNames(
      #   lapply(.SD, function(x) as.numeric(quantile(x, 0.75, na.rm = T))),
      #   paste0(names(.SD), "_q75_per_persoon")
      # ),
      # setNames(
      #   lapply(.SD, function(x) as.numeric(quantile(x, 0.95, na.rm = T))),
      #   paste0(names(.SD), "_q95_per_persoon")
      # ),
      setNames(
        lapply(.SD, function(x) sum(x, na.rm = T)),
        paste0(names(.SD), "_sum_totaal_groep")
      ),
      setNames(
        lapply(.SD, function(x) round(uniqueN(rinpersoon[x > 0]), -1)),
        paste0(names(.SD), "_n_totaal_gebruikers")
      ),
      setNames(
        lapply(.SD, function(x) as.numeric(round(mean(x > 0, na.rm = T), 5))),
        paste0(names(.SD), "_gebruikt_per_persoon")
      )
    ),
    by = group_var,
    .SDcols = cols_cont
    ]
  }
  
  df_agg <- merge(df_agg_binary, df_agg_cont, by = group_var, all = T)
  
  df_agg <- drop_if_below_x(df_agg, c(cols_binary, cols_cont), x = 10)
  
  first_cols <- c(group_var, "n_totaal")
  other_cols <- setdiff(names(df_agg), first_cols)
  other_cols <- sort(other_cols)
  setcolorder(df_agg, c(first_cols, other_cols))
  
  # Make it to long format
  df_agg <- melt(df_agg,
                 id.vars = first_cols,
                 measure.vars = other_cols,
                 variable.name = "variable",
                 value.name = "value"
  )
  
  # Split a variable name into two parts
  df_agg[, c("name", "type") := {
    parts <- stringr::str_split(variable, "_")
    list(
      sapply(parts, \(x) paste(head(x, -3), collapse = "_")),
      sapply(parts, \(x) paste(tail(x, 3), collapse = "_"))
    )
  }]
  
  # Drop if below 10 counts
  df_agg <- df_agg[complete.cases(value)]
  
  return(df_agg)
}


# If the number of people with a given condition < 10, code all measures as NA
drop_if_below_x <- function(df_agg, cols, x = 10) {
  for (col in cols) {
    var_cols <- grep(paste0("^", col, "($|_)"), names(df_agg), value = T)
    df_agg[get(paste0(col, "_n_totaal_gebruikers")) <= x, (var_cols) := NA]
  }
  return(df_agg)
}

calculate_costs_by_bin_size <- function(
    costs_dt,
    overlijden_dt,
    cost_columns,
    cost_date_col,
    bin_size = c("months33", "months5", "50", "100", "1000", "2years", "last_month", "wlz_categorisation"),
    aggregate_groupby_cols = NULL,
    get_counts = FALSE,
    id_col = c(),
    inflation_correction = NULL
){

  # define bins
  chosen_bin_size <- match.arg(bin_size)
  step_days <- switch(chosen_bin_size,
                      "months33" = 30,
                      "months5" = 30,
                      "50" = 50,
                      "100" = 100,
                      "1000" = 1000,
                      "2years" = 365*2, 
                      "last_month"= 30,
                      "wlz_categorisation"= 0
  )
  
  # copies
  costs_dt_clean <- copy(costs_dt)
  overlijden_dt_clean <- copy(overlijden_dt)
  
  n_bins <- floor(1000 / step_days)
  if (chosen_bin_size == "last_month") {
    n_bins = 1
  } else if (chosen_bin_size == "months5") {
    n_bins = 5
  } else if (chosen_bin_size == "wlz_categorisation") n_bins = 4
  if (!popEpi::is.Date(costs_dt_clean[[cost_date_col]])) {
    costs_dt_clean <- fast_to_date(costs_dt_clean, cost_date_col)
    # costs_dt_clean[, (cost_date_col) := as.IDate(as.character(get(cost_date_col)), format = "%Y%m%d")]
  }

  # change costs to numeric
  if(!is.null(aggregate_groupby_cols)) {
    costs_dt_clean[, (cost_columns) := lapply(.SD, as.numeric), .SDcols = cost_columns]
  }
  
  # Correct for inflation
  if(!is.null(inflation_correction)) {
    costs_dt_clean[, year := as.integer(substr(get(cost_date_col), 1, 4))]
    costs_dt_clean <- merge(costs_dt_clean, cpi, by = 'year', all.x = T)
    costs_dt_clean[, (cost_columns) :=
         lapply(.SD, function(x) x / cpi),
       .SDcols = cost_columns]
    costs_dt_clean[, c('year', 'cpi') := NULL]
  }
  
  overlijden_dt_clean <- copy(overlijden_dt)

  if (chosen_bin_size == "wlz_categorisation") {
    dt_grid <- overlijden_dt_clean[, .(
      t = -1:-(n_bins)
    ), by = names(overlijden_dt_clean)]
    
    dt_grid[, bin_end := fcase(
                                t == -1 , gbadatumoverlijden + 30, # start in last 0 -3 months
                                t == -2 , gbadatumoverlijden + -3 * 30, # start in last 4-12 months
                                t == -3 , gbadatumoverlijden + -12 * 30, # start in last 13-33 months
                                t == -4 , gbadatumoverlijden + -33 * 30 # start before 1000 days
    )]
    
    dt_grid[, bin_start := fcase( 
                                  t == -1 , gbadatumoverlijden + (-3 * 30) + 1, # start in last 0 -3 months
                                  t == -2 , gbadatumoverlijden + (-12 * 30) + 1, # start in last 4-12 months
                                  t == -3 , gbadatumoverlijden + (-33 * 30) + 1, # start in last 13-33 months
                                  t == -4 , gbadatumoverlijden + (-45 * 30) # start before 1000 days
    )]
  } else {
    dt_grid <- overlijden_dt_clean[, .(
      t = 1:-(n_bins)
    ), by = names(overlijden_dt_clean)]
    
    dt_grid[, bin_end := gbadatumoverlijden + ((t + 1) * step_days)]
    dt_grid[, bin_start := gbadatumoverlijden + ((t) * step_days) + 1]
  
  # for overleden populations, we want to get until t=1, for alive, we want until -1
  dt_grid <- dt_grid[!(died == "In leven" & t %in% c(0, 1))]
  dt_grid <- dt_grid[(died == "Overleden" & t %in% c(0, 1)), t := -1]
  }
  
  
  # convert to numeric for foverlaps
  dt_grid[, c("bin_start", "bin_end") := lapply(.SD, function(x) {
    year(x) * 10000 + month(x) * 100 + mday(x)
  }), .SDcols = c("bin_start", "bin_end")]
  costs_dt_clean[, ":="(start_temp = get(cost_date_col), end_temp = get(cost_date_col))]
  costs_dt_clean[, c("start_temp", "end_temp") := lapply(.SD, function(x) {
    year(x) * 10000 + month(x) * 100 + mday(x)
  }), .SDcols = c("start_temp", "end_temp")]
  
  setkey(dt_grid, rinpersoon, bin_start, bin_end)
  setkey(costs_dt_clean, rinpersoon, start_temp, end_temp)
  print("overlapping")
  dt_merged <- foverlaps(
    x = dt_grid,
    y = costs_dt_clean,
    mult = "all",
    type = "any",
    nomatch = NA
  )

  if (!is.null(aggregate_groupby_cols)) {
    print("aggregating")
    j_syntax <- paste0(
      ".(",
      paste0(cost_columns, " = sum(", cost_columns, ", na.rm = T)", collapse = ", "),
      ")"
    )
    
    dt_merged <- dt_merged[,
      eval(parse(text = j_syntax)),
      by = c(union(aggregate_groupby_cols, "t"))
    ]
  }
  
  if (get_counts) {
    # make cost column numeric
    dt_merged[, (cost_columns) := as.numeric(get(cost_columns))]
    
    final_group_cols <- c(names(overlijden_dt_clean), id_col)

    dt_merged <- dt_merged[, .(
      n_rows = .N,
      sum_costs = sum(get(cost_columns), na.rm=T),
      share_ozp = mean(is_ozp)
      ), by = final_group_cols]

    dt_merged[, days_before_death := step_days]
  }
  return(dt_merged)
}

load_mszprestaties <- function(yr, cols, rinpersoon_chunk = NULL, labelled_cols = NULL) {
  
  filepath <- get_newest_parquet_check(
    folder_h_parquet = "H:/data/Parquet_files_G_drive/MSZPrestaties/parquet_files",
    folder_g_parquet = "G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data/",
    folder_g_sav = "G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB",
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
  )
  
  ds <- arrow::open_dataset(filepath)
  
  if (!is.null(rinpersoon_chunk)) {
    dt_mszprest <- ds |>
      filter(as.numeric(RINPERSOON) %in% rinpersoon_chunk) |>
      select(c(dplyr::starts_with(cols))) |>
      collect()
  } else {
    dt_mszprest <- ds |>
      select(c(dplyr::starts_with(cols))) |>
      collect()
  }
  
  if (!is.null(labelled_cols)) {
    dt_mszprest <- replace_values_by_haven_labels(
      dt_mszprest,
      sav_path = get_path_newest_updated("G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB", yr, extension = ".sav"),
      cols = labelled_cols
      )
  }
  
  return(format_data(dt_mszprest))
}

load_huisartsdecltab <- function(yr, cols, rinpersoon_chunk = NULL) {
  
  filepath <- get_newest_parquet_check(
    folder_h_parquet = "H:/data/Parquet_files_G_drive/HUISARTSDECLTAB/parquet_files",
    folder_g_parquet = "G:/GezondheidWelzijn/HUISARTSDECLTAB",
    folder_g_sav = "G:/GezondheidWelzijn/HUISARTSDECLTAB",
    string_pattern_parquet = yr,
    string_pattern_sav = yr,
  )
  
  ds <- arrow::open_dataset(filepath)
  
  if (!is.null(rinpersoon_chunk)) {
    dt_huisarts <- ds |>
      filter(as.numeric(RINPERSOON) %in% rinpersoon_chunk) |>
      select(c(dplyr::all_of(cols))) |>
      collect()
  } else {
    dt_huisarts <- ds |>
      select(c(dplyr::all_of(cols))) |>
      collect()
  }
  
  return(format_data(dt_huisarts))
}

replace_values_by_haven_labels <- function(dt, sav_path, cols, format = F){
  
  dt_temp = haven::read_sav(
    sav_path,
    n_max=1,
  )
  
  # Necessary if dt is already formatted:
  if (format) {
    dt_temp <- format_data(dt_temp)
  }
  dt_temp <- dt_temp[, .SD, .SDcols = cols]
  
  print("replacing labelled columns")
  for(col in cols){
    labs = attributes(dt_temp[[col]])$labels
    dict = setNames(as.list(names(labs)),as.character(labs))
    if(grepl("opl", col)){
      dict[dict=="Hoog"] = "Hoger"
    }
    
    dt[, (col):=as.character(get(col))]
    suppressWarnings({
      dt[get(col) %in% names(dict),(col):=dict[as.character(get(col))]]
    })
  }
}

make_huishoudsamenstelling <- function(dt, remove_orig_bool=T) {
  
  print("Making huishoudsamenstelling variable")
  
  if (remove_orig_bool) {
    setnames(dt, "huishsamstsocec", "huishoudsamenstelling")
  } else {
    dt$huishoudsamenstelling <- copy(dt$huishsamstsocec)
  }
  
  
  dt$huishoudsamenstelling <- dplyr::case_when(
    dt$huishoudsamenstelling == "Paar zonder kinderen" ~  "2persoons_hh",
    dt$huishoudsamenstelling == "Eenpersoonshuishouden" ~  "1persoons_hh",
    dt$huishoudsamenstelling == "Paar met kinderen" ~  "stel_kinderen_hh",
    dt$huishoudsamenstelling == "Eenouderhuishouden" ~  "eenouder_hh",
    dt$huishoudsamenstelling == "Overig meerpersoonshuishouden" ~  "overig_onbekend_hh",
    dt$huishoudsamenstelling == "Onbekend" ~  "overig_onbekend_hh",
    dt$huishoudsamenstelling == "Institutioneel huishouden" ~  "institutioneel_hh",
    dt$huishoudsamenstelling == "Niet in populatie stapelingsmonitor" ~  "overig_onbekend_hh",
    dt$huishoudsamenstelling == "Niet in populatie stapelingsmonitor (31-12-JJJJ)" ~  "overig_onbekend_hh",
    dt$huishoudsamenstelling == "9" ~  "overig_onbekend_hh",
    TRUE ~ "Unknown"
  ) # maybe use fcase instead of case_when?
  assertthat::assert_that(length(unique(dt$huishoudsamenstelling)) == 6)
  return(dt)
}

make_inkomen_klasse <- function(
    dt, remove_orig = c("inkpers", "percsm", "hgopl"), imp_inst=TRUE,
    make_student=TRUE) {
  ## Function to wrangle percsm variable from stapelingsmonitor to inkomen_klasse
  #' @param dt Data.table with original percsm variable
  #' @param remove_orig Columns in original data to remove
  #' @param imp_inst Boolean whether to impute percsm for institutional
  #' @param make_student Use hbopl and hgopl to identify students
  #' @return Data.table with wrangled huishoudsamenstelling variable
  
  gen_inst_percsm <- function(dt) {
    ## Make a percsm variable for institutional households based on inkpers
    #' @param dt Data.table that includes percsm, and inkpers
    #' @return Assignment of institutionals to standard inkomen_klasses
    
    ## Calculate social minimum from numeric income and single-person households
    rin_perc_100 <- as.numeric(dt$rinpersoon[dt$percsm == 100]) ## Have a look into NA
    rin_perc_100 <- rin_perc_100[!is.na(rin_perc_100)]
    
    assertthat::assert_that(
      nrow(dt[huishsamstsocec == "Eenpersoonshuishouden"])>100)
    
    assertthat::assert_that(
      nrow(dt[tolower(hgopl) == "hoger"])>100)
    
    assertthat::assert_that(
      nrow(dt[tolower(hgopl) == "hoger"])>100)
    
    social_minimum <- dt[
      (rinpersoon %in% rin_perc_100) &
        (huishsamstsocec == "Eenpersoonshuishouden"), ]
    
    social_minimum <- median(social_minimum$inkpers, na.rm = T)
    # data table way
    # social_minimum <- social_minimum[, median(inkpers, na.rm = T)]
    
    print(paste0("Estimated social minimum is: ", social_minimum))
    
    ## Assign individuals to income brackets based on personal income
    dt$inkpers <- as.numeric(dt$inkpers)
    print(glue("Setting {sum(is.na(dt$inkpers[dt$percsm == -2]))} NA",
               " institutional inkpers percsm to 0"))
    dt$inkpers <- ifelse((dt$percsm == -2) & is.na(dt$inkpers), 0, dt$inkpers)
    
    print(glue("Setting {sum(dt$inkpers[dt$percsm == -2] < 0)} negative",
               " institutional inkpers percsm to 1"))
    dt$inkpers <- ifelse((dt$percsm == -2) & (dt$inkpers < 0), 1, dt$inkpers)
    
    # above lines can be more efficient with skipping some dt$var....
    
    ## Make boolean for institutional
    dt[dt$percsm == -2,
       percsm := (inkpers / social_minimum) * 100]
    
    return(dt)
  }
  
  ## Format percsm
  print("Making inkomen_klasse variable")
  assertthat::has_name(dt, "percsm")
  dt[, percsm := as.numeric(percsm)]
  
  print(glue("Setting percsm to -1 for {sum(is.na(dt$percsm))} NA."))  
  dt[is.na(percsm), percsm := -1]
  
  print(glue("Setting percsm to 1 for {sum(dt$percsm < -3, na.rm=T)} negative percsms",
             " (less than -3)"))  
  dt[percsm < -3, percsm := 1]
  
  assertthat::assert_that(all(!is.na(dt$percsm)))
  assertthat::assert_that(all(between(dt$percsm, -3, Inf)))
  
  if (make_student) {
    ## If someone is following a "hoog" opleidingsniveau but has not completed
    ## and is labeled a student according to inkomen_klasse, make student
    ## else assign a low income.
    
    print("Identifying students based on hbopl and hgopl")
    
    assertthat::has_name(dt, "hbopl")
    assertthat::has_name(dt, "hgopl")
    
    dt$student <- 0
    
    dt[(tolower(dt$hgopl) == "hoger") &
         (tolower(dt$hbopl) != "hoger") &
         (dt$percsm == -3), student := 1]
    
    print(glue("Identified {sum(dt$student)} students out of ",
               "{sum(dt$percsm == -3)} labeled according to income. ",
               "Assigning rest a percsm of 1."))
    
    dt[(percsm == -3) & (student != 1), percsm := 1]
    dt[, student := NULL]
  }
  
  if (imp_inst) {
    print("Making percsm version based on inkpers for institutional households")
    
    assertthat::has_name(dt, "inkpers")
    
    dt <- gen_inst_percsm(dt)
  }
  
  dt <- dt[, percsm := as.integer(percsm)]
  dt[["inkomen_klasse"]] <- cut(
    dt[["percsm"]], breaks=c(-4, -3, -1, 119, 279, 399, Inf), 
    labels=c("Overig","Overig", "tot_120", "120_280", 
             "280_400", "400+"))
  
  assertthat::assert_that(length(unique(dt$inkomen_klasse)) == 5)
  # assertthat::assert_that(
  #   !any(is.na(dt$inkomen_klasse)),
  #   msg = glue("Nr. of rows w/ NA inkomen_klasse: {nrow(dt[is.na(inkomen_klasse)])}"))
  # 
  if (length(remove_orig) > 0) {
    print(glue("Removing {remove_orig}"))
    dt[, (remove_orig) := NULL]
  }
  
  return(dt)
}

get_path_newest_updated <- function(
    path,
    string_pattern,
    extension=".dta",
    method="max_version",
    recursive = FALSE
){
  files_target_extension = list.files(path, pattern = paste0("\\",extension,"$"), full.names = T, ignore.case = T, recursive = recursive)
  
  matching_files = files_target_extension[grepl(string_pattern,files_target_extension)]
  
  if(length(matching_files)==0){
    return(paste0("No matching ",extension," files found."))
  }
  
  if(method=="newest"){
    file_info = file.info(matching_files)
    out_file_path = matching_files[which.max(file_info$mtime)]
  }else if(method=="max_version"){
    matching_files = matching_files[grepl("V[0-9]+",matching_files)]
    if(length(matching_files)==0){
      return("No files matching pattern 'V%digit' found.")
    }
    file_numbers = as.numeric(gsub(".*V([0-9]+).*","\\1",basename(matching_files)))
    out_file_path = matching_files[which.max(file_numbers)]
  }
  
  return(out_file_path)
}

fast_to_date <- function(dt, col_name, format = "%Y%m%d") {
  unique_values <- dt[!is.na(get(col_name)), .(raw = unique(get(col_name)))]
  
  unique_values[, clean := as.IDate(raw, format = format)]
  
  dt[unique_values, clean_col := i.clean, on = setNames("raw", col_name)]
  
  dt[, (col_name) := NULL]
  setnames(dt, "clean_col", col_name)
  
  return(dt)
}

load_reflijst_activiteiten <- function(cols = NULL) {
  # load and clean reflijst
  reflijst_zorgactiviteiten <- rio::import("K:/GezondheidWelzijn/MSZZORGACTIVITEITENVEKTTAB/ReflijstZorgactiviteiten.ods")
  if (is.null(cols)) {reflijst_zorgactiviteiten <- format_data(reflijst_zorgactiviteiten, rin_num = F)}
  else {
  reflijst_zorgactiviteiten <- format_data(reflijst_zorgactiviteiten, rin_num = F)[, .SD, .SDcols = cols]
  }
  reflijst_zorgactiviteiten[, mszzorgactiviteit := as.numeric(mszzorgactiviteit)]
  return(reflijst_zorgactiviteiten)
}

merge_with_validate <- function(x, y, by = intersect(names(x), names(y)),
                                by.x = by, by.y = by, validate = NULL, 
                                require_match = NULL, ...) {
  
  
  if (!is.null(validate)) {
    check_dups <- function(df, cols, table_name) {
      if (length(cols) == 0) {
        stop(sprintf("No 'by'columns found for the %s table.", table_name))
      }
      if (anyDuplicated(df[, ..cols, drop=FALSE])) {
        stop(sprintf("Merge validation error: The %s table is not unique on the merge key(s).", table_name))
      }
    }
    
    validate <- match.arg(validate, choices = c("one_to_one", "one_to_many", 
                                                "many_to_one", "many_to_many"))
    # perform validation
    if (validate == "one_to_one") {
      check_dups(x, by.x, "left")
      check_dups(y, by.y, "right")
    } else if (validate == "one_to_many") {
      check_dups(x, by.x, "left")
    } else if (validate == "many_to_one") {
      check_dups(y, by.y, "right")
    }
  }
    

    

  
  if (!is.null(require_match)) {
    require_match <- match.arg(require_match, choices = c("left", "right", "both"))

    keys_x <- do.call(paste, c(x[, ..by.x, drop=FALSE], sep = "\r"))
    keys_y <- do.call(paste, c(y[, ..by.y, drop=FALSE], sep = "\r"))
    
    if (require_match %in% c("left", "both")) {
      missing_in_y <- sum(!keys_x %in% keys_y)
      if (missing_in_y > 0) {
        stop(sprintf("Match validation error: %d rows in the LEFT table have no match in the right table.", missing_in_y))
      }
    }
    
    if (require_match %in% c("right", "both")) {
      missing_in_x <- sum(!keys_y %in% keys_x)
      if (missing_in_x > 0) {
        stop(sprintf("Match validation error: %d rows in the RIGHT table have no match in the right table.", missing_in_y))
      }
    }
  }
  
  
  # perform the merge
  merge(x = x, y = y , by.x = by.x, by.y = by.y, ...)
}
