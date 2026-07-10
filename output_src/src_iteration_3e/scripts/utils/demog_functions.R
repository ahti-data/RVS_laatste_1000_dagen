source("H:/utils/m_functions.R")

#' @name Demog_functions_overview
#' @title Content
#' @md
#' @description
#' Described functions in this document:
#' * [read_demog_stapeling()]
#' * [make_inkomen_klasse()]
#' * [make_huishoudsamenstelling()]
#' * [make_geslacht()]
#' * [make_hbopl()]
#' * [add_herkomst()]
#' * [add_seswoa()]
#' * [add_region()]
#' * [cut_w_NA()]
#' * [add_students()]
#' * [replace_values_by_haven_labels()]
NULL


#' Read and process a STAPELINGSMONITOR demographic file
#'
#' @description
#' Reads the most recent STAPELINGSMONITOR file for a given year (31-12-yyyy), so valid for year yyyy+1, selects the
#' requested columns, converts labelled haven variables, and applies default
#' transformations (gender, household composition, education level, income class).
#'
#' @param year Character or integer. The year of the STAPELINGSMONITOR file.
#' @param cols Character vector of additional columns to be taken (without suffix) from the stapelingsmonitor file.
#' @param labelled_cols Character vector of additional columns to be taken AND labelled (without suffix) from the stapelingsmonitor file.
#' @param n_max Maximum number of rows to read (passed to `haven::read_sav()`).
#' 
#' @param use_parquet Boolean indicating whether to use parquet file loading (as opposed to .sav file loading).
#'
#' @return A `data.table` containing cleaned and formatted demographic variables.
#'
#' @examples
#' \dontrun{
#' dt <- read_demog_stapeling(2021)
#' }
#'
#' @export
read_demog_stapeling <- function(
    year, 
    cols = c("leeftijd", "geslacht", "huishoudsamenstelling", "inkomen_klasse", "hbopl", "gem", "wc", "bc"),
    labelled_cols = c("geslacht", "hbopl"),
    n_max = Inf,
    use_parquet = T
    ){
  
    if (!missing(cols) | !missing(labelled_cols)) {
      warning(
        "Column-handling in the read_demog_stapeling function changed as of Feb 2026. The columns passed to 'cols' and/or 'labelled_cols' will be the exact columns returned."
      )
    }
  
  # Assign columns
  calculation_cols = unique(c(
    "percsm", "huishsamstsocec", "hgopl", "hbopl", "inkpers", # for make_inkomen_klasse()
    "huishsamstsocec", # for make_huishoudsamenstelling()
    "geslacht", # for make_geslacht(), 
    "hbopl" # for make_hbopl()
  ))
  
  calculation_labelled_cols = unique(c(
    "huishsamstsocec", "hgopl", "hbopl",  # for make_inkomen_klasse()
    "huishsamstsocec", # for make_huishoudsamenstelling()
    "geslacht", # for make_geslacht(), 
    "hbopl" # for make_hbopl()
    ))
  
  all_cols <- unique(c(calculation_cols, calculation_labelled_cols, cols, labelled_cols))
  
  # Directories and paths
  directory_sav <- "G:/Maatwerk/STAPELINGSMONITOR/"
  path_read_parquet <- get_newest_parquet_check(
    folder_g_parquet = "G:/Maatwerk/STAPELINGSMONITOR/Geconverteerde bestanden",
    folder_h_parquet = "H:/data/Parquet_files_G_drive/Stapeling/parquet_files",
    folder_g_sav = file.path(directory_sav, year),
    string_pattern_parquet = year,
    string_pattern_sav = year,
    stop_on_mismatch = F
  )
  path_read_sav <- get_path_newest(file.path(directory_sav, year), "Stapelingsmonitor", extension=".sav")
  
  # Load dataset
  if (use_parquet) {
    dt <- tryCatch({
      assertthat::assert_that(n_max == Inf, msg = "
                            .parquet files cannot be read using n_max.
                            Please set argument use_parquet = FALSE.")
      ds <- arrow::open_dataset(path_read_parquet)
      dt <- ds |>
        dplyr::select(c("RINPERSOON", dplyr::starts_with(paste0(all_cols, "_")))) |>
        dplyr::collect()
    }, error = function(e) {
      print("Parquet unavailable, defaulting back to sav")
      warning("Parquet unavailable, defaulting back to sav")
      return(haven::read_sav(path_read_sav, col_select = c("RINPERSOON", starts_with(paste0(all_cols, "_"))), n_max = n_max))
    })
  } else {
    dt <- haven::read_sav(path_read_sav, col_select = c("RINPERSOON", starts_with(paste0(all_cols, "_"))), n_max = n_max)
  }
  
  if ("problematische_schulden" %in% all_cols) {
    if ("smalle_def_problematische_schulden_HH_20190101" %in%
        names(dt)) {
      setnames(
        dt, "smalle_def_problematische_schulden_HH_20190101",
        "problematische_schulden_hh"
      )
    }
  }
  dt <- format_data(dt)
  if ("problematische_schulden" %in% all_cols) {
    if ("smalle_schuld_huishouden" %in% names(dt)) {
      setnames(
        dt, "smalle_schuld_huishouden",
        "problematische_schulden_hh"
      )
    }
    ## Rename columns due to inconsistencies over years
    all_cols <- all_cols[!(all_cols %in% c(
      "smalle_def_problematische_schulden_hh",
      "smalle_schuld_huishouden"
    ))]
    all_cols <- unique(c(all_cols, "problematische_schulden_hh"))
    calculation_labelled_cols <- calculation_labelled_cols[
      !(calculation_labelled_cols %in% c(
        "smalle_def_problematische_schulden_hh",
        "smalle_schuld_huishouden"
      ))
    ]
    calculation_labelled_cols <- unique(c(calculation_labelled_cols, "problematische_schulden_hh"))
  }
  
  if (any(grepl("bijzondere.*bijstand", names(dt)))) {
    if ("bijzonderebijstand" %in% names(dt)) {
      setnames(
        dt, "bijzonderebijstand",
        "bijzondere_bijstand"
      )
    }
    
    ## Rename columns due to inconsistencies over years
    all_cols <- all_cols[!(cols %in% c("bijzonderebijstand"))]
    all_cols <- unique(c(all_cols, "bijzondere_bijstand"))
    calculation_labelled_cols <- calculation_labelled_cols[
      !(calculation_labelled_cols %in% c("bijzonderebijstand"))
    ]
    calculation_labelled_cols <- unique(c(calculation_labelled_cols, "bijzondere_bijstand"))
  }
  
  # Replace the calculation label columns
  replace_values_by_haven_labels(
    dt = dt,
    sav_path = path_read_sav,
    cols = intersect(calculation_labelled_cols, names(dt)),
    format = TRUE
  )
  
  missing_cols <- setdiff(all_cols, names(dt))
  if (length(missing_cols > 0)) {
    message("Missing_columns set to 'missing':", paste(missing_cols,
                                                       collapse = ", " ))
    dt[, (missing_cols) := "missing"]
  }
  
  # Add other demog columns
  if (("inkomen_klasse") %in% all_cols) {
    remove_orig <- setdiff(c("inkpers", "percsm", "hgopl"), cols)
    if (!("huishsamstsocec" %in% missing_cols)) { # huishsamstsocec not yet in 2024 dt, this line can be adjusted once newer version comes
      dt <- make_inkomen_klasse(dt, imp_inst = ("inkpers" %in% all_cols), remove_orig = remove_orig)
      cols <- c(cols, "inkomen_klasse")
    }
  }
  
  if (("huishsamstsocec") %in% cols & !("huishsamstsocec" %in% missing_cols)) { # huishsamstsocec not yet in 2024 dt, this line can be adjusted once newer version comes
    dt <- make_huishoudsamenstelling(dt, remove_orig_bool = F)
  } else if ("huishoudsamenstelling" %in% cols & !("huishsamstsocec" %in% missing_cols)) {
    dt <- make_huishoudsamenstelling(dt, remove_orig_bool = T)
  }
  
  if (("geslacht") %in% cols) {
    dt <- make_geslacht(dt)
  }
  
  if (("hbopl") %in% cols) {
    dt <- make_hbopl(dt)
  }
  
  # Replace the additional label columns
  if (length(labelled_cols) > 0) {
    replace_values_by_haven_labels(
      dt = dt,
      sav_path = path_read_sav,
      cols = c(labelled_cols),
      format = TRUE
    )
  }
  
  # return only necessary cols
  return(dt[, .SD, .SDcol = unique(c("rinpersoon", cols, labelled_cols))])
}


#' Construct income classes based on `percsm`, `inkpers`, `huishsamstsocec`, `hbopl`, and `hgopl` from the STAPELINGSMONITOR
#'
#' @description
#' Cleans and imputes the `percsm` variable, corrects institutional households,
#' identifies student cases, and generates the factor `inkomen_klasse`.
#'
#' @details
#' Internally, this function uses a helper function `gen_inst_percsm()` to
#' generate an imputed `percsm` value for institutional households based on
#' personal income (`inkpers`) and the estimated social minimum.
#'
#' @param dt A `data.table` containing at least the variables `percsm`,
#'   `inkpers`, `huishsamstsocec`, `hbopl`, and `hgopl`.
#' @param remove_orig Character vector of column names to remove after
#'   construction of the new variable.
#' @param imp_inst Logical. If `TRUE`, impute `percsm` for institutional
#'   households based on `inkpers`.
#' @param make_student Logical. If `TRUE`, identify students using `hbopl` and
#'   `hgopl`.
#'
#' @return A `data.table` including the constructed `inkomen_klasse` factor.
#'
#' @examples
#' \dontrun{
#' dt <- make_inkomen_klasse(dt)
#' }
#'
#' @export
make_inkomen_klasse <- function(
    dt, remove_orig = c("inkpers", "percsm", "hgopl"), imp_inst=TRUE,
    make_student=TRUE) {

  gen_inst_percsm <- function(dt) {

    ## Calculate social minimum from numeric income and single-person households
    rin_perc_100 <- as.numeric(dt$rinpersoon[dt$percsm == 100]) ## Have a look into NA
    rin_perc_100 <- rin_perc_100[!is.na(rin_perc_100)]


    assertthat::assert_that(
      nrow(dt[huishsamstsocec == "Eenpersoonshuishouden"])>100)
    
    assertthat::assert_that(
      nrow(dt[tolower(hgopl) %in% c("hoger", "hbo, wo")])>100)

    social_minimum <- dt[
      (rinpersoon %in% rin_perc_100) &
        (huishsamstsocec == "Eenpersoonshuishouden"), ]

    social_minimum <- median(social_minimum$inkpers, na.rm = T)

    # data table way
    # social_minimum <- social_minimum[, median(inkpers, na.rm = T)]
    print(paste0("Estimated social minimum is: ", social_minimum))

    ## Assign individuals to income brackets based on personal income
    dt$inkpers <- as.numeric(dt$inkpers)
    print(glue::glue("Setting {sum(is.na(dt$inkpers[dt$percsm == -2]))} NA",
               " institutional inkpers percsm to 0"))
    dt$inkpers <- ifelse((dt$percsm == -2) & is.na(dt$inkpers), 0, dt$inkpers)

    print(glue::glue("Setting {sum(dt$inkpers[dt$percsm == -2] < 0)} negative",
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

  print(glue::glue("Setting percsm to -1 for {sum(is.na(dt$percsm))} NA."))
  dt[is.na(percsm), percsm := -1]

  print(glue::glue("Setting percsm to 1 for {sum(dt$percsm < -3, na.rm=T)} negative percsms",
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
    
    

    dt[(tolower(dt$hgopl) %in% c("hoger", "hbo, wo")) &
         !(tolower(dt$hbopl) %in% c("hoger", "hbo, wo")) &
         (dt$percsm == -3), 
       student := 1]

    print(glue::glue("Identified {sum(dt$student)} students out of ",
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
    dt[["percsm"]], breaks=c(-4, -3, -1, 119, 159, 199, 239, 279, 399, Inf),
    labels=c("student","Onbekend_institutioneel", "tot_120", "120_160", "160_200", "200_240",
             "240_280", "280_400", "400+"))

  assertthat::assert_that(length(unique(dt$inkomen_klasse)) == 9,
                          msg = glue::glue("Length inkomen_klasse is {length(unique(dt$inkomen_klasse))} instead of 9"))
  assertthat::assert_that(!any(is.na(dt$inkomen_klasse)))

  # assertthat::assert_that(
  #   !any(is.na(dt$inkomen_klasse)),
  #   msg = glue::glue("Nr. of rows w/ NA inkomen_klasse: {nrow(dt[is.na(inkomen_klasse)])}"))
  if (length(remove_orig) > 0) {
    print(glue::glue("Removing {remove_orig}"))
    dt[, (remove_orig) := NULL]
  }

  return(dt)
}


#' Harmonise household composition into fixed categories
#'
#' @description
#' Converts the STAPELINGSMONITOR household composition variable
#' (`huishsamstsocec`) into a standardised set of six categories.
#'
#' @param dt A `data.table` containing `huishsamstsocec`.
#' @param remove_orig_bool Logical. If `TRUE`, the original column is renamed. If
#'   `FALSE`, it is preserved.
#'
#' @return A `data.table` with the variable `huishoudsamenstelling`.
#'
#' @export
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
    TRUE ~ "Unknown"
  ) # maybe use fcase instead of case_when?
  assertthat::assert_that(length(unique(dt$huishoudsamenstelling)) == 6)
  return(dt)
}


#' Harmonise the gender variable
#'
#' @description
#' Converts the gender variable to labels: `"Man"` and `"Vrouw"`.
#'
#' @param dt A `data.table` containing the `geslacht` variable.
#'
#' @return A `data.table` with a cleaned gender variable.
#'
#' @export
make_geslacht <- function(dt) {

  print("Making geslacht variable")

  dt$geslacht <- as.character(dt$geslacht)

  # assign in place with :=
  dt$geslacht[dt$geslacht == "vrouw"] <- "Vrouw"
  dt$geslacht[dt$geslacht == "man"] <- "Man"
  assertthat::assert_that(length(unique(dt$geslacht)) == 2)

  return(dt)
}


#' Harmonise highest completed education level (`hbopl`)
#'
#' @description
#' Converts the education variable to lowercase and replaces missing values with
#' `"onbekend"`.
#'
#' @param dt A `data.table` containing the variable `hbopl`.
#'
#' @return A `data.table` with standardised `hbopl` categories.
#'
#' @export
make_hbopl <- function(dt) {

  print("Making opleidingsniveau variable")
  dt[, hbopl := tolower(hbopl)]

  print(glue::glue("Setting {sum(is.na(dt$hbopl))} NA hbopl to onbekend."))
  dt[is.na(hbopl), hbopl := "onbekend"]
  
  # Convert 2023 hbopl & hgopl options to prior years' options
  if (any(unique(dt$hbopl) %in% c("basisonderwijs, vmbo, mbo1", "havo, vwo, mbo2-4", "hbo, wo"))) {
    dt[hbopl == "basisonderwijs, vmbo, mbo1", ':='(hbopl = "lager", hgopl = "lager")]
    dt[hbopl == "havo, vwo, mbo2-4", ':='(hbopl = "middelbaar", hgopl = "middelbaar")]
    dt[hbopl == "hbo, wo", ':='(hbopl = "hoger", hgopl = "hoger")]
  }

  assertthat::assert_that(
    all(dt$hbopl %in% c(
      "lager", "middelbaar", "hoger", "onbekend" #until 2023
      )))

  return(dt)
}


#' Add origin (herkomst) categories to a dataset
#'
#' @description
#' Merges GBA person tables and SSB reference country tables to derive herkomst
#' classifications (`herkomst3` and `herkomst7`) for each individual.
#'
#' @param dt A `data.table` containing numeric `rinpersoon`.
#' @param gbapersoon Optional `data.table` with GBA person information. If not
#'   provided, the most recent version is loaded automatically.
#' @param remove_orig_bool Logical. If `TRUE`, original origin-related columns are
#'   removed.
#' @param herkomst_groepering Character vector indicating which reference-table
#'   grouping variables to use (default `"ETNGRP"`).
#'
#' @return A `data.table` enriched with `herkomst3` and `herkomst7`.
#'
#' @examples
#' \dontrun{
#' dt <- add_herkomst(dt)
#' }
#'
#' @export
add_herkomst <- function(dt, gbapersoon=NULL, remove_orig_bool=FALSE,
                         herkomst_groepering = c("LANDACHTDELING")){
  if (is.null(gbapersoon)) {
    ## Get latest gbapersoon
    year_dirs <- list.dirs("G:/Bevolking/GBAPERSOONTAB", recursive = F, full.names=F)
    newest_year <- max(year_dirs)
    
    print(glue::glue("No gbapersoon provided, loading in {newest_year} version."))
    
    gbapersoon_path <- get_path_newest(
      glue::glue("G:/Bevolking/GBAPERSOONTAB/{newest_year}/"),
      string_pattern="GBAPERSOON",
      extension = ".sav",
      method = "max_version")
    
    gbapersoon <- haven::read_sav(
      gbapersoon_path,
      col_select = c("RINPERSOON", "GBAGENERATIE", 'GBAHERKOMSTGROEPERING'))
    
    gbapersoon <- format_data(gbapersoon)
  }
  
  herkomst_data <- merge(dt, gbapersoon, by="rinpersoon",all.x=T)
  
  reference_file <- as.data.table(haven::read_sav("K:/Utilities/Code_Listings/SSBreferentiebestanden/LANDAKTUEELREFV14.SAV",
                                                  col_select = c("LAND", all_of(herkomst_groepering))))
  reference_file$LAND <- haven::zap_labels(reference_file$LAND)
  # reference_file[, LAND := as.numeric(LAND)]
  
  dt_merged <- merge(herkomst_data,
                     reference_file,
                     by.x="gbaherkomstgroepering",
                     by.y="LAND",
                     all.x=T)
  setnames(dt_merged, c(herkomst_groepering, "gbageneratie"), c("herkomst7", "herkomst3"))
  dt_merged[, herkomst7 := as.character(haven::as_factor(herkomst7))]
  dt_merged[, herkomst3 := as.character(haven::as_factor(herkomst3))]
  
  
  ## update herkomst3 to new labels
  dt_merged[, herkomst3 := ifelse(herkomst3 == "autochtoon",
                                  "Nederlandse herkomst",
                                  ifelse(herkomst3 == "eerste generatie allochtoon",
                                         "Migrant", 
                                         ifelse(herkomst3 == "tweede generatie allochtoon",
                                                "Kind van migrant",
                                                "")))]
  assertthat::assert_that(length(unique(dt_merged$herkomst3)) == 3)
  if (remove_orig_bool) {
    dt_merged$gbaherkomstgroepering <- NULL
  }
  
  return(dt_merged)
}

#' Add SESWOA scores and percentile categories
#'
#' @description
#' Loads SESWOA scores for the following year (year + 1), calculates the average
#' total score per core household, merges these scores back to individuals via
#' `rinpersoonkern`, and assigns percentile categories.
#'
#' @param dt A `data.table` containing numeric `rinpersoon`.
#' @param year Integer. The reference year to which SESWOA scores will be added.
#'
#' @return A `data.table` containing the categorical variable `seswoa_cat`.
#'
#' @export
add_seswoa <- function(dt, year){

  ## Read SESWOA data of year + 1
  seswoa_path <- get_path_newest(
    "G:/Bevolking/SESWOA",
    string_pattern=year+1, extension = ".sav")

  print(seswoa_path)
  seswoa <- haven::read_sav(
    seswoa_path, col_select=c("RINPERSOONHKW", starts_with("TOTAALSCORE")))
  seswoa <- format_data(seswoa, rin_num = FALSE)

  ## Read stapeling of year
  stapelings_path <-
    get_path_newest(file.path("G:/Maatwerk/STAPELINGSMONITOR/", year),
                    "Stapelingsmonitor",
                    extension=".sav")

  stapeling <- haven::read_sav(
    stapelings_path, col_select = c("RINPERSOON",
                                    starts_with("rinpersoonkern_")))
  stapeling <- format_data(stapeling)

  ## Make rinpersoonkern and seswoa_score
  seswoa <- seswoa[
    , .(rinpersoonkern = rinpersoonhkw,
        seswoa_score = rowMeans(
          dplyr::select(seswoa,starts_with("totaalscore_")), na.rm=T))]

  # merge seswoa (mean) and stapeling on the rinpersoonkern to get individual seswoa scores
  seswoa_res <- merge(seswoa, stapeling, by = "rinpersoonkern", all.x=T)
  seswoa_res[, rinpersoonkern := NULL]

  cut_10 <- levels(ggplot2::cut_number(seswoa_res$seswoa_score, n=10))
  cut_5 <- levels(ggplot2::cut_number(seswoa_res$seswoa_score, n=20))

  ranges_seswoa <- c()
  ranges_seswoa <- append(ranges_seswoa, as.numeric(strsplit(strsplit(cut_10[1], ",")[[1]][2], "]")))
  ranges_seswoa <- append(ranges_seswoa, as.numeric(strsplit(strsplit(cut_10[2], ",")[[1]][2], "]")))
  ranges_seswoa <- append(ranges_seswoa, as.numeric(strsplit(strsplit(cut_5[7], ",")[[1]][2], "]")))
  ranges_seswoa <- append(ranges_seswoa, as.numeric(strsplit(strsplit(cut_10[5], ",")[[1]][2], "]")))
  ranges_seswoa <- append(ranges_seswoa, as.numeric(strsplit(strsplit(cut_5[15], ",")[[1]][2], "]")))

  seswoa_res[, seswoa_cat := cut(seswoa_score, c(-10, ranges_seswoa, 10),
                                 labels=c("0-10%", "10-20%", "20-35%", "35-50%", "50-75%", "75-100%"))]

  dt_seswoa <- merge(dt, dplyr::select(seswoa_res, rinpersoon, seswoa_cat),
                     by = "rinpersoon", all.x=T)

  print(glue::glue("Setting {sum(is.na(dt_seswoa$seswoa_cat))} ",
             "SESWOA scores to Onbekend"))

  dt_seswoa[is.na(seswoa_cat), seswoa_cat := "Onbekend"]

  return(dt_seswoa)
}


#' Add regional classifications (e.g., veiligheidsregio, zorgkantoorregio)
#'
#' @description
#' Reads GIN geography files and merges regional aggregation levels to the
#' dataset based on gemeente codes.
#'
#' @param dt A `data.table` containing a column named `gem_<year>`.
#' @param year Character or integer. The year of the GIN classification.
#' @param agg_levels Character vector with desired aggregation levels.
#'
#' @return A `data.table` enriched with the specified regional variables based
#' on gemeente codes.
#'
#' @examples
#' Do we want an example here?
#'
#' @export
add_region <- function(
    dt, year,
    agg_levels=c("veiligheidsregio", "zorgkantoorregio", "gemeente")) {

  ## Read GIN file
  gin_file_path <- get_path_newest(
    path="K:/Utilities/HULPbestanden/GebiedeninNederland/",
    string_pattern = glue::glue("GIN{year}V|GIN{year}.xlsx"), ".xlsx",
    method="newest")

  if (year == "2019") {
    gin_file <- readxl::read_xlsx(gin_file_path, skip=2)
  } else {
    gin_file <- readxl::read_xlsx(gin_file_path)
  }

  ## Fix some inconsistent naming of columns across years
  if (any(grepl("\\|Code", names(gin_file)))) {
    setnames(gin_file, c("Veiligheidsregio's|Code",
                         "Zorgkantoorregio's|Code",
                         "gemeenten|Code"),
             c("veiligheidsregio", "zorgkantoorregio", "gemeente"))
  }

  ## Select relevant columns
  gin_file <- gin_file[, c("gemeente", "veiligheidsregio", "zorgkantoorregio")]
  setnames(gin_file, "gemeente", "gem")

  dt <- merge(dt, gin_file, by = "gem", all.x=T)
  return(dt)
}


#' Cut numeric vector with explicit labels and NA bucket
#'
#' Discretize a numeric vector using integer break points and generate inclusive
#' labels like `"0-4"`, `"5-9"`, etc with seq(0,100,5). Adds an explicit `"Missing"` level for NAs.
#'
#' @param vector Numeric vector. Can also be a data table column
#' @param breaks Numeric vector of break points (ascending).
#' @param is_right_side_open_ended Logical. If `TRUE`, add a final `"+"`
#'   category (>= last break). Default is `FALSE`.
#' @param .missing Character label for missing values. Default is `"Missing"`.
#'
#' @return An ordered factor vector with labelled levels including a Missing level.
#' @examples
#' x <- c(1, 5, 10, NA)
#' cut_w_NA(x, breaks = seq(0, 15, 5))
#' cut_w_NA(x, breaks = c(0,5,10), is_right_side_open_ended = TRUE)
#' @export
cut_w_NA <- function(vector, breaks,is_right_side_open_ended = F,
                     .missing = "Missing"){
  
  if(is_right_side_open_ended){
    breaks = c(breaks, max(vector, na.rm =T)+1)
    labels <- paste(breaks[-length(breaks)],breaks[-1]-1, sep="-")
    labels[length(labels)] <- paste0(breaks[length(breaks)-1],"+")
  }else{
    labels <- paste(breaks[-length(breaks)],breaks[-1]-1, sep="-")
  }
  out = cut(x = vector, breaks = breaks, labels = labels,
            include.lowest = T, right = F)
  out = addNA(out) #convert NA into factor level
  levels(out)[is.na(levels(out))] = .missing
  
  return(out)
}


#' Add student category
#'
#' Function to add student category to a dataset
#' 
#' @param data Dataset with a rinpersoon column
#' @param year year for which to include student types
#'
#' @return Dataset with an added column indicating student type
#' 
#' @export
add_students <- function(data, year) {
  
  nrow_old <- nrow(data)
  students <- fread(file.path(
    "H:", "data", "studerenden", year, "rin_student.csv.gz"))
  assertthat::assert_that(all(data$rinpersoon %in% students$rinpersoon),
                          msg = "Not all rinpersoon in data found in students")
  data <- merge(data, students, by="rinpersoon")
  assertthat::assert_that(nrow(data) == nrow_old,
                          msg = "Size of data post merge is not equal to size
                          of data pre-merge")
  
  return(data)
}


#' Function to replace values in data table by haven labels
#'
#' Function to add student category to a dataset
#' 
#' @param dt Data.table with columns cols
#' @param sav_path Path of sav dataset underlying dt (read from parquet), sav dataset has labelled columns cols
#' @param cols Columns to replace

#' @return data.table with values replaced by labels
#' 
#' @export
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


#' Map to later municipality classification
#'
#' Function to map gemeentecodes from source years to gemeentecodes in a
#' target year.
#' 
#' @param dt A data.table that is assumed to have a gem column and year column
#' @param target_year Year to which to map source years to
#' @param source_years Source years from which to map to the target year
#' @param offset In case the year of the file is offset (e.g. to align with
#' another file) the year has to be offset in merging
#' @return A data.table with a gem_new column indicating the mapped gem
#' 
#' @export
align_gm_to_target_year <- function(dt, target_year, source_years, offset=0) {
  ## Read crosswalk for gemeenten since 1981
  gem_over_years <- readxl::read_xlsx(
    "K:/Utilities/HULPbestanden/GebiedeninNederland/Gemeenten en COROP vanaf 1981.xlsx"  
  )
  
  ## Turn source years into actual gemeente codes
  source_gms <- paste0("GM", source_years)
  
  ## Turn target year into actual gemeente code
  target_gms <- glue("GM{target_year}")
  
  assertthat::assert_that(
    all(c(source_gms, target_gms) %in% names(gem_over_years))
  )
  
  ## Only retain relevant gemeente years
  gem_over_years_rel <- gem_over_years[, unique(c(source_gms, target_gms))]
  gem_over_years_rel <- as.data.table(gem_over_years_rel)
  gem_over_years_rel <- gem_over_years_rel[!duplicated(gem_over_years_rel), ]
  
  ## Report on gemeenten that are split up into multiple gemeenten in the future
  for (year_gm in source_gms) {
    print(year_gm)
    comparison_pair <- c(year_gm, target_gms)
    temp_comp <- gem_over_years_rel[, ..comparison_pair]
    dup_source_gms <- temp_comp[[year_gm]][duplicated(temp_comp[[year_gm]])]
    temp_comp <- temp_comp[temp_comp[[year_gm]] %in% dup_source_gms, ]
    temp_comp <- temp_comp[temp_comp[[year_gm]] != temp_comp[[target_gms]], ]
    if (length(temp_comp) > 0) {
      for (g in unique(temp_comp[[year_gm]])) {
        print(glue(
          "NOTE: gemeente {g} is mapped to: ",
          "{paste0(temp_comp[[target_gms]][temp_comp[[year_gm]] == g])}"))
      }
    }
  }
  
  ## Turn data to long for merging
  gem_over_years_melt <- 
    reshape2::melt(gem_over_years_rel,
                   measure.vars = source_gms,
                   id.vars = target_gms)
  
  gem_over_years_melt$year <- as.numeric(
    gsub("GM", "", gem_over_years_melt$variable))
  
  gem_over_years_melt$variable <- NULL
  setnames(gem_over_years_melt, "value", "gem")
  
  gem_over_years_melt <- unique(gem_over_years_melt)
  gem_over_years_melt$gem <- stringr::str_pad(
    gem_over_years_melt$gem, 4, "left", "0"
  )
  
  gem_over_years_melt <- setDT(gem_over_years_melt)
  
  dt[, year := year - offset]

  dt[, gem := haven::zap_labels(gem)]
  dt[, gem := stringr::str_pad(gem, 4, "left", "0")]
  dt_n <- nrow(dt)
  
  ##NOTE: There may be edge case where a gemeente is split in two and thus
  ##features twice. Assigned to first instance
  
  ## Case 1: Gemeente Haaren dissolved into multiple gemeenten.
  ## Oisterwijk received the main village.
  print("Assigning gemeente Haaren (0788) to Oisterwijk (0824) in target")
  gem_over_years_melt[gem_over_years_melt$gem %in% c("0788") &
                        gem_over_years_melt[[target_gms]] %in%
                        c("0757", "0824", "0865"), (target_gms)  := "0824"]
  gem_over_years_melt <- gem_over_years_melt[!duplicated(gem_over_years_melt), ]
  
  gem_over_years_melt_clean <- gem_over_years_melt[
    !(duplicated(gem_over_years_melt[, 2:3])), 
  ]
  
  if (nrow(gem_over_years_melt_clean) < nrow(gem_over_years_melt)) {
    print(glue(
      "WARNING: {nrow(gem_over_years_melt) - nrow(gem_over_years_melt_clean)} ",
      "municipalities split into one or more municipalities."))
    
    deviant_gms <- gem_over_years_melt$gem[
      (duplicated(gem_over_years_melt[, 2:3]))]
    
    print(glue("Affected municipalities: "))
    print(gem_over_years_melt[gem_over_years_melt$gem %in% deviant_gms, ])
    print("Municipalities in source year are assigned to first instance.")
  }
  
  dt <- merge(dt, gem_over_years_melt_clean,
              by=c("year", "gem"), all.x=T)
  
  assertthat::assert_that(nrow(dt) == dt_n)
  
  dt[, gem_new := gem]
  
  dt[gem == "----", (target_gms) := "----"]
  dt[!is.na(get(target_gms)), gem_new := get(target_gms)]
  
  ## Ensure unique gemeenten in each year is the same
  check_cols <- c("gem_new", "year")
  year_check <- dt[, ..check_cols]
  year_check <- year_check[!duplicated(year_check), ]
  year_check <- year_check[gem_new != "----", ]
  year_count <- year_check[
    gem_new != "----",
    .(unique_gem = length(unique(gem_new))), by = year]
  
  assertthat::assert_that(
    all(year_count$unique_gem == year_count$unique_gem[1])
    )
  
  dt[, (target_gms) := NULL]
  
  dt[, year := year + offset]

  return(dt)  
}
