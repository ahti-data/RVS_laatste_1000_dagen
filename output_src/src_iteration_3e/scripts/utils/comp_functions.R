#' Make all NA and / or negative values in a data.table equal to zero
#'
#' @description
#' Function to make all NA and / or negative values in a data.table equal to
#' zero
#'
#' @param dt Data.table with columns with numeric values and possible NAs
#' @param cols Numeric columns for which to assign NA values to zero
#' @param na Boolean whether to set NA to zero
#' @param neg Boolean whether to set negative values to zero
#'
#' @return A `data.table` containing zeroes for all NA or negative values in
#' cols
#'
#' @examples
#' \dontrun{
#' dt <- make_na_lt_zero_dt(dt, c("outcome_1", "outcome_2", "outcome_3"))
#' }
#'
#' @export
make_na_lt_zero_dt <- function(dt, cols, na = TRUE, neg = TRUE) {
  ### Function to efficiently make all NA and negative values equal to zero
  na.replace <- function(v, value = 0) {
    v[is.na(v)] <- value
    v
  }
  lt.zero <- function(v, value = 0) {
    v[v < 0] <- value
    v
  }

  for (c in cols) {
    if (na) {
      log_info(
        glue("Making all NA values in {deparse(substitute(dt))} zero}")
      )
      eval(parse(text = paste("dt[,", c, ":=na.replace(", c, ")]")))
    }

    if (neg) {
      log_info(
        glue("Making all negative values in {deparse(substitute(dt))} zero}")
      )
      eval(parse(text = paste("dt[,", c, ":=lt.zero(", c, ")]")))
    }
  }
  dt
}


#' Function to make all non-zero costs equal to one to indicate use
#'
#' @description
#' Function to make all non-zero costs equal to one to indicate use
#'
#' @param dt Data.table with columns with numeric values and possible NAs
#' @param cols Numeric columns for which to assign NA values to zero
#'
#' @return A `data.table` containing ones for all NA or negative values in
#' cols
#'
#' @examples
#' \dontrun{
#' dt <- make_use_dt(dt, c("outcome_1", "outcome_2", "outcome_3"))
#' }
#'
#' @export
make_use_dt <- function(dt, cols) {
  make_use <- function(v) {
    v[v > 0] <- 1
    v[v <= 0] <- 0
    v
  }
  for (c in cols) {
    eval(parse(text = paste("dt[,", c, ":=make_use(", c, ")]")))
  }
  dt
}

#' Logical checks on observed outcomes and comparison outcomes
#'
#' @description
#' Function that checks that all observed outcomes sum up to all comp
#' outcomes. This test is only relevant if target_rins == comp_rins
#'
#' @param dt Data.table with _target and _comp outcomes
#' @param pu Boolean whether check is performed on costs per user
#'
#' @return pass
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
check_comp_target_cols <- function(dt, pu = T) {
  target_cols <- names(dt)[grepl("_target", names(dt))]
  comp_cols <- names(dt)[grepl("_comp", names(dt))]

  target <- dt[, ..target_cols]
  comp <- dt[, ..comp_cols]
  comp <- comp %>%
    as.matrix()
  target <- target %>%
    as.matrix()

  if (pu) {
    comp[is.na(target)] <- NA
  }

  target_cols <- round(colMeans(target, na.rm = T), 3)
  comp_cols <- round(colMeans(comp, na.rm = T), 3)

  assertthat::assert_that(
    all(target_cols[!is.na(target_cols)] == comp_cols[!is.na(comp_cols)])
  )

  log_info("Test success: all comps and all observed align")
}


#' Make estimates by group based on a reference group
#'
#' @description
#' Takes a dataset that contains rinpersoon (ID), a profile column, group labels
#' and a set of outcome columns. For each group in the group labels, we
#' calculate the observed mean, proportion non-zero and mean if positive values.
#' For each, we then add a comparison value that is based on the observed values
#' for each profile in the group but based on a comparison group. For example:
#' we calculate the observed mean zvw costs for Group A and calculate a best-
#' guess value for the mean zvw costs based on the mean cost in some reference
#' group, matched by profile.
#'
#' @param df_sheet Data.table with group / aggregation columns and outcomes
#' @param target_rins What rinpersonen to include to make a comp for
#' @param comp_rins What rinpersonen to base the comp on
#' @param outcomes_target What outcomes to make observed / comp for
#' @param profile_depth What variable to use to make comparison groups
#' @param agg_cols What column(s) to aggregate the dataset by
#' @param use Boolean whether to calculate non-zero means
#' @param costs Boolean whether to calculate numeric means
#' @param costs_per_user Boolean whether to calculate positive numeric means
#'
#' @return
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
make_comp <- function(
    df_sheet, target_rins, comp_rins, outcomes_target,
    profile_depth = "", agg_cols = c("gem"),
    use = TRUE, costs = TRUE, costs_per_user = TRUE) {
  ## Store booleans for convenience
  comp_and_target_align <- setequal(comp_rins, target_rins)

  ## Subset data to include only the group variables by which to aggregate
  agg_sel_cols <- c(agg_cols, "rinpersoon")
  df_agg <- df_sheet[, ..agg_sel_cols]
  setkey(df_agg, rinpersoon)

  ## Subset data to include only outcome, ID and profile data
  rel_cols <- c(
    "rinpersoon",
    outcomes_target,
    glue("profile_{profile_depth}")
  )
  df_sheet <- df_sheet[, ..rel_cols]

  ## Rename the profile column to the generice "cat"
  setnames(df_sheet, glue("profile_{profile_depth}"), "cat")

  ## Make NA and negative values in outcomes equal to zero
  df_sheet <- make_na_lt_zero_dt(
    df_sheet,
    cols = outcomes_target
  )
  setkey(df_sheet, "rinpersoon")

  ## Define target population
  df_sheet_target <- df_sheet[.(target_rins)]
  assertthat::assert_that(nrow(df_sheet_target) == length(target_rins))
  log_info(glue("Target group is {nrow(df_sheet_target)} observations"))

  ## Define comparison population
  df_sheet_comp <- df_sheet[.(comp_rins)]
  assertthat::assert_that(nrow(df_sheet_comp) == length(comp_rins))
  log_info(glue("Comparison group is {nrow(df_sheet_target)} observations"))

  if (use) {
    ## -- USAGE -- ##
    dt_comp_use <- copy(df_sheet_comp)
    dt_comp_use <- make_use_dt(dt_comp_use, cols = outcomes_target)

    dt_target_use <- copy(df_sheet_target)
    dt_target_use <- make_use_dt(dt_target_use, cols = outcomes_target)

    comp_group_use <- dt_comp_use[, lapply(.SD, mean, na.rm = T),
      by = "cat",
      .SDcols = outcomes_target
    ]

    agg_use <- combine_comp_target_and_aggregate(
      dt_target_use, comp_group_use, df_agg, outcomes_target, agg_cols,
      assert_equality = comp_and_target_align
    )

    rm(dt_comp_use, dt_target_use, comp_group_use)

    regional_use <- make_aggregates(
      agg_use,
      agg_cols = "code"
    )
  }

  if (costs) {
    ## -- PER CAPITA COSTS -- ##
    dt_comp_pc <- copy(df_sheet_comp)
    dt_target_pc <- copy(df_sheet_target)

    comp_group_pc <- dt_comp_pc[, lapply(.SD, mean, na.rm = T),
      by = "cat",
      .SDcols = outcomes_target
    ]
    agg_pc <- combine_comp_target_and_aggregate(
      dt_target_pc, comp_group_pc, df_agg, outcomes_target, agg_cols,
      assert_equality = comp_and_target_align
    )

    rm(dt_comp_pc, dt_target_pc, comp_group_pc)

    regional_pc <- make_aggregates(
      agg_pc,
      agg_cols = "code"
    )
  }

  if (costs_per_user) {
    ## -- PER USER COSTS -- ##
    dt_comp_pu <- copy(df_sheet_comp)
    dt_comp_pu <- make_pu_dt(dt_comp_pu, cols = outcomes_target)

    dt_target_pu <- copy(df_sheet_target)
    result_pu <- make_pu_dt(dt_target_pu, cols = outcomes_target)

    comp_group_pu <- dt_comp_pu[, lapply(.SD, mean, na.rm = T),
      by = "cat",
      .SDcols = outcomes_target
    ]
    agg_pu <- combine_comp_target_and_aggregate(
      dt_target_pu, comp_group_pu, df_agg, outcomes_target, agg_cols,
      assert_equality = comp_and_target_align
    )

    rm(dt_comp_pu, dt_target_pu, comp_group_pu)

    # ## Check that overall mean overall assigned means and real data alligns if
    # ## comp and target are the same
    # if (all(target_rins %in% comp_rins) &
    #     all(comp_rins %in% target_rins)) {
    #   check_comp_target_cols(result_pu, log_file ="", pu = T)
    # }

    regional_pu <- make_aggregates(
      agg_pu,
      agg_cols = "code"
    )
  }

  empirical_n <- rbindlist(lapply(agg_cols, function(a) {
    temp <- df_agg[rinpersoon %in% target_rins, .N, by = a]
    setnames(temp, a, "code")
    temp[, level := a]
    return(temp)
  }))

  # unit test uitgezet op aangeven van Mark 21 april 2024 (Marcel)
  # assertthat::assert_that(
  #  all(regional_pc$code[!grepl("--|NL|stadsdeel|buurtteam|Amsterdam", regional_pc$code)] %in% c(kwb$code, "none", "Total", "total")))

  if (exists("kwb")) {
    setkey(kwb, code)
    kwb <- kwb[!is.na(kwb$code), ]

    kwb <- rbind(
      kwb,
      data.frame(code = "total", regio = "total")
    )

    if (costs) {
      setkey(regional_pc, code)
      kwb_pc <- merge(kwb, regional_pc, by = "code")
      kwb_pc <- left_join(kwb_pc, empirical_n)
    }

    if (use) {
      setkey(regional_use, code)
      kwb_use <- merge(kwb, regional_use, by = "code")
      kwb_use <- left_join(kwb_use, empirical_n)
    }

    if (costs_per_user) {
      setkey(regional_pu, code)
      kwb_pu <- merge(kwb, regional_pu, by = "code")
      kwb_pu <- left_join(kwb_pu, empirical_n)
    }
  } else {
    if (costs) {
      kwb_pc <- left_join(regional_pc, empirical_n)
    }
    if (use) {
      kwb_use <- left_join(regional_use, empirical_n)
    }
    if (costs_per_user) {
      kwb_pu <- left_join(regional_pu, empirical_n)
    }
  }

  if (use & costs) {
    assertthat::assert_that(
      sum(kwb_pc$n, na.rm = T) == sum(kwb_use$n, na.rm = T)
    )
  }

  if (costs & costs_per_user) {
    assertthat::assert_that(sum(kwb_pc$n, na.rm = T) == sum(kwb_pu$n, na.rm = T))
  }

  final <- list()

  if (costs) {
    final[["pc"]] <- left_join(regional_pc, empirical_n)
  }
  if (use) {
    final[["use"]] <- left_join(regional_use, empirical_n)
  }
  if (costs_per_user) {
    final[["pu"]] <- left_join(regional_pu, empirical_n)
  }

  return(final)
}

#' Write output from make_comp to .xlsx files
#'
#' @description
#' Helper to write output to .xlsx files and verifies that all expected
#' outcome variables are included.
#'
#' @param results List with data.table(s)
#' @param outcomes_target Outcomes that are expected in the result sheet
#' @param file_name file_name (excluding extension)
#' @param path Folder to write results to
#' @param override_assert Boolean whether to check whether outcomes are present
#'
#' @return pass
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
write_results <- function(results, outcomes_target, file_name,
                          path = path, override_assert = F) {
  lapply(names(results), function(r) {
    if (!override_assert) {
      assertthat::assert_that(
        all(paste0(outcomes_target, "_comp") %in% names(results[[r]]))
      )
    }
    writexl::write_xlsx(
      results[[r]],
      file.path(path, glue::glue("{file_name}_{r}.xlsx"))
    )
  })
}

#' Add group level aggregates
#'
#' @description Function to combine a target population set with group-level
#' expectations and make aggregate observed / expected values.
#' @param target Individual level DT with rinpersoon, cat and outcomes
#' @param comp Group level outcomes
#' @param agg Individual level DT with aggregation columns
#' @param outcomes Outcomes
#' @param agg_cols Cols to aggregate along
#'
#' @return Row-binded aggregate outcomes with observed and expected.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
combine_comp_target_and_aggregate <- function(
    target, comp, agg, outcomes, a_cols, assert_equality = T) {
  setnames(comp, outcomes, paste0(outcomes, "_comp"))

  setkey(target, cat)
  setkey(comp, cat)

  result <- comp[target]
  setnames(result, outcomes, paste0(outcomes, "_target"))

  setkey(result, rinpersoon)
  setkey(agg, rinpersoon)
  result <- agg[result]

  if (assert_equality) {
    check_comp_target_cols(result, pu = T)
  }

  return(make_aggregates(result, agg_cols = a_cols))
}

#' Make group level aggregates
#'
#' @description Function to aggregate along target and comp columns by group
#' variables
#' @param dt Data.table with group column and outcome columns for target and comp
#' @param agg_cols Cols to aggregate along
#'
#' @return Aggregate outcomes with observed and expected.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
make_aggregates <- function(dt, agg_cols = c("bc", "wc", "gem", "total")) {
  agg_list <- lapply(agg_cols, function(x) {
    ac <- names(dt)[
      grepl(paste0("_target|_comp|", x), names(dt))
    ]
    agg_set <- dt[, ..ac]
    agg_set <- agg_set[, lapply(.SD, mean, na.rm = T), by = x]
    setnames(agg_set, x, "code")
    return(agg_set)
  })

  return(rbindlist(agg_list))
}
