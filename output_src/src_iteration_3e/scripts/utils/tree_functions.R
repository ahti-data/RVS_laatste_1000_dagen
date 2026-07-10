#' @name Tree_functions_overview
#' @title Content
#' @md
#' @description
#' Described functions in this document:
#' * [split_labels()]
#' * [fit_rpart_with_stump_check()]
#' * [cross_validation_tree()]
#' * [tree_fit()]
#' * [make_one_tree()]
#' * [make_grviz_tree()]
#'
NULL


#' Wrap long split labels for plotting
#'
#' Formats split labels by inserting spaces after commas and wrapping
#' to a fixed width so they render nicely in tree plots.
#'
#' @param x Ignored (kept for compatibility with plotting callbacks).
#' @param labs Character vector of labels to format.
#' @param digits Ignored (kept for compatibility).
#' @param varlen Ignored (kept for compatibility).
#' @param faclen Ignored (kept for compatibility).
#'
#' @return Character vector with formatted (possibly multi-line) labels.
#' @examples
#' split_labels(NULL, c("a,b,c","verylonglabelname"), 3, 0, 0)
#' @export
split_labels <- function(x, labs, digits, varlen, faclen){
  labs = gsub(",",", ", labs)
  for(i in 1:length(labs)){
    labs[i] <- paste(strwrap(labs[i], width = 15), collapse = "\n")
  }
  labs
}


#' Fit rpart model with stump check
#'
#' Fits an `rpart` tree and throws an error if the result is a stump
#' (i.e., a single-node tree with `<leaf>` only).
#'
#' @param formula Model formula passed to `rpart::rpart()`.
#' @param data Data passed to `rpart::rpart()`.
#' @param ... Additional arguments forwarded to `rpart::rpart()`.
#'
#' @return An `rpart` model object, unless a stump is detected (then `stop()`).
#' @examples
#' \dontrun{
#' fit_rpart_with_stump_check(Species ~ ., data = iris)
#' }
#' @importFrom rpart rpart
#' @export
fit_rpart_with_stump_check <- function(formula, data, ...){
  model <- rpart(formula, data = data, ...)

  if(length(unique(model$frame$var))==1 && unique(model$frame$var)== "<leaf>"){
    stop("Error: The tree is a stump,
         i.e. it has zero layers and did not use any variables.")
  }

  return(model)
}


#' Cross-validated cp for rpart
#'
#' Tunes the `cp` parameter for an rpart tree using 10-fold cross-validation
#' via `caret::train()` with a simple grid.
#'
#' @param dt Training data.
#' @param formula Model formula.
#' @param minbucket Minimum number of observations per terminal node.
#' @param maxdepth Maximum tree depth (number of levels).
#'
#' @return Numeric best `cp` value.
#' @examples
#' \dontrun{
#' cross_validation_tree(iris, Species ~ ., minbucket = 5, maxdepth = 5)
#' }
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom stats na.pass
#' @export
cross_validation_tree <- function(dt, formula, minbucket, maxdepth){
  control <- caret::trainControl(method = "cv", number = 10)
  grid <- expand.grid(cp = seq(0.0001, 0.001, by = 0.0005))
  cl <- parallel::makeCluster(parallel::detectCores() -1)
  doParallel::registerDoParallel(cl)
  tuned_model <- caret::train(formula,
                       data = dt,
                       method = "rpart",
                       trControl = caret::trainControl(method = "cv",
                                                       number = 10),
                       tuneGrid = grid,
                       na.action = na.pass,
                       minbucket = minbucket,
                       maxdepth = maxdepth)
  parallel::stopCluster(cl)
  best_cp <- tuned_model$bestTune$cp

  return (best_cp)
}

#' Bootstrap helper for tree accuracy
#'
#' Fits a tree on a bootstrap sample and evaluates accuracy on the
#' out-of-bag set (or the full data if `indices` cover all rows).
#'
#' @param data Full dataset.
#' @param indices Integer indices for the bootstrap provided by `boot::boot()`.
#' @param formula Model formula.
#' @param cp cp value computed using cross validation
#' @param dep_var Name of dependent variable (column in `data`).
#' @param minbucket Minimum number of observations per node.
#' @param maxdepth Maximum tree depth.
#'
#' @return Numeric accuracy (mean of correct predictions) or `NA` if not computable.
#' @examples
#' \dontrun{
#' # Typically used via boot::boot(...)
#' }
#' @importFrom rpart rpart rpart.control
#' @importFrom stats predict
#' @export
tree_fit <- function(data, indices, formula, cp, dep_var, minbucket, maxdepth){
  train_data <- data[indices, ]
  test_data <- data[-indices, ]

  if (length(indices) == nrow(data)){
    test_data <- data
  }
  model <- rpart(formula,
                 data = train_data,
                 method = "anova",
                 control = rpart.control(cp = cp,
                                         minbucket = minbucket,
                                         maxdepth = maxdepth))

  predictions <- predict(model,
                         newdata = test_data,
                         type = "vector")

  if (nrow(test_data) > 0){
    accuracy <- mean(predictions == test_data[[dep_var]])
  } else {
    accuracy <- NA
    print("test_data contains zero rows")
  }

  if (is.nan(accuracy)) {
    cat("NAN accuracy encountered \n")
  }
  return (accuracy)
}


#' Build, export, and plot a single decision tree
#'
#' Runs cross-validation to select `cp`, optionally bootstraps accuracy,
#' fits an rpart tree, exports a CSV with reproducible tree info, and writes
#' an SVG plot (Graphviz via DiagrammeR).
#'
#' @param dt Dataset used to fit the tree (subset beforehand if needed).
#' @param formula Model formula (prefer factors for categorical inputs).
#' @param dep_var Dependent variable name.
#' @param pop_name Population name (used in filenames).
#' @param region Region label (used in filenames).
#' @param maxdepth Maximum tree depth. Default is 15.
#' @param minbucket Minimum observations per node. Default is 10.
#' @param tree_name Tree name (used in filenames). Default is `"tree"`.
#' @param cp Initial cp (overwritten by cross-validation). Default if 0.001
#' @param cex Font size for plotting. Default is 0.3
#' @param faclen Label shortening length (0 = full label). Default is 0
#' @param save_folder Folder to save outputs.
#' @param save_name Base filename; population/region/tree name appended.
#' @param prevalentie Logical; prevalence (`TRUE`) vs. costs (`FALSE`) mode. Default is TRUE
#' @param include_bootstrap Logical; whether to run bootstrap (can be slow). Default is FALSE
#'
#' @return NULL. Writes `{save_name}_export_tree.csv` and `{save_name}.svg`.
#'
#' @details
#' The CSV includes node ids, depths, labels, sample sizes (rounded to 5),
#' predicted values, split variables, and a filter expression (`path_to_node`)
#' to reconstruct nodes. Nodes with counts \< 10 (and, for prevalence mode,
#' nodes with predicted count \< 10) are removed.
#' The .svg file is for viewing inside the environment, only the .csv file should
#' be submitted for output.
#' This function is already available in the Github outside the RA.
#' The tree can be recreated outside the environment using the make_grviz_tree
#' function.
#'
#' @examples
#' \dontrun{
#' make_one_tree(
#'   dt = iris, formula = Species ~ ., dep_var = "Species",
#'   pop_name = "all", region = "NL", save_folder = tempdir(),
#'   save_name = "iris_tree"
#' )
#' }
#' @importFrom boot boot
#' @importFrom rpart rpart.control printcp path.rpart
#' @importFrom assertthat assert_that
#' @importFrom tidyr drop_na
#' @importFrom stringr str_extract
#' @importFrom glue glue
#' @importFrom DiagrammeR grViz
#' @export
make_one_tree <- function(dt, formula, dep_var, pop_name, region,
                          maxdepth = 15,minbucket = 10, tree_name = "tree",
                          cp = 0.001, cex = 0.3, faclen = 0, save_folder,
                          save_name, prevalentie = T, include_bootstrap = F){

  print(formula)

  # cross validation and bootstrapping (bootstrapping is optional)
  cp <- cross_validation_tree(dt,
                              formula,
                              minbucket = minbucket,
                              maxdepth = maxdepth)
  print(cp)

  if (include_bootstrap == T){
    cat("Doing Bootstrapping \n")
    bootstrap_results <- boot(data = dt,
                              statistic = tree_fit,
                              R=100,
                              formula = formula,
                              cp = cp,
                              dep_var = dep_var,
                              minbucket = minbucket,
                              maxdepth = maxdepth)
    bootstrap_results$t <- na.omit(bootstrap_results$t)
    summary(bootstrap_results$t)
    print(bootstrap_results)
  }

  tree_1000 <-
    fit_rpart_with_stump_check(formula,
                               data = dt,
                               control = rpart.control(maxdepth = maxdepth,
                                                       minbucket = minbucket,
                                                       cp = cp))

  printcp(tree_1000)

  tree_info <- tree_1000$frame
  node_ids <- as.numeric(rownames(tree_info))
  get_node_depth <- function(id){
    floor(log2(id))
  }

  print(tree_info)
  # extract labels
  split_var_names <- tree_info$var[tree_info$var != "<leaf>"]
  split_vars <- unique(split_var_names)
  factor_levels <- lapply(split_vars, function(var) {
    if (is.factor(dt[[var]])) levels(dt[[var]]) else NULL  })
  names(factor_levels) <- split_vars
  splits_raw <- labels(tree_1000, digits = 3, varlen = 0)


  decode_split_label <- function(var, label) {
    if (is.na(var) ||
        !(var %in% names(factor_levels)) ||
        is.null(factor_levels[[var]])) {
      return(label)
    }

    pattern <- paste0("^", var, "=([a-z]+)$")
    match <- regmatches(label, regexec(pattern, label))[[1]]

    if (length(match) > 1){
      letters_str <- match[2]
      letters_vec <- unlist(strsplit(letters_str, ""))
      levels <- factor_levels[[var]]
      idx <- match(letters_vec, letters[1:length(levels)])
      decoded_levels <- levels[idx]

      return (paste0(var, " = ", paste(decoded_levels, collapse = ", \\n")))
    }

    return (label)
  }


  export_table <- data.frame(
    node = node_ids,
    level = sapply(node_ids, get_node_depth),
    split_label = splits_raw,
    percentage = tree_info$n / max(tree_info$n),
    total_n = tree_info$n,
    predicted_class = tree_info$yval,
    formula =  paste0(formula[-1], collapse = " ~ "),
    stringsAsFactors = FALSE
  )

  # check whether node N at the top is equal to population in dt
  assertthat::assert_that(max(export_table$total_n) == nrow(dt))

  # round N
  export_table$total_n <- DescTools::RoundTo(export_table$total_n, 5)

  # calculate whether percentage * n per node is <10
  if (prevalentie == T){
    export_table$value_too_small <- ifelse(export_table$total_n *
                                             export_table$predicted_class < 10 |
                                             export_table$total_n < 10,
                                           NA,
                                           "no")
  } else {
    export_table$value_too_small <- ifelse(export_table$total_n < 10,
                                           NA,
                                           "no")
  }

  library(dplyr)
  library(tidyr)
  export_table <- export_table %>% drop_na()

  print(export_table)
  ## extract path to each node
  paths <- c()
  for (node_number in export_table$node){
    path_list <- path.rpart(tree_1000, nodes = node_number, print.it= F)
    path_to_max <- unlist(path_list[[1]])
    filter_expr <- paste(path_to_max, collapse = " & ")
    paths <- c(paths, filter_expr)
  }

  print(paths)
  export_table$path_to_node <- paths

  var_pattern <- paste0("\\b(", paste(split_vars, collapse = "|"), ")\\b")
  export_table$split_variable <- stringr::str_extract(export_table$split_label,
                                                      var_pattern)

  export_table$split_label <- mapply(decode_split_label,
                                     export_table$split_variable,
                                     export_table$split_label)

  write.csv(export_table, glue::glue("{save_folder}/{save_name}_export_tree.csv"),
            row.names = F)

  g <- make_grviz_tree(export_table, prevalentie)
  print(g)
  svg <- DiagrammeRsvg::export_svg(g)
  writeLines(svg, glue::glue("{save_folder}/{save_name}.svg"))


}


#' Render Graphviz tree from exported table
#'
#' Builds a Graphviz graph (via `DiagrammeR::grViz`) from an exported tree table.
#'
#' @param export_table Table created by `make_one_tree()` containing nodes,
#'   labels, predicted values, percentages, and ids.
#' @param prevalentie Logical; prevalence (`TRUE`) vs. costs (`FALSE`) label mode.
#'
#' @return A `DiagrammeR` htmlwidget object representing the tree.
#'
#' @details
#' This function is also available outside the environment on the Github.
#'
#' @examples
#' \dontrun{
#' g <- make_grviz_tree(export_table)
#' }
#' @importFrom DiagrammeR grViz
#' @export
make_grviz_tree <- function(export_table, prevalentie = T){
  labels <- export_table$split_label
  ids <- export_table$node

  new_labels <- c()
  for (i in 1:length(export_table$split_label)){
    label <- export_table$split_label[[i]]
    if (prevalentie == T){
      pred_label <- paste0("Prevalentiex1000 = ",
                           round(export_table$predicted_class[[i]], 2)*1000)
    } else {
      pred_label <- paste0("Predicted costs = ",
                           round(export_table$predicted_class[[i]], 2))
    }
    n_label <- paste0("n = ",
                      export_table$total_n[[i]],
                      " (", round(export_table$percentage[[i]] * 100,1), "%)")
    # percentage_label <- paste0("percentage = ",
    # round(export_table$percentage[[i]] * 100,1), "%")
    new_label <- paste(label, pred_label, n_label, sep = "\\n")
    new_labels <- c(new_labels, new_label)
  }

  node_defs <- paste0(
    "node", ids, ' [label="',
    new_labels, '", shape=box, style=filled, fillcolor=lightblue];'
  )


  edges <- character()
  for (i in ids) {
    left <- i * 2
    right <- i * 2 + 1
    if (as.character(left) %in% as.character(ids)) {
      edges <- c(
        edges,
        paste0("node", i, " -> node", left, ";")
      )
    }
    if (as.character(right) %in% as.character(ids)){
      edges <- c(edges,
                 paste0("node", i, " -> node", right, ";"))
    }
  }

  grViz(sprintf("
                digraph rpart_tree {
                graph [layout = dot, rankdir = TB, fontsize = 10,
                nodesep = 0.1,
                ranksep= 1.0]
                node [fontname = Arial, fontsize = 10,
                width = 0.1, fixedsize = false, shape = box, style = filled]
                %s
                %s
                }
                ",
                paste(node_defs, collapse = "\n"),
                paste(edges, collapse = "\n")))

}
