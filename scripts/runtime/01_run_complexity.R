################################################################################
# Measure training time, prediction time, and tree complexity
#
# Run this script from the repository root:
#   Rscript scripts/runtime/01_run_complexity.R
################################################################################

library(dplyr)

algorithm_list <- c(
  "randomForest",
  "SARF",
  "randomRotationForest",
  "rotationForest",
  "CARF",
  "extremelyRandomizedForest"
)

source_files <- file.path(
  "R",
  c(
    "general_functions.R",
    "rotation_forest.R",
    "random_rotation_forest.R",
    "SARF.R",
    "CARF.R",
    "simple_algorithms.R"
  )
)

missing_source_files <- source_files[!file.exists(source_files)]

if (length(missing_source_files) > 0L) {
  stop(
    "The following source files are missing:\n",
    paste0("  - ", missing_source_files, collapse = "\n"),
    "\nRun the script from the repository root."
  )
}

invisible(lapply(source_files, source))

settings_file <- file.path(
  "configs",
  "demonstration",
  "settings.rds"
)

data_dir <- file.path(
  "data",
  "demonstration",
  "simple_gaussians"
)

result_dir <- file.path("results", "runtime")
result_file <- file.path(result_dir, "runtime_complexity.rds")

if (!file.exists(settings_file)) {
  stop("Settings file not found: ", settings_file)
}

if (!dir.exists(data_dir)) {
  stop("Data directory not found: ", data_dir)
}

dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

settings <- readRDS(settings_file)

required_settings_columns <- c("shared", "linDep", "noiseCols")
missing_settings_columns <- setdiff(required_settings_columns, names(settings))

if (length(missing_settings_columns) > 0L) {
  stop(
    "The settings file is missing these columns: ",
    paste(missing_settings_columns, collapse = ", ")
  )
}

relevant_settings <- settings |>
  mutate(id = row_number()) |>
  filter(
    !is.na(linDep),
    (shared & linDep) | (!shared & !linDep),
    id <= 4L | noiseCols > 0L,
    noiseCols <= 40L,
    noiseCols %% 10L == 0L
  )

if (nrow(relevant_settings) == 0L) {
  stop("No settings match the runtime-study filter.")
}

missing_data_files <- file.path(
  data_dir,
  paste0("id_", relevant_settings$id, ".rds")
)
missing_data_files <- missing_data_files[!file.exists(missing_data_files)]

if (length(missing_data_files) > 0L) {
  stop(
    "The following simple-Gaussian data files are missing:\n",
    paste0("  - ", missing_data_files, collapse = "\n")
  )
}

get_ranger_models <- function(model) {
  if (inherits(model, "ranger")) {
    return(list(model))
  }

  if (!is.null(model$model) && inherits(model$model, "ranger")) {
    return(list(model$model))
  }

  if (!is.null(model$models) && is.list(model$models)) {
    ranger_models <- lapply(
      model$models,
      function(component) {
        if (inherits(component, "ranger")) {
          return(component)
        }

        if (
          is.list(component) &&
          !is.null(component$model) &&
          inherits(component$model, "ranger")
        ) {
          return(component$model)
        }

        NULL
      }
    )

    return(Filter(Negate(is.null), ranger_models))
  }

  list()
}

extract_tree_complexity <- function(model) {
  ranger_models <- get_ranger_models(model)

  if (length(ranger_models) == 0L) {
    warning(
      "Could not extract ranger trees from model class: ",
      paste(class(model), collapse = ", ")
    )

    return(
      list(
        number_of_trees = NA_integer_,
        mean_number_of_leaves = NA_real_,
        mean_number_of_nodes = NA_real_
      )
    )
  }

  node_counts <- numeric(0L)
  leaf_counts <- numeric(0L)

  for (ranger_model in ranger_models) {
    number_of_trees <- ranger_model$num.trees

    if (is.null(number_of_trees) || length(number_of_trees) != 1L) {
      number_of_trees <- 1L
    }

    for (tree_id in seq_len(number_of_trees)) {
      tree <- ranger::treeInfo(ranger_model, tree = tree_id)

      node_counts <- c(node_counts, nrow(tree))
      leaf_counts <- c(
        leaf_counts,
        sum(is.na(tree$leftChild) & is.na(tree$rightChild))
      )
    }
  }

  list(
    number_of_trees = length(node_counts),
    mean_number_of_leaves = mean(leaf_counts),
    mean_number_of_nodes = mean(node_counts)
  )
}

runtime_jobs <- expand.grid(
  algorithm_id = seq_along(algorithm_list),
  fold = seq_len(10L),
  id = relevant_settings$id,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

runtime_results <- vector("list", nrow(runtime_jobs))
loaded_data_id <- NA_integer_
problems <- NULL

for (job_index in seq_len(nrow(runtime_jobs))) {
  job <- runtime_jobs[job_index, , drop = FALSE]
  data_id <- job$id[[1]]
  fold_id <- job$fold[[1]]
  algorithm_id <- job$algorithm_id[[1]]
  algorithm_name <- algorithm_list[[algorithm_id]]

  if (is.na(loaded_data_id) || loaded_data_id != data_id) {
    problems <- readRDS(
      file.path(data_dir, paste0("id_", data_id, ".rds"))
    )
    loaded_data_id <- data_id
  }

  if (length(problems) < fold_id) {
    stop(
      "Data file id_", data_id,
      " contains fewer than ", fold_id, " repetitions."
    )
  }

  problem <- problems[[fold_id]]

  if (!all(c("train1000", "test") %in% names(problem))) {
    stop(
      "Data file id_", data_id,
      ", repetition ", fold_id,
      " does not contain both 'train1000' and 'test'."
    )
  }

  training_data <- problem$train1000
  test_data <- problem$test

  if (!"y" %in% names(training_data) || !"y" %in% names(test_data)) {
    stop(
      "The response column 'y' is missing for data id ",
      data_id,
      ", repetition ",
      fold_id,
      "."
    )
  }

  algorithm_function <- get(algorithm_name, mode = "function")

  set.seed(
    data_id * 100000L +
      algorithm_id * 1000L +
      fold_id
  )

  training_time <- system.time({
    model <- algorithm_function(
      x = dplyr::select(training_data, -y),
      y = training_data$y
    )
  })[["elapsed"]]

  tree_complexity <- extract_tree_complexity(model)

  prediction_time <- system.time({
    predictions <- predict(
      model,
      dplyr::select(test_data, -y)
    )$predictions
  })[["elapsed"]]

  runtime_results[[job_index]] <- data.frame(
    algorithm_id = algorithm_id,
    algorithm = algorithm_name,
    fold = fold_id,
    id = data_id,
    training_time_seconds = as.numeric(training_time),
    prediction_time_seconds = as.numeric(prediction_time),
    number_of_trees = tree_complexity$number_of_trees,
    mean_number_of_leaves = tree_complexity$mean_number_of_leaves,
    mean_number_of_nodes = tree_complexity$mean_number_of_nodes
  )

  message(
    "Completed ",
    job_index,
    "/",
    nrow(runtime_jobs),
    ": data id ",
    data_id,
    ", fold ",
    fold_id,
    ", ",
    algorithm_name
  )
}

runtime_results <- bind_rows(runtime_results)

saveRDS(runtime_results, result_file)

message("Saved runtime results to: ", result_file)
