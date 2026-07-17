################################################################################
# Run the complex structural simulations
################################################################################

library(dplyr)

source(file.path("R", "general_functions.R"))
source(file.path("R", "rotation_forest.R"))
source(file.path("R", "random_rotation_forest.R"))
source(file.path("R", "SARF.R"))
source(file.path("R", "CARF.R"))
source(file.path("R", "simple_algorithms.R"))

algorithm_list <- list(
  "randomForest",
  "SARF",
  "randomRotationForest",
  "rotationForest",
  "CARF",
  "extremelyRandomizedForest"
)

data_dir <- file.path("data", "demonstration", "complex_data")
result_dir <- file.path("results", "demonstration", "complex_data")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

data_files <- list.files(
  data_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

if (length(data_files) == 0L) {
  stop("No complex simulation data found in: ", data_dir)
}

data_files <- sort(data_files)
job_id <- suppressWarnings(as.integer(Sys.getenv("PBS_ARRAYID")))

#' For running single ids:
# job_id = 1

if (is.na(job_id)) {
  selected_files <- data_files
} else {
  if (job_id < 1L || job_id > length(data_files)) {
    stop("PBS_ARRAYID must be between 1 and ", length(data_files), ".")
  }
  selected_files <- data_files[[job_id]]
}

run_algorithm <- function(algorithm_name, training_data, test_data, seed) {
  set.seed(seed)
  algorithm_function <- get(algorithm_name, mode = "function")

  model <- algorithm_function(
    x = dplyr::select(training_data, -y),
    y = training_data$y
  )

  predictions <- predict(
    model,
    dplyr::select(test_data, -y)
  )$predictions

  mean(predictions == test_data$y)
}

for (file_index in seq_along(selected_files)) {
  data_file <- selected_files[[file_index]]
  target_file <- file.path(result_dir, basename(data_file))

  if (file.exists(target_file)) {
    message("Result already exists: ", target_file)
    next
  }

  problem <- readRDS(data_file)

  if (!all(c("train", "test") %in% names(problem))) {
    stop("Data file does not contain both 'train' and 'test': ", data_file)
  }

  dataset_number <- match(data_file, data_files)
  classification_rates <- vapply(
    seq_along(algorithm_list),
    function(algorithm_id) {
      run_algorithm(
        algorithm_name = algorithm_list[[algorithm_id]],
        training_data = problem$train,
        test_data = problem$test,
        seed = dataset_number * 1000L + algorithm_id
      )
    },
    numeric(1)
  )

  names(classification_rates) <- unlist(algorithm_list, use.names = FALSE)
  saveRDS(classification_rates, target_file)

  message(
    "Saved ", basename(target_file), " (",
    match(data_file, data_files), "/", length(data_files), ")"
  )
}
