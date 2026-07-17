################################################################################
# Run the controlled Gaussian simulations
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

data_dir <- file.path("data", "demonstration", "simple_gaussians")
result_dir <- file.path("results", "demonstration", "simple_gaussians")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

data_files <- list.files(
  data_dir,
  pattern = "^id_[0-9]+\\.rds$",
  full.names = TRUE
)

if (length(data_files) == 0L) {
  stop("No Gaussian data files found in: ", data_dir)
}

extract_data_id <- function(path) {
  as.integer(sub("^id_([0-9]+)\\.rds$", "\\1", basename(path)))
}

data_ids <- vapply(data_files, extract_data_id, integer(1))
order_index <- order(data_ids)
data_files <- data_files[order_index]
data_ids <- data_ids[order_index]

job_id <- suppressWarnings(as.integer(Sys.getenv("PBS_ARRAYID")))

if (is.na(job_id)) {
  stop(
    "PBS_ARRAYID is not set. Submit an array with indices 1-",
    length(data_files) * length(algorithm_list),
    "."
  )
}

number_of_algorithms <- length(algorithm_list)
number_of_jobs <- length(data_files) * number_of_algorithms

if (job_id < 1L || job_id > number_of_jobs) {
  stop("PBS_ARRAYID must be between 1 and ", number_of_jobs, ".")
}

#' For running single ids:
# job_id = 1

data_index <- ((job_id - 1L) %/% number_of_algorithms) + 1L
algorithm_id <- ((job_id - 1L) %% number_of_algorithms) + 1L
data_id <- data_ids[[data_index]]
algorithm_name <- algorithm_list[[algorithm_id]]
algorithm_function <- get(algorithm_name, mode = "function")

problems <- readRDS(data_files[[data_index]])
training_set_name <- "train1000"
train_size <- 1000L

for (run_id in seq_along(problems)) {
  target_file <- file.path(
    result_dir,
    paste0(
      "result_id_", data_id,
      "_algorithm_", algorithm_id,
      "_train_size_", train_size,
      "_run_", run_id,
      ".rds"
    )
  )

  if (file.exists(target_file)) {
    message("Result already exists: ", target_file)
    next
  }

  problem <- problems[[run_id]]

  if (!training_set_name %in% names(problem) || !"test" %in% names(problem)) {
    stop(
      "Data file ", basename(data_files[[data_index]]),
      " does not contain '", training_set_name, "' and 'test' for run ",
      run_id, "."
    )
  }

  training_data <- problem[[training_set_name]]
  test_data <- problem$test

  set.seed(data_id * 100000L + algorithm_id * 1000L + run_id)

  model <- algorithm_function(
    x = dplyr::select(training_data, -y),
    y = training_data$y
  )

  predictions <- predict(
    model,
    dplyr::select(test_data, -y)
  )$predictions

  result <- data.frame(
    mcr = mean(predictions != test_data$y),
    alg_id = algorithm_id,
    run = run_id,
    id = data_id,
    trainSize = train_size
  )

  saveRDS(result, target_file)
  message("Saved: ", target_file)
}
