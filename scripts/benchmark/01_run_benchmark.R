################################################################################
# Run benchmark job
################################################################################

library(OpenML)
library(tidyverse)

# job_id = 1
job_id <- suppressWarnings(as.integer(Sys.getenv("PBS_ARRAYID")))

if (is.na(job_id)) {
  stop("PBS_ARRAYID is not set. Run this script as a PBS array job.")
}

source(file.path("R", "general_functions.R"))
source(file.path("R", "rotation_forest.R"))
source(file.path("R", "random_rotation_forest.R"))
source(file.path("R", "SARF.R"))
source(file.path("R", "CARF.R"))
source(file.path("R", "simple_algorithms.R"))

benchmark_jobs <- readRDS(
  file.path("configs", "benchmark", "benchmark_jobs.rds")
)

algorithm_list <- readRDS(
  file.path("configs", "benchmark", "algorithm_list.rds")
)

if (job_id < 1L || job_id > nrow(benchmark_jobs)) {
  stop(
    "PBS_ARRAYID must be between 1 and ",
    nrow(benchmark_jobs),
    "."
  )
}

job <- benchmark_jobs[job_id, , drop = FALSE]
task_id <- job$tsk_id[[1]]
algorithm_id <- job$alg_id[[1]]

task <- OpenML::getOMLTask(task_id)

raw_result_dir <- file.path("results", "benchmark", "raw")
dir.create(raw_result_dir, recursive = TRUE, showWarnings = FALSE)

for (fold in seq_len(10L)) {
  target_name <- file.path(
    raw_result_dir,
    paste0(
      "result_task_", task_id,
      "_algorithm_", algorithm_id,
      "_fold_", fold,
      ".rds"
    )
  )
  
  if (file.exists(target_name)) {
    message("Result already exists: ", target_name)
    next
  }
  
  splits <- task$input$estimation.procedure$data.splits
  task_data <- task$input$data.set$data
  
  idx_split <- splits$fold == fold
  idx_train <- splits$rowid[idx_split & splits$type == "TRAIN"]
  idx_test <- splits$rowid[idx_split & splits$type == "TEST"]
  
  idx_target <- colnames(task_data) == task$input$target.features
  idx_features <- !idx_target
  
  x_train <- task_data[idx_train, idx_features, drop = FALSE]
  y_train <- task_data[idx_train, idx_target]
  
  complete_train <- rowSums(is.na(x_train)) == 0
  x_train <- x_train[complete_train, , drop = FALSE]
  y_train <- y_train[complete_train]
  
  x_test <- task_data[idx_test, idx_features, drop = FALSE]
  y_test <- task_data[idx_test, idx_target]
  
  complete_test <- rowSums(is.na(x_test)) == 0
  x_test <- x_test[complete_test, , drop = FALSE]
  y_test <- y_test[complete_test]
  
  algorithm_name <- algorithm_list[[algorithm_id]]
  algorithm_function <- get(algorithm_name, mode = "function")
  
  set.seed(1)
  start_time <- Sys.time()
  
  model <- algorithm_function(
    x = x_train,
    y = y_train
  )
  
  predictions <- predict(model, x_test)$predictions
  end_time <- Sys.time()
  
  result <- data.frame(
    mcr = mean(predictions != y_test),
    alg_id = algorithm_id,
    fold = fold,
    tasks_id = task_id,
    time_used = end_time - start_time
  )
  
  saveRDS(result, target_name)
}
