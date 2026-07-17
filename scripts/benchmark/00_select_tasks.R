################################################################################
# Select OpenML tasks for the benchmark
################################################################################

library(OpenML)
library(dplyr)
library(tidyr)

selected_tasks_file <- file.path(
  "results",
  "benchmark",
  "selected_tasks.rds"
)

dir.create(
  dirname(selected_tasks_file),
  recursive = TRUE,
  showWarnings = FALSE
)

get_selected_tasks <- function(study_id) {
  study <- OpenML::getOMLStudy(study_id)
  
  qualities <- lapply(
    study$data$data.id,
    OpenML::getOMLDataSetQualities
  )
  
  quality_data <- do.call(
    rbind,
    lapply(seq_along(qualities), function(i) {
      cbind(
        qualities[[i]],
        task_id = study$tasks$task.id[i],
        data_id = study$data$data.id[i]
      )
    })
  )
  
  quality_data |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    ) |>
    dplyr::filter(
      NumberOfNumericFeatures >= 5,
      NumberOfInstancesWithMissingValues / NumberOfInstances < 0.1,
      NumberOfNumericFeatures < 200,
      NumberOfInstances < 100000
    ) |>
    dplyr::transmute(
      data_id,
      task_id,
      stud_id = study_id
    )
}

selectedTasks <- dplyr::bind_rows(
  get_selected_tasks(270),
  get_selected_tasks(99),
  get_selected_tasks(337)
) |>
  dplyr::arrange(stud_id) |>
  dplyr::group_by(data_id) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

saveRDS(selectedTasks, selected_tasks_file)

################################################################################
# Create benchmark job configuration
################################################################################

config_dir <- file.path("configs", "benchmark")

dir.create(
  config_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

algorithm_list <- c(
  "randomForest",
  "SARF",
  "randomRotationForest",
  "rotationForest",
  "CARF",
  "extremelyRandomizedForest"
)

benchmark_jobs <- tidyr::crossing(
  tsk_id = selectedTasks$task_id,
  alg_id = seq_along(algorithm_list)
) |>
  dplyr::arrange(tsk_id, alg_id)

saveRDS(
  benchmark_jobs,
  file.path(config_dir, "benchmark_jobs.rds")
)

saveRDS(
  algorithm_list,
  file.path(config_dir, "algorithm_list.rds")
)

message(
  "Saved ",
  nrow(selectedTasks),
  " selected tasks and ",
  nrow(benchmark_jobs),
  " benchmark jobs."
)
