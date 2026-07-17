################################################################################
# Combine benchmark results into a single data frame
################################################################################

library(OpenML)
library(dplyr)
library(tidyr)

raw_result_dir <- file.path("results", "benchmark", "raw")
full_result_file <- file.path("results", "benchmark", "full_result.rds")
selected_tasks_file <- file.path("results", "benchmark", "selected_tasks.rds")

result_files <- list.files(
  raw_result_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

if (length(result_files) == 0L) {
  stop("No benchmark result files found in: ", raw_result_dir)
}

result_data <- do.call(
  rbind,
  lapply(result_files, readRDS)
)

selected_tasks <- readRDS(selected_tasks_file)

algorithm_names <- data.frame(
  alg_id = 1:6,
  Algorithm = factor(
    c("RF", "SARF", "RR-RF", "Rot-For", "CARF", "ERT"),
    levels = c("RF", "ERT", "RR-RF", "Rot-For", "SARF", "CARF", "SCARF")
  )
)

get_study_qualities <- function(study_id) {
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
        task_id = study$tasks$task.id[i]
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
    dplyr::select(
      task_id,
      AutoCorrelation,
      Dimensionality,
      MajorityClassPercentage,
      NumberOfClasses,
      NumberOfInstances,
      NumberOfNumericFeatures,
      NumberOfSymbolicFeatures
    ) |>
    dplyr::mutate(
      frctNumeric =
        NumberOfNumericFeatures /
        (NumberOfNumericFeatures + NumberOfSymbolicFeatures),
      stud_id = study_id
    )
}

all_quality_data <- dplyr::bind_rows(
  get_study_qualities(99),
  get_study_qualities(270),
  get_study_qualities(337)
) |>
  dplyr::distinct()

complete_tasks <- result_data |>
  dplyr::count(tasks_id, name = "n") |>
  dplyr::filter(n == 60) |>
  dplyr::select(tasks_id) |>
  dplyr::inner_join(
    selected_tasks |>
      dplyr::select(-data_id),
    by = c("tasks_id" = "task_id")
  )

# One task did not run with ERT and is therefore excluded.
full_result <- all_quality_data |>
  dplyr::inner_join(
    result_data |>
      dplyr::inner_join(complete_tasks, by = "tasks_id"),
    by = c("task_id" = "tasks_id")
  ) |>
  dplyr::inner_join(
    algorithm_names,
    by = "alg_id"
  ) |>
  dplyr::filter(task_id != 146820 | stud_id.x == 99) |>
  dplyr::select(-stud_id.x, -stud_id.y)

dir.create(
  dirname(full_result_file),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(full_result, full_result_file)