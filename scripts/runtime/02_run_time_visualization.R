################################################################################
# Visualize relative training runtimes
#
# Run this script from the repository root:
#   Rscript scripts/runtime/02_run_time_visualization.R
################################################################################

library(dplyr)
library(ggplot2)

runtime_file <- file.path(
  "results",
  "runtime",
  "runtime_complexity.rds"
)

settings_file <- file.path(
  "configs",
  "demonstration",
  "settings.rds"
)

figure_dir <- file.path("figures", "runtime")
figure_file <- file.path(figure_dir, "relative_runtime.eps")

if (!file.exists(runtime_file)) {
  stop(
    "Runtime results not found: ",
    runtime_file,
    "\nRun 01_run_complexity.R first."
  )
}

if (!file.exists(settings_file)) {
  stop("Settings file not found: ", settings_file)
}

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

runtime_results <- readRDS(runtime_file)
settings <- readRDS(settings_file) |>
  mutate(id = row_number())

required_runtime_columns <- c(
  "algorithm_id",
  "algorithm",
  "fold",
  "id",
  "training_time_seconds"
)

missing_runtime_columns <- setdiff(
  required_runtime_columns,
  names(runtime_results)
)

if (length(missing_runtime_columns) > 0L) {
  stop(
    "The runtime result file is missing these columns: ",
    paste(missing_runtime_columns, collapse = ", ")
  )
}

algorithm_labels <- c(
  randomForest = "RF",
  SARF = "SARF",
  randomRotationForest = "RR-RF",
  rotationForest = "Rot-For",
  CARF = "CARF",
  extremelyRandomizedForest = "ERT"
)

algorithm_colors <- c(
  "RF" = "#000000",
  "ERT" = "#D55E00",
  "RR-RF" = "#009E73",
  "Rot-For" = "#0072B2",
  "SARF" = "#CC79A7",
  "CARF" = "#7A3E9D"
)

plot_data <- runtime_results |>
  inner_join(settings, by = "id") |>
  filter(linDep) |>
  mutate(
    algorithm_label = unname(algorithm_labels[algorithm]),
    dimensionality = 4L + noiseCols
  ) |>
  group_by(
    algorithm,
    algorithm_label,
    dimensionality
  ) |>
  summarise(
    median_training_time = median(
      training_time_seconds,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  group_by(dimensionality) |>
  mutate(
    rf_training_time = median_training_time[
      algorithm == "randomForest"
    ],
    relative_training_time = median_training_time / rf_training_time
  ) |>
  ungroup()

if (any(is.na(plot_data$algorithm_label))) {
  stop(
    "Unknown algorithms in runtime results: ",
    paste(
      unique(plot_data$algorithm[is.na(plot_data$algorithm_label)]),
      collapse = ", "
    )
  )
}

if (
  any(!is.finite(plot_data$relative_training_time)) ||
  any(plot_data$relative_training_time <= 0)
) {
  stop(
    "Relative runtimes could not be calculated. ",
    "Check that RF results exist and all measured times are positive."
  )
}

runtime_plot <- ggplot(
  plot_data,
  aes(
    x = dimensionality,
    y = relative_training_time,
    color = algorithm_label,
    shape = algorithm_label,
    linetype = algorithm_label
  )
) +
  geom_hline(yintercept = 1, linewidth = 0.4) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.7) +
  scale_y_log10() +
  scale_color_manual(values = algorithm_colors) +
  labs(
    x = "Dimensionality",
    y = "Relative training runtime (RF = 1)",
    color = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

ggsave(
  filename = figure_file,
  plot = runtime_plot,
  width = 20,
  height = 12,
  units = "cm"
)

message("Saved runtime figure to: ", figure_file)
