################################################################################
# Correlate benchmark performance gains with dataset meta-features
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

full_result <- readRDS(
  file.path("results", "benchmark", "full_result.rds")
)

tasks_meta_features <- readRDS(
  file.path("results", "benchmark", "tasks_meta_features.rds")
)

# Relative performance gain compared with RF, scaled by the RF standard deviation.
relative_performance <- full_result |>
  dplyr::group_by(task_id, alg_id) |>
  dplyr::summarise(
    sd = stats::sd(mcr),
    mcr = mean(mcr),
    .groups = "drop"
  ) |>
  dplyr::group_by(task_id) |>
  dplyr::mutate(
    relMcr = (mcr[alg_id == 1] - mcr) / sd[alg_id == 1]
  ) |>
  dplyr::ungroup() |>
  dplyr::inner_join(
    tasks_meta_features |>
      dplyr::mutate(id = as.numeric(id)),
    by = c("task_id" = "id")
  ) |>
  dplyr::select(-mcr, -sd) |>
  dplyr::filter(alg_id != 1)

# Spearman correlations between relative performance and all meta-features.
algorithm_ids <- unique(relative_performance$alg_id)

relative_performance_correlations <- lapply(
  algorithm_ids,
  function(algorithm_id) {
    algorithm_data <- relative_performance |>
      dplyr::filter(alg_id == algorithm_id)
    
    meta_feature_data <- algorithm_data |>
      dplyr::select(-relMcr, -task_id, -alg_id)
    
    correlations <- vapply(
      meta_feature_data,
      function(feature) {
        stats::cor(
          feature,
          algorithm_data$relMcr,
          method = "spearman"
        )
      },
      numeric(1)
    )
    
    data.frame(
      as.list(correlations),
      alg_id = algorithm_id,
      check.names = FALSE
    )
  }
) |>
  dplyr::bind_rows()

# Prepare the correlation table used for the manuscript heatmap.
correlation_table <- relative_performance_correlations |>
  dplyr::filter(alg_id != 14) |>
  dplyr::select(-nonNum) |>
  tidyr::pivot_longer(
    cols = n:max_best_threshold_acc,
    names_to = "Feature",
    values_to = "cor"
  ) |>
  dplyr::mutate(
    cor = round(cor, 2),
    alg_id = dplyr::case_when(
      alg_id == 2 ~ "SARF",
      alg_id == 5 ~ "CARF",
      alg_id == 3 ~ "RR-RF",
      alg_id == 4 ~ "Rot-For",
      TRUE ~ as.character(alg_id)
    )
  ) |>
  tidyr::pivot_wider(
    names_from = alg_id,
    values_from = cor
  )

selected_features <- c(
  "n",
  "p",
  "n_classes",
  "mean_abs_cor",
  "effective_rank_ratio",
  "condition_number",
  "frac_low_cardinality",
  "mean_max_value_frequency",
  "signal_concentration_mi",
  "mean_best_threshold_acc"
)

feature_labels <- c(
  n = "Sample size",
  p = "Number of features",
  n_classes = "Number of classes",
  mean_abs_cor = "Mean absolute correlation",
  effective_rank_ratio = "Effective rank ratio",
  condition_number = "Covariance condition number",
  frac_low_cardinality = "Low-cardinality features",
  mean_max_value_frequency = "Point-mass frequency",
  signal_concentration_mi = "Signal concentration",
  mean_best_threshold_acc = "Average threshold accuracy"
)

feature_order <- rev(unname(feature_labels))

heat_df <- correlation_table |>
  dplyr::filter(Feature %in% selected_features) |>
  tidyr::pivot_longer(
    cols = c(`RR-RF`, `Rot-For`, SARF, CARF),
    names_to = "Algorithm",
    values_to = "rho"
  ) |>
  dplyr::mutate(
    Algorithm = factor(
      Algorithm,
      levels = c("RR-RF", "Rot-For", "SARF", "CARF")
    ),
    Feature = dplyr::recode(
      Feature,
      !!!feature_labels
    ),
    Feature = factor(
      Feature,
      levels = feature_order
    )
  )

heatmap_plot <- ggplot2::ggplot(
  heat_df,
  ggplot2::aes(
    x = Algorithm,
    y = Feature,
    fill = rho
  )
) +
  ggplot2::geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", rho)),
    size = 3.5
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-0.7, 0.7),
    name = expression(rho)
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = ""
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 12),
    legend.position = "right"
  )

ggplot2::ggsave(
  filename = file.path("figures", "metaFeatsHeatMap.eps"),
  plot = heatmap_plot,
  width = 18,
  height = 11,
  units = "cm"
)