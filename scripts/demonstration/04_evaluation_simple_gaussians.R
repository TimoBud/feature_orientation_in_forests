################################################################################
# Evaluate the controlled Gaussian simulations
################################################################################

library(dplyr)
library(ggplot2)
library(scales)

result_dir <- file.path(
  "results",
  "demonstration",
  "simple_gaussians"
)

settings_file <- file.path(
  "configs",
  "demonstration",
  "settings.rds"
)

figure_dir <- "figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

result_files <- list.files(
  result_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

if (length(result_files) == 0L) {
  stop("No Gaussian simulation results found in: ", result_dir)
}

result_data <- do.call(
  rbind,
  lapply(result_files, readRDS)
)

settings <- readRDS(settings_file)

algorithm_names <- data.frame(
  alg_id = 1:6,
  Algorithm = factor(
    c("RF", "SARF", "RR-RF", "Rot-For", "CARF", "ERT"),
    levels = c("RF", "ERT", "RR-RF", "Rot-For", "SARF", "CARF")
  )
)

final_results <- result_data |>
  dplyr::right_join(
    settings |>
      dplyr::mutate(id = dplyr::row_number()),
    by = "id"
  ) |>
  dplyr::mutate(
    shared = dplyr::if_else(
      shared,
      "Shared Covariance",
      "Individual Covariance"
    ),
    linDep = dplyr::case_when(
      is.na(linDep) ~ "High Correlation",
      linDep ~ "Singular",
      TRUE ~ "Low Correlation"
    ),
    linDep = factor(
      linDep,
      levels = c(
        "Low Correlation",
        "High Correlation",
        "Singular"
      )
    )
  ) |>
  dplyr::inner_join(
    algorithm_names,
    by = "alg_id"
  )

algorithm_colors <- c(
  "RF" = "#000000",
  "ERT" = "#D55E00",
  "RR-RF" = "#009E73",
  "Rot-For" = "#0072B2",
  "SARF" = "#CC79A7",
  "CARF" = "#7A3E9D"
)

common_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.spacing = grid::unit(2, "lines"),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 15),
    legend.position = "bottom",
    strip.text = ggplot2::element_text(size = 15),
    axis.title = ggplot2::element_text(size = 15),
    axis.text = ggplot2::element_text(size = 15)
  )

distance_plot <- final_results |>
  dplyr::filter(noiseCols == 0) |>
  dplyr::group_by(
    Algorithm,
    shared,
    linDep,
    id,
    dist
  ) |>
  dplyr::summarise(
    mcr = mean(mcr),
    .groups = "drop"
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = dist,
      y = mcr,
      color = Algorithm
    )
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = Algorithm)
  ) +
  ggplot2::geom_smooth(
    ggplot2::aes(linetype = Algorithm),
    se = FALSE,
    linewidth = 0.5
  ) +
  ggplot2::scale_color_manual(values = algorithm_colors) +
  ggplot2::scale_x_log10(
    breaks = c(1e-8, 1e-5, 1e-2),
    labels = scales::label_log()
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4)
  ) +
  ggplot2::facet_grid(
    linDep ~ shared,
    labeller = ggplot2::label_wrap_gen(width = 8),
    scales = "free"
  ) +
  ggplot2::labs(
    x = "Class Distance",
    y = "Misclassification Rate"
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  common_theme

ggplot2::ggsave(
  filename = file.path(figure_dir, "MCR_Dist.eps"),
  plot = distance_plot,
  width = 13,
  height = 15,
  units = "cm"
)

dimension_plot <- final_results |>
  dplyr::filter(
    dist == 1,
    noiseCols <= 25
  ) |>
  dplyr::group_by(
    Algorithm,
    shared,
    linDep,
    id,
    noiseCols
  ) |>
  dplyr::summarise(
    mcr = mean(mcr),
    .groups = "drop"
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = 4 + noiseCols,
      y = mcr,
      color = Algorithm
    )
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = Algorithm)
  ) +
  ggplot2::geom_smooth(
    ggplot2::aes(linetype = Algorithm),
    se = FALSE,
    linewidth = 0.5
  ) +
  ggplot2::scale_color_manual(values = algorithm_colors) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4)
  ) +
  ggplot2::facet_grid(
    linDep ~ shared,
    labeller = ggplot2::label_wrap_gen(width = 8),
    scales = "free_y"
  ) +
  ggplot2::labs(
    x = "Dimensions",
    y = "Misclassification Rate"
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  common_theme

ggplot2::ggsave(
  filename = file.path(figure_dir, "MCR_Dim.eps"),
  plot = dimension_plot,
  width = 13,
  height = 15,
  units = "cm"
)
