################################################################################
# Evaluate the structural simulation study
################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(knitr)
library(kableExtra)

result_dir <- file.path(
  "results",
  "demonstration",
  "complex_data"
)

result_files <- list.files(
  result_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

if (length(result_files) == 0L) {
  stop("No complex simulation results found in: ", result_dir)
}

file_names <- basename(result_files)

classification_rates <- do.call(
  rbind,
  lapply(
    result_files,
    function(file) matrix(readRDS(file), nrow = 1)
  )
)

colnames(classification_rates) <- names(
  readRDS(result_files[[1]])
)

file_properties <- data.frame(
  name = file_names |>
    stringr::str_remove("\\.rds$") |>
    stringr::str_split_i("_", 1),
  run = file_names |>
    stringr::str_remove("\\.rds$") |>
    stringr::str_split_i("_", 2)
) |>
  dplyr::mutate(
    dim = dplyr::case_when(
      stringr::str_detect(name, "20") ~ 20,
      stringr::str_detect(name, "10") ~ 10,
      stringr::str_detect(name, "5") ~ 5
    ),
    name = stringr::str_remove_all(name, "\\d+")
  )

algorithm_labels <- c(
  "randomForest" = "RF",
  "SARF" = "SARF",
  "randomRotationForest" = "RR-RF",
  "rotationForest" = "Rot-For",
  "CARF" = "CARF",
  "extremelyRandomizedForest" = "ERT"
)

problem_labels <- c(
  boundedFeats = "Reflect by Bound",
  boundedWallFeats = "Collect by Bound",
  logitCor = "Logistic Function",
  multiCenterFeats = "Multi Center Gaussian",
  multiClassFeats = "Multi Class Gaussian",
  naturalBinom = "Binomial",
  multiCorFeats = "Multi Cov Gaussian",
  pointMassFeats = "Point Mass Gaussian",
  scaledBinom = "Normalized Binomial",
  oneHotEnc = "One Hot Encoded",
  XOR = "High Dimensional XOR",
  Circ = "Circles",
  logit = "Sparse logit"
)

problem_order <- c(
  "Multi Center Gaussian",
  "Multi Cov Gaussian",
  "Multi Class Gaussian",
  "Reflect by Bound",
  "Collect by Bound",
  "Point Mass Gaussian",
  "Binomial",
  "Normalized Binomial",
  "Logistic Function",
  "Sparse logit",
  "Circles",
  "High Dimensional XOR",
  "One Hot Encoded"
)

results <- data.frame(classification_rates) |>
  dplyr::bind_cols(file_properties) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(names(algorithm_labels)),
    names_to = "algorithm_function",
    values_to = "ccr"
  ) |>
  dplyr::filter(
    !stringr::str_detect(name, "CorBound"),
    !stringr::str_detect(name, "normalizedCorBinom"),
    !stringr::str_detect(name, "multiCorBoundWallFeats"),
    !stringr::str_detect(name, "multiCorBoundFeats"),
    !stringr::str_detect(name, "corBinom")
  ) |>
  dplyr::mutate(
    Algorithm = factor(
      unname(algorithm_labels[algorithm_function]),
      levels = c("RF", "ERT", "RR-RF", "Rot-For", "SARF", "CARF")
    ),
    Problem = factor(
      unname(problem_labels[name]),
      levels = problem_order
    ),
    mcr = 1 - ccr
  ) |>
  dplyr::select(Problem, run, dim, Algorithm, mcr)

# Use the dimensions reported in the manuscript:
# generally the higher-dimensional setting, except for XOR.
paper_results <- results |>
  dplyr::filter(
    (dim > 5 & Problem != "High Dimensional XOR") |
      (dim == 5 & Problem == "High Dimensional XOR")
  )

summary_table <- paper_results |>
  dplyr::group_by(Problem, Algorithm, dim) |>
  dplyr::summarise(
    mean_mcr = mean(mcr, na.rm = TRUE),
    sd_mcr = stats::sd(mcr, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(Problem) |>
  dplyr::mutate(
    best = mean_mcr == min(mean_mcr, na.rm = TRUE),
    value = ifelse(
      best,
      sprintf(
        "\\makecell[c]{\\textbf{%.3f}\\\\\\textbf{(%.3f)}}",
        mean_mcr,
        sd_mcr
      ),
      sprintf(
        "\\makecell[c]{%.3f\\\\(%.3f)}",
        mean_mcr,
        sd_mcr
      )
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(Problem, dim, Algorithm, value) |>
  tidyr::pivot_wider(
    names_from = Algorithm,
    values_from = value
  ) |>
  dplyr::arrange(Problem)

latex_table <- summary_table |>
  kableExtra::kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    escape = FALSE,
    label = "tbl:synthetic_results",
    caption = "Mean misclassification rate and standard deviation over 20 runs."
  ) |>
  kableExtra::kable_styling(
    latex_options = c("repeat_header"),
    font_size = 8
  )

print(latex_table)
