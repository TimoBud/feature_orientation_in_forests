################################################################################
# Statistical comparison of benchmark algorithms
################################################################################

library(dplyr)
library(tidyr)
library(PMCMRplus)
library(xtable)

result_data <- readRDS(
  file.path("results", "benchmark", "full_result.rds")
)

rank_data <- result_data |>
  dplyr::group_by(Algorithm, task_id) |>
  dplyr::summarise(
    mcr = mean(mcr),
    .groups = "drop"
  ) |>
  dplyr::group_by(task_id) |>
  dplyr::mutate(
    r = rank(mcr)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-mcr)

friedman_result <- rank_data |>
  tidyr::pivot_wider(
    names_from = Algorithm,
    values_from = r
  ) |>
  dplyr::select(-task_id) |>
  as.matrix() |>
  stats::friedman.test()

print(friedman_result)

post_hoc_test <- PMCMRplus::frdAllPairsNemenyiTest(
  y = rank_data$r,
  blocks = rank_data$task_id,
  groups = droplevels(rank_data$Algorithm)
)

print(post_hoc_test)

post_hoc_test$p.value |>
  xtable::xtable(
    caption = paste(
      "Matrix of p-values obtained from the Nemenyi post-hoc test",
      "following the Friedman test. Each entry reports the p-value",
      "of the pairwise comparison between two algorithms across all",
      "benchmark data sets. Values below 0.05 indicate statistically",
      "significant differences."
    ),
    label = "tbl:pVals"
  ) |>
  print(caption.placement = "top")