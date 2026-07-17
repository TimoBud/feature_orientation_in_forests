################################################################################
# Create benchmark meta-features used in the manuscript analysis
################################################################################

library(OpenML)
library(dplyr)

full_result <- readRDS(
  file.path("results", "benchmark", "full_result.rds")
)

task_ids <- unique(full_result$task_id)

mutual_information_numeric <- function(x, y, bins = 10L) {
  breaks <- unique(
    stats::quantile(
      x,
      probs = seq(0, 1, length.out = bins + 1L),
      na.rm = TRUE,
      type = 7
    )
  )
  
  if (length(breaks) <= 2L) {
    return(0)
  }
  
  x_binned <- cut(
    x,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  probabilities <- table(x_binned, y)
  probabilities <- probabilities / sum(probabilities)
  
  marginal_x <- rowSums(probabilities)
  marginal_y <- colSums(probabilities)
  
  valid <- probabilities > 0
  expected <- outer(marginal_x, marginal_y)
  
  sum(
    probabilities[valid] *
      log(probabilities[valid] / expected[valid])
  )
}

best_threshold_accuracy_binary <- function(x, y_binary) {
  order_index <- order(x)
  x <- x[order_index]
  y_binary <- y_binary[order_index]
  
  candidates <- which(diff(x) != 0)
  
  if (length(candidates) == 0L) {
    return(max(mean(y_binary == 0), mean(y_binary == 1)))
  }
  
  max(vapply(
    candidates,
    function(index) {
      prediction <- as.integer(seq_along(x) > index)
      
      max(
        mean(prediction == y_binary),
        mean((1 - prediction) == y_binary)
      )
    },
    numeric(1)
  ))
}

best_threshold_accuracy_multiclass <- function(x, y) {
  max(vapply(
    levels(y),
    function(class_label) {
      best_threshold_accuracy_binary(
        x,
        as.integer(y == class_label)
      )
    },
    numeric(1)
  ))
}

compute_meta_features <- function(x, y) {
  x <- as.data.frame(x)
  y <- as.factor(y)
  
  x <- x[, vapply(x, is.numeric, logical(1)), drop = FALSE]
  
  usable_columns <- vapply(
    x,
    function(column) {
      sum(!is.na(column)) > 1L &&
        stats::sd(column, na.rm = TRUE) > 0
    },
    logical(1)
  )
  x <- x[, usable_columns, drop = FALSE]
  
  n <- nrow(x)
  p <- ncol(x)
  n_classes <- nlevels(y)
  
  if (p < 2L || n < 5L) {
    return(data.frame(
      n = n,
      p = p,
      n_classes = n_classes,
      mean_abs_cor = NA_real_,
      effective_rank_ratio = NA_real_,
      condition_number = NA_real_,
      frac_low_cardinality = NA_real_,
      mean_max_value_frequency = NA_real_,
      signal_concentration_mi = NA_real_,
      mean_best_threshold_acc = NA_real_,
      max_best_threshold_acc = NA_real_
    ))
  }
  
  x_imputed <- x
  for (column_index in seq_len(p)) {
    median_value <- stats::median(
      x_imputed[[column_index]],
      na.rm = TRUE
    )
    x_imputed[[column_index]][
      is.na(x_imputed[[column_index]])
    ] <- median_value
  }
  
  x_scaled <- scale(x_imputed)
  
  correlation_matrix <- suppressWarnings(
    stats::cor(x_scaled)
  )
  correlation_matrix[is.na(correlation_matrix)] <- 0
  
  mean_abs_cor <- mean(
    abs(correlation_matrix[upper.tri(correlation_matrix)])
  )
  
  eigenvalues <- eigen(
    stats::cov(x_scaled),
    symmetric = TRUE,
    only.values = TRUE
  )$values
  eigenvalues <- pmax(eigenvalues, 0)
  
  if (sum(eigenvalues) > 0) {
    probabilities <- eigenvalues / sum(eigenvalues)
    positive_probabilities <- probabilities[probabilities > 0]
    
    effective_rank <- exp(
      -sum(
        positive_probabilities *
          log(positive_probabilities)
      )
    )
    
    positive_eigenvalues <- eigenvalues[
      eigenvalues > 1e-10
    ]
    
    condition_number <- max(eigenvalues) /
      max(min(positive_eigenvalues), 1e-10)
  } else {
    effective_rank <- NA_real_
    condition_number <- NA_real_
  }
  
  unique_counts <- vapply(
    x_imputed,
    function(column) length(unique(column)),
    numeric(1)
  )
  
  unique_ratios <- unique_counts / n
  
  frac_low_cardinality <- mean(
    unique_counts <= 10 |
      unique_ratios < 0.05
  )
  
  mean_max_value_frequency <- mean(vapply(
    x_imputed,
    function(column) {
      max(table(column)) / length(column)
    },
    numeric(1)
  ))
  
  mutual_information_values <- vapply(
    x_imputed,
    mutual_information_numeric,
    numeric(1),
    y = y
  )
  
  signal_concentration_mi <-
    if (sum(mutual_information_values) > 0) {
      max(mutual_information_values) /
        sum(mutual_information_values)
    } else {
      NA_real_
    }
  
  threshold_accuracies <- vapply(
    x_imputed,
    best_threshold_accuracy_multiclass,
    numeric(1),
    y = y
  )
  
  data.frame(
    n = n,
    p = p,
    n_classes = n_classes,
    mean_abs_cor = mean_abs_cor,
    effective_rank_ratio = effective_rank / p,
    condition_number = condition_number,
    frac_low_cardinality = frac_low_cardinality,
    mean_max_value_frequency = mean_max_value_frequency,
    signal_concentration_mi = signal_concentration_mi,
    mean_best_threshold_acc = mean(threshold_accuracies),
    max_best_threshold_acc = max(threshold_accuracies)
  )
}

meta_feature_results <- lapply(
  seq_along(task_ids),
  function(index) {
    task_id <- task_ids[[index]]
    
    message(
      "Progress: ",
      round(100 * index / length(task_ids), 1),
      "%"
    )
    
    task <- OpenML::getOMLTask(task_id)
    task_data <- task$input$data.set$data
    target_name <- task$input$target.features
    
    x <- task_data[
      ,
      setdiff(names(task_data), target_name),
      drop = FALSE
    ]
    y <- unlist(task_data[, target_name, drop = FALSE])
    
    meta_features <- compute_meta_features(x, y)
    meta_features$id <- as.numeric(task_id)
    
    numeric_x <- x[, vapply(x, is.numeric, logical(1)), drop = FALSE]
    meta_features$nonNum <- sum(numeric_x) / length(numeric_x)
    
    meta_features
  }
)

tasks_meta_features <- dplyr::bind_rows(
  meta_feature_results
)

output_file <- file.path(
  "results",
  "benchmark",
  "tasks_meta_features.rds"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(tasks_meta_features, output_file)