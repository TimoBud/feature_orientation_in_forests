################################################################################
# Random Rotation Forest
################################################################################

createRandomRotation <- function(dim) {
  random_matrix <- matrix(
    stats::rnorm(dim^2),
    nrow = dim
  )

  rotation_matrix <- qr.Q(qr(random_matrix))

  if (det(rotation_matrix) <= 0) {
    random_matrix[, 1] <- -random_matrix[, 1]
    rotation_matrix <- qr.Q(qr(random_matrix))
  }

  rotation_matrix
}

randomRotationForest <- function(
    x,
    y,
    ntrees = 500,
    mtry = ncol(x),
    sample.fraction = 1,
    replace = FALSE
) {
  numeric_columns <- find_numeric_columns(x)

  numeric_data <- x[, numeric_columns, drop = FALSE]
  scales <- if (sum(numeric_columns) > 1L) {
    apply(numeric_data, 2, stats::sd)
  } else {
    rep(1, sum(numeric_columns))
  }
  scales[scales == 0] <- 1
  numeric_data <- sweep(
    numeric_data,
    2,
    scales,
    "/"
  )

  if (sum(!numeric_columns) > 0L) {
    non_numeric_data <- x[, !numeric_columns, drop = FALSE]
  }

  numeric_names <- paste0(
    "num",
    seq_len(sum(numeric_columns))
  )

  models <- lapply(
    seq_len(ntrees),
    function(i) {
      rotation_matrix <- createRandomRotation(
        sum(numeric_columns)
      )

      rotated_data <- as.matrix(numeric_data) %*%
        rotation_matrix
      colnames(rotated_data) <- numeric_names

      if (sum(!numeric_columns) > 0L) {
        rotated_data <- cbind(
          rotated_data,
          non_numeric_data
        )
      }

      tree_model <- ranger::ranger(
        y = y,
        x = rotated_data,
        mtry = mtry,
        replace = replace,
        sample.fraction = sample.fraction,
        num.trees = 1,
        min.node.size = 1,
        min.bucket = 1
      )

      list(
        rrm = rotation_matrix,
        model = tree_model
      )
    }
  )

  result <- list(
    numCols = numeric_columns,
    scales = scales,
    names_num = numeric_names,
    models = models
  )

  class(result) <- "rotateEachTree"
  result
}
