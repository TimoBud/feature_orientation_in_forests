################################################################################
# Rotation Forest
################################################################################

createRotForestRotation <- function(
    x_num,
    y,
    K = floor(ncol(x_num) / 3)
) {
  groups <- sample(seq_len(ncol(x_num))) %% K
  rotation_matrix <- diag(1, ncol(x_num))

  for (group_id in unique(groups)) {
    included_classes <- unique(
      sample(
        unique(y),
        length(unique(y)),
        replace = TRUE
      )
    )

    included_rows <- y %in% included_classes
    sampled_rows <- sample(
      which(included_rows),
      round(0.75 * sum(included_rows))
    )

    group_columns <- which(groups == group_id)

    rotation_matrix[
      group_columns,
      group_columns
    ] <- stats::prcomp(
      x_num[
        sampled_rows,
        group_columns,
        drop = FALSE
      ],
      scale = FALSE,
      center = FALSE
    )$rotation
  }

  rotation_matrix
}

rotationForest <- function(
    x,
    y,
    ntrees = 500,
    mtry = ncol(x),
    sample.fraction = 1,
    replace = FALSE
) {
  numeric_columns <- find_numeric_columns(x)
  K <- floor(sum(numeric_columns) / 3)

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
      rotation_matrix <- tryCatch(
        createRotForestRotation(
          numeric_data,
          y,
          K
        ),
        error = function(error) {
          message(
            "Error while computing the Rotation Forest transformation: ",
            error$message
          )
          diag(1, ncol(numeric_data))
        }
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
