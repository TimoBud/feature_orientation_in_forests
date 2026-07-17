################################################################################
# Separation-Aligned Random Forest
################################################################################

make_single_diff_rot <- function(x_num, y) {
  classes <- unique(y)
  class_a <- sample(classes, 1)

  idx_a <- rep(
    sample(which(y == class_a), 1),
    ncol(x_num)
  )
  idx_b <- sample(
    which(y != class_a),
    ncol(x_num),
    replace = FALSE
  )

  difference_matrix <- x_num[idx_a, ] - x_num[idx_b, ]

  rotateDiffs(
    as.matrix(difference_matrix)
  )
}

rotateDiffs <- function(differences) {
  rotation_matrix <- diag(1, ncol(differences))
  transformed_differences <- differences

  for (i in seq_len(nrow(differences) - 1L)) {
    vector_length <- sqrt(
      sum(
        transformed_differences[
          1,
          c(i, i + 1L)
        ]^2
      )
    )

    if (isTRUE(all.equal(vector_length, 0))) {
      rotation_vector <- c(1, 0)
      orthogonal_vector <- c(0, -1)
    } else {
      rotation_vector <-
        transformed_differences[
          1,
          c(i, i + 1L)
        ] / vector_length

      orthogonal_vector <-
        rotation_vector[c(2, 1)] * c(1, -1)
    }

    local_rotation <- diag(1, ncol(differences))
    local_rotation[
      c(i, i + 1L),
      c(i, i + 1L)
    ] <- cbind(
      orthogonal_vector,
      rotation_vector
    )

    rotation_matrix <- rotation_matrix %*% local_rotation
    transformed_differences <-
      transformed_differences %*% local_rotation
  }

  if (ncol(differences) >= 3L) {
    local_rotation <- diag(1, ncol(differences))

    local_rotation[
      seq_len(ncol(differences) - 1L),
      seq_len(ncol(differences) - 1L)
    ] <- rotateDiffs(
      transformed_differences[
        2:ncol(differences),
        seq_len(ncol(differences) - 1L),
        drop = FALSE
      ]
    )

    rotation_matrix <- rotation_matrix %*% local_rotation
  }

  rotation_matrix
}

singleDiffForest <- function(
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
      rotation_matrix <- make_single_diff_rot(
        numeric_data,
        y
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

SARF <- singleDiffForest
