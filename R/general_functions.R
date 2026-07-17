################################################################################
# General functions for rotated tree ensembles
################################################################################

find_numeric_columns <- function(x) {
  vapply(
    seq_len(ncol(x)),
    function(i) identical(class(x[, i]), "numeric"),
    logical(1)
  )
}

predict.rotateEachTree <- function(object, x, ...) {
  numeric_data <- x[, object$numCols, drop = FALSE]
  numeric_data <- sweep(
    numeric_data,
    2,
    object$scales,
    "/"
  )

  if (sum(!object$numCols) > 0L) {
    non_numeric_data <- x[, !object$numCols, drop = FALSE]
  }

  all_predictions <- sapply(
    object$models,
    function(tree_model) {
      rotated_data <- as.matrix(numeric_data) %*% tree_model$rrm
      colnames(rotated_data) <- object$names_num

      if (sum(!object$numCols) > 0L) {
        rotated_data <- cbind(
          rotated_data,
          non_numeric_data
        )
      }

      predict(
        tree_model$model,
        rotated_data
      )$predictions
    }
  )

  predictions <- vapply(
    seq_len(nrow(x)),
    function(i) {
      names(
        sort(
          table(all_predictions[i, ]),
          decreasing = TRUE
        )
      )[1]
    },
    character(1)
  )

  list(
    allPreds = all_predictions,
    predictions = predictions
  )
}

mergeForests <- function(forestList) {
  result <- list(
    numCols = forestList[[1]]$numCols,
    scales = forestList[[1]]$scales,
    names_num = forestList[[1]]$names_num,
    models = do.call(
      c,
      lapply(
        forestList,
        function(model) model$models
      )
    )
  )

  class(result) <- "rotateEachTree"
  result
}
