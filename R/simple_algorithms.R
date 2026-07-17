################################################################################
# Standard tree-ensemble wrappers
################################################################################

randomForest <- function(
    x,
    y,
    ntrees = 500,
    mtry = sqrt(ncol(x)),
    sample.fraction = 1,
    replace = TRUE
) {
  ranger::ranger(
    x = x,
    y = y,
    num.trees = ntrees,
    mtry = mtry,
    sample.fraction = sample.fraction,
    replace = replace
  )
}

extremelyRandomizedForest <- function(
    x,
    y,
    ntrees = 500,
    mtry = ncol(x),
    sample.fraction = 1,
    replace = FALSE
) {
  ranger::ranger(
    x = x,
    y = y,
    num.trees = ntrees,
    splitrule = "extratrees",
    num.random.splits = 1,
    mtry = mtry,
    sample.fraction = sample.fraction,
    replace = replace,
    min.node.size = 1,
    min.bucket = 1
  )
}

