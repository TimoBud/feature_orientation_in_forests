################################################################################
# Create the controlled Gaussian demonstration data
################################################################################

set.seed(1L)

output_dir <- file.path("data", "demonstration", "simple_gaussians")
settings_file <- file.path("configs", "demonstration", "settings.rds")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(settings_file), recursive = TRUE, showWarnings = FALSE)

create_4_class_generator <- function(
    class_distance = 1,
    noise_columns = 0L,
    shared_covariance = TRUE,
    centers_per_class = 1L,
    covariance_sample_size = (4L + noise_columns)^2
) {
  dimension <- 4L + noise_columns

  if (!centers_per_class %in% c(1L, 2L)) {
    stop("centers_per_class must be either 1 or 2.")
  }

  centers <- lapply(seq_len(4L), function(class_index) {
    center <- numeric(dimension)
    center[class_index] <- class_distance / sqrt(2)

    if (centers_per_class == 1L) {
      matrix(center, nrow = 1L)
    } else {
      rbind(center, -center)
    }
  })

  make_covariance <- function() {
    stats::cov(
      scale(
        matrix(
          stats::rnorm(covariance_sample_size),
          ncol = dimension
        )
      )
    )
  }

  covariance_matrices <- if (shared_covariance) {
    covariance_matrix <- make_covariance()
    rep(list(covariance_matrix), 4L)
  } else {
    lapply(seq_len(4L), function(i) make_covariance())
  }

  sample_data <- function(n) {
    if (n %% 4L != 0L) {
      stop("n must be divisible by 4 to obtain balanced classes.")
    }

    observations_per_class <- n %/% 4L
    feature_matrix <- matrix(NA_real_, nrow = n, ncol = dimension)
    row_start <- 1L

    for (class_index in seq_len(4L)) {
      center_ids <- sample(
        seq_len(centers_per_class),
        observations_per_class,
        replace = TRUE
      )

      for (center_index in seq_len(centers_per_class)) {
        count <- sum(center_ids == center_index)
        if (count == 0L) {
          next
        }

        rows <- row_start:(row_start + count - 1L)
        feature_matrix[rows, ] <- mvtnorm::rmvnorm(
          count,
          mean = centers[[class_index]][center_index, ],
          sigma = covariance_matrices[[class_index]]
        )
        row_start <- row_start + count
      }
    }

    response <- factor(
      rep(letters[1:4], each = observations_per_class),
      levels = letters[1:4]
    )

    data.frame(feature_matrix, y = response, check.names = FALSE)
  }

  list(
    centers = centers,
    covariances = covariance_matrices,
    class_distance = class_distance,
    noise_columns = noise_columns,
    shared_covariance = shared_covariance,
    centers_per_class = centers_per_class,
    covariance_sample_size = covariance_sample_size,
    sample = sample_data
  )
}

# Backward-compatible aliases used in older scripts.
create4ClassDataGenerator <- create_4_class_generator
create4classData <- function(
    n,
    classDists,
    noiseCols = 0L,
    sharedCov = TRUE,
    centersPerClass = 1L,
    ncov = (4L + noiseCols)^2
) {
  create_4_class_generator(
    class_distance = classDists,
    noise_columns = noiseCols,
    shared_covariance = sharedCov,
    centers_per_class = centersPerClass,
    covariance_sample_size = ncov
  )$sample(n)
}

base_settings <- rbind(
  expand.grid(
    shared = c(TRUE, FALSE),
    linDep = c(TRUE, FALSE),
    dist = c(10^(-seq.int(0L, 10L)), 0.5),
    noiseCols = 0L
  ),
  expand.grid(
    shared = c(TRUE, FALSE),
    linDep = c(TRUE, FALSE),
    dist = 1,
    noiseCols = seq.int(5L, 50L, by = 5L)
  )
)

high_correlation_settings <- rbind(
  expand.grid(
    shared = c(TRUE, FALSE),
    linDep = NA,
    dist = c(10^(-seq.int(0L, 10L)), 0.5),
    noiseCols = 0L
  ),
  expand.grid(
    shared = c(TRUE, FALSE),
    linDep = NA,
    dist = 1,
    noiseCols = seq.int(5L, 25L, by = 5L)
  )
)

create_problem_collection <- function(setting, include_all_training_sizes) {
  dimension <- 4L + setting$noiseCols

  covariance_sample_size <- if (is.na(setting$linDep)) {
    dimension^2 + dimension
  } else if (setting$linDep) {
    dimension^2
  } else {
    2L * dimension^2
  }

  lapply(seq_len(10L), function(run) {
    generator <- create_4_class_generator(
      class_distance = setting$dist,
      shared_covariance = setting$shared,
      noise_columns = setting$noiseCols,
      covariance_sample_size = covariance_sample_size
    )

    problem <- list(
      train1000 = generator$sample(1000L),
      test = generator$sample(10000L),
      datGen = generator
    )

    if (include_all_training_sizes) {
      problem$train100 <- generator$sample(100L)
      problem$train10000 <- generator$sample(10000L)
      problem <- problem[c("train100", "train1000", "train10000", "test", "datGen")]
    }

    problem
  })
}

all_settings <- rbind(base_settings, high_correlation_settings)

for (setting_id in seq_len(nrow(all_settings))) {
  print(setting_id)
  setting <- all_settings[setting_id, , drop = FALSE]
  problems <- create_problem_collection(
    setting = setting,
    include_all_training_sizes = setting_id <= nrow(base_settings)
  )

  saveRDS(
    problems,
    file.path(output_dir, paste0("id_", setting_id, ".rds"))
  )
}

saveRDS(all_settings, settings_file)
