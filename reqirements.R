# =============================================================================
# requirements.R
#
# Installs all R packages required to run the SARF/CARF repository.
#
# Usage:
#   source("requirements.R")
# =============================================================================

required_packages <- c(
  "tidyverse",
  "ranger",
  "OpenML",
  "kableExtra",
  "knitr",
  'scales',
  "data.table"
)

installed <- rownames(installed.packages())

missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  message("Installing missing packages:")
  print(missing)
  install.packages(missing, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

message("Repository requirements are satisfied.")