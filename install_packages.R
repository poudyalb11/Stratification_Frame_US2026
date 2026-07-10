# install_packages.R
# Run this once before running any pipeline script.
# Installs all R packages required by the Stratification Frame pipeline.

packages <- c(
  "tidyverse",       # data manipulation, ggplot2, tidyr, dplyr, purrr, etc.
  "here",            # relative paths
  "data.table",      # fast reading of large CSVs
  "readxl",          # reading Excel BAF files
  "haven",           # reading Stata .dta and IPUMS data
  "tidycensus",      # querying Census API for block populations
  "rpart",           # CART models
  "rpart.plot",      # CART visualization
  "janitor"
)

# Install missing packages
missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  install.packages(missing)
}

# Optional: pin CRAN mirror to avoid interactive prompt
options(repos = c(CRAN = "https://cloud.r-project.org"))

cat("All required packages installed.\n")
