# This script sets up a renv for the project and installs required packages

# We are using the daily CRAN snapshots from RStudio Package Manager: 
# https://packagemanager.rstudio.com/client/#/repos/1/overview
# Currently, we are using the snapshot from May 1, 2023:
# https://packagemanager.rstudio.com/cran/2023-05-01

# Select the repo snapshot:
options(repos = c(
  REPO_NAME = "https://packagemanager.rstudio.com/cran/2023-05-01"
  ))

# Install renv
install.packages("renv")

# Initialize renv for the project
# bare = TRUE: instead of installing dependencies automatically, we install packages manually
renv::init(bare = TRUE)

# Install the packages
install.packages(c(
  "tidyverse", "dplyr", "stringr", "readxl", "readr", "parallel",
  "doSNOW", "foreach", "stargazer", "jsonlite", "tibble", "lubridate"
  ))

# Take a snapshot of the renv
renv::snapshot()
