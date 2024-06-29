output_time <- system.time({
  # activate renv
  renv::activate()

  # pre-process data
  source("scripts/01_data_preprocessing.R")

  # estimate gravity model
  source("scripts/02_gravity_model.R")

  # train random forest model
  source("scripts/03_random_forest.R")
})
