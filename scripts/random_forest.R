# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(parallel)
library(doSNOW)
library(foreach)
library(stargazer)
library(jsonlite)
library(tibble)
library(lubridate)
library(tidymodels)
library(randomForest)

# clear workspace
rm(list=ls())

# set seed
set.seed(127)

# load dataframe
df = readRDS(file.path("prepared", "df.rds")) %>%
  select(-c(X...CO2, COU, YEA, COU_YEA)) %>%
  na.omit()

# data sampling and pre-processing ----
df_split = initial_split(df)

df_split %>%
    training() %>%
    glimpse()

# training data
df_recipe = training(df_split) %>%
    recipe(Value ~ .) %>%
    step_corr(
        dist, colony, comlang_ethno, gdp_pc_ratio, tas_anom, pr_anom, conflict
    ) %>%
    step_center(
        dist, colony, comlang_ethno, gdp_pc_ratio, tas_anom, pr_anom, conflict,
        all_outcomes()
    ) %>%
    step_scale(
        dist, colony, comlang_ethno, gdp_pc_ratio, tas_anom, pr_anom, conflict,
        all_outcomes()
    ) %>%
    prep()

# execute pre-processing
df_testing = df_recipe %>%
    bake(testing(df_split))

glimpse(df_testing)

df_training = juice(df_recipe)

# model training
df_randomForest = rand_forest(
    trees = 100,
    mode = "regression"
) %>%
    set_engine("randomForest") %>%
    fit(
        Value ~ .,
        data = df_training
    )

# model prediction
df_predict = df_randomForest %>%
  predict(df_testing) %>%
  bind_cols(df_testing)

# measure performance of the model
df_predict %>% metrics(truth = Value, estimate = .pred)

# plot predicted vs. actual values
dir.create("figures", showWarnings = F)

df_predict %>%
  ggplot(
    aes(
      x = Value,
      y = .pred
    )
  ) + 
  geom_abline(
    col = "green",
    lty = 2,
    size = 1
  ) + 
  geom_point(alpha = .4)

ggsave(
  file.path(
    "figures",
    "predicted_vs_actual_randomForest.pdf"
  )
)