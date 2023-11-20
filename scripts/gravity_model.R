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

# clear workspace
rm(list=ls())



# load dataframe
df = readRDS(file.path("prepared", "df.rds"))

# baseline formula
baseline_formula = "mig_flow ~ gdp_pc_ratio + dist + factor(CO2) + factor(COU_YEA)"

# additional covariates
covars = colnames(df)[
  ! colnames(df) %in%
    c("CO2", "COU", "YEA", "mig_flow", "gdp_pc_ratio", "dist", "COU_YEA")
]

# create folder to store model RDS files
dir.create("models", showWarnings = F)

# prepare parallelized computation
cl = makeCluster(detectCores() - 2, type = "SOCK") # make cluster
registerDoSNOW(cl) # register cluster
packages = c(
  "tidyverse", "dplyr", "stringr", "readxl", "readr", "parallel",
  "doSNOW", "foreach"
)

# estimate models, consecutively adding covariates
gravity_output = foreach(
  i = 1:(length(covars) + 1),
  .packages = packages
) %dopar% {
  # create model name and add covariates to baseline formula
  if(i == 1) {
    # no covariates
    gravity_model_name = "gravity_model"
  } else {
    # name model according to covariate added
    gravity_model_name = paste(
      "gravity_model",
      covars[i - 1],
      sep = "_"
    )
    # add covariate to baseline formula
    baseline_formula = paste(
      baseline_formula,
      str_replace_all(
        toString(covars[1:(i - 1)]),
        ", ",
        " + "
      ),
      sep = " + "
    )
  }
  
  # estimate gravity model
  assign(
    x = gravity_model_name,
    value = glm(
      formula = as.formula(baseline_formula),
      family = quasipoisson(link = log),
      data = df
    )
  )
  
  # save model as RDS file
  # saveRDS(
  # object = get(gravity_model_name),
  # file = file.path("models", paste0(gravity_model_name, ".rds"))
  # )
  return(get(gravity_model_name))
}
stopCluster(cl)
rm(cl, packages)



#### create LaTeX regression table ####
# create directory for LaTeX table
dir.create("tables", showWarnings = F)

# write regression output to LaTeX table
writeLines(
  capture.output(
    stargazer(
      gravity_output,
      dep.var.labels = "Bilateral migration flow",
      column.sep.width = "0pt",
      omit.stat = c("f", "ser"),
      omit = c("CO2", "COU_YEA"),
      no.space = TRUE
    )
  ),
  "tables/gravity_models.tex"
)
rm(gravity_output)
