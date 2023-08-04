# This R code produces an example of a simple gravity model of migration from
# Sub-Saharan African countries to OECD countries for the period 1997-2020.
# Data on migration flows is taken from the OECD International Migration 
# Database, and data on distance, common languages and former colonial
# relationships is taken from the GeoDist CEPII database. GDP per capita data
# is obtained from the World Development Indicators by the World Bank.

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

# clear workspace
rm(list=ls())



#### 1. load migration data ####
df = read.csv(
  "raw/MIG_02052023143458762.csv",
  header = T
) %>% select(
    X...CO2,
    COU,
    YEA,
    Value
  )



#### 2. load dyadic country variables ####
distances = read_xls(
  "raw/dist_cepii.xls",
  col_names = T
) %>% select(
    iso_o, iso_d, dist, colony, comlang_ethno
  )

# join dyadic country variables with migration data
df = left_join(
  df,
  distances,
  by = join_by(
    X...CO2 == iso_o, # origin country
    COU == iso_d # destination country
  ),
  keep = FALSE
)



#### 3. load GDP per capita data ####
gdp_pc = read.csv(
  "raw/75d59fa2-c7f9-4cb3-899f-ae8ab410f1e3_Data.csv",
  header = T
) %>% select(
  -c(Country.Name, Series.Name, Series.Code)
  )

# clean up column names
colnames(gdp_pc) = str_remove(
  gsub(".*..YR","",colnames(gdp_pc)),
  "\\.$"
)

# convert GDP per capita data to long format
gdp_pc = gdp_pc %>%
  pivot_longer(
    cols = `1995`:`2021`,
    names_to = "year",
    values_to = "gdp_pc"
)

# convert year column to numeric
gdp_pc$year = as.numeric(gdp_pc$year)

# join GDP per capita data with data frame
# for origin and destination country GDP per capita
df = left_join( # origin country
  df,
  gdp_pc,
  by = join_by(X...CO2 == Country.Code, YEA == year),
  keep = FALSE
) %>% 
  dplyr::rename("gdp_pc_origin" = "gdp_pc")

df = left_join( # destination country
    df,
    gdp_pc,
    by = join_by(COU == Country.Code, YEA == year),
    keep = FALSE
) %>%
  dplyr::rename("gdp_pc_dest" = "gdp_pc")

# convert GDP per capita columns to numeric
df$gdp_pc_origin = as.numeric(df$gdp_pc_origin)
df$gdp_pc_dest = as.numeric(df$gdp_pc_dest)

# remove distance and GDP per capita data frames
rm(distances, gdp_pc)

# create origin-destination GDP per capita ratio
df$gdp_pc_ratio = df$gdp_pc_dest / df$gdp_pc_origin
df = df %>% select(-c(gdp_pc_origin, gdp_pc_dest))

# concatenate destination country and year
df$COU_YEA = paste0(df$COU, df$YEA)



#### 4. estimate gravity model ####
# baseline formula
baseline_formula = "Value ~ gdp_pc_ratio + dist + factor(X...CO2) + factor(COU_YEA)"

# additional covariates
covars = c("", "colony", "comlang_ethno")

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
  i = 1:length(covars),
  .packages = packages
  ) %dopar% {
  # create model name
  gravity_model_name = paste(
    "gravity_model",
    covars[i],
    sep = "_"
  )
  
  # add covariates to baseline formula
  if(i > 1) {
    baseline_formula = paste(
      baseline_formula,
      covars[i],
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
  
  # # save model as RDS file
  # saveRDS(
  #   object = get(gravity_model_name),
  #   file = file.path("models", paste0(gravity_model_name, ".rds"))
  # )
  return(get(gravity_model_name))
}
stopCluster(cl)
rm(cl, packages)


#### 5. create LaTeX regression table ####
# create directory for LaTeX table
dir.create("tables", showWarnings = F)

# write regression output to LaTeX table
writeLines(
  capture.output(
    stargazer(
      gravity_output,
      dep.var.labels = "Bilateral migration rate",
      column.sep.width = "0pt",
      omit.stat = c("f", "ser"),
      omit = c("X...CO2", "COU_YEA"),
      no.space = TRUE
      )
  ),
  "tables/gravity_models.tex"
)
rm(gravity_output)