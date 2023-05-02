library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(readxl)

rm(list=ls())

df = read.csv(
  "raw/MIG_02052023143458762.csv",
  header = T
  ) %>% select(
    X...CO2,
    COU,
    YEA,
    Value
  )

gdp_pc = read.csv(
  "raw/75d59fa2-c7f9-4cb3-899f-ae8ab410f1e3_Data.csv",
  header = T
)

distances = read_xls("raw/dist_cepii.xls", col_names = T)

df$gdp_pc_origin = NA
df$gdp_pc_dest = NA
df$dist = NA
for(i in 1:nrow(df)) {
  df$gdp_pc_origin[i] = as.numeric(
    gdp_pc %>% 
      filter(Country.Code == df$X...CO2[i]) %>% 
      select(contains(as.character(df$YEA[i])))
    )
  
  df$gdp_pc_dest[i] = as.numeric(
    gdp_pc %>% 
      filter(Country.Code == df$COU[i]) %>% 
      select(contains(as.character(df$YEA[i])))
  )
  
  df$dist[i] = as.numeric(
    distances %>%
      filter(iso_o == df$X...CO2[i] &
             iso_d == df$COU[i]) %>%
      select(dist)
  )
}
rm(i)

df$gdp_pc_ratio = df$gdp_pc_dest / df$gdp_pc_origin

df$COU_YEA = paste0(df$COU, df$YEA)

gravity_model = glm(
  formula = Value ~ gdp_pc_ratio + dist + as.factor(X...CO2) + as.factor(COU_YEA),
  family = quasipoisson(link = log),
  data = df
)
