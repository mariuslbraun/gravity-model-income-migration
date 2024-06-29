# This R code produces an example of a simple gravity model of migration from
# Sub-Saharan African countries to OECD countries for the period 1997-2020.
# Data on migration flows is taken from the OECD International Migration 
# Database, and data on distance, common languages and former colonial
# relationships is taken from the GeoDist CEPII database. GDP per capita data
# is obtained from the World Development Indicators by the World Bank.
# Data on temperature and precipitation in countries of origin is obtained
# via the World Bank Climate Change Knowledge Portal API, and data on intra-
# state conflict is taken from the Uppsala Conflict Data Program.

# load packages
source("scripts/load_packages.R")

# clear workspace
rm(list=ls())



#### load migration data ####
df = read.csv(
  "raw/MIG_02052023143458762.csv",
  header = T
) %>% select(
    CO2,
    COU,
    YEA,
    Value
  ) %>%
  rename(mig_flow = Value)



#### load dyadic country variables ####
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
    CO2 == iso_o, # origin country
    COU == iso_d # destination country
  ),
  keep = FALSE
)



#### load GDP per capita data ####
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
  by = join_by(CO2 == Country.Code, YEA == year),
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



#### load climate data ####
# create vector of climate variable names (temperature and precipitation)
climate_vars = c("tas", "pr")

# create country code vector from data frame
countrycodes = toString(unique(df$CO2)) %>%
  str_remove_all(" ")

# download climate data using CCKP API
for(i in 1:length(climate_vars)) {
  # data frame name
  data_name = paste(
    climate_vars[i],
    "data",
    sep = "_"
  )
  
  # API query (saves as JSON file)
  download.file(
    url = paste0(
      "https://cckpapi.worldbank.org/cckp/v1/cru-x0.5_timeseries_",
      climate_vars[i],
      "_timeseries_annual_1901-2022_mean_historical_cru_ts4.07_mean/",
      countrycodes,
      "?_format=json"
    ),
    destfile = file.path("raw", paste0(data_name, ".json"))
  )
  
  # load JSON file into workspace
  climate_data = as.data.frame(
    t(
      do.call(
        cbind,
        (
          read_json(
            file.path("raw", paste0(data_name, ".json")),
            simplifyVector = T
          )
        )$data
      )
    )
  ) %>% tibble::rownames_to_column(
    var = "country"
  ) %>% pivot_longer(
    cols = !country,
    names_to = "year",
    values_to = "value"
  )
  
  # convert year and value to numeric
  climate_data$year = as.numeric(
    str_replace_all(
      climate_data$year,
      "-07",
      ""
    )
  )
  climate_data$value = as.numeric(climate_data$value)
  
  # calculate climatic anomalies relative to 20th century
  climate_data$anom = (climate_data$value - mean(climate_data$value)) /
    sd(climate_data$value)
  
  climate_data = climate_data %>% rename_with(
   .fn = ~ paste(climate_vars[i], "anom", sep = "_"),
   .cols = ends_with("anom")
  )
  
  # join with data frame
  df = left_join(
    df,
    climate_data,
    by = join_by(
      CO2 == country,
      YEA == year
    )
  )
  df = df %>% select(
    -value
  )
}
rm(i, data_name, climate_data)



#### add intra-state conflict data #####
# read XLS file into workspace
conflict_data = read_csv(
  file = "raw/micFINAL.csv",
  col_names = T
) %>% select(
  conflict_name,
  start_event,
  end_event
)

# clean up conflict names in order to match with data frame
conflict_data$conflict_name = str_replace(
  conflict_data$conflict_name,
  "\\(.*",
  ""
)
conflict_data$conflict_name = trimws(
  conflict_data$conflict_name,
  which = "right"
)

# load lookup table to replace country names with 3-letter country codes
lookup_countrycodes = read_csv(
  file = "interim/lookup_countrycodes.csv",
  col_names = T
)

# join country codes with conflict data
conflict_data = left_join(
  x = conflict_data,
  y = lookup_countrycodes,
  by = join_by(
    conflict_name == country
  )
)

# view countries that could not be matched
unique(
  (conflict_data %>%
     filter(is.na(conflict_data$countrycode))
   )$conflict_name
)

# convert conflict start and end dates to year
conflict_data$start_event = mdy(conflict_data$start_event)
conflict_data$start_event = year(conflict_data$start_event)
conflict_data$end_event = mdy(conflict_data$end_event)
conflict_data$end_event = year(conflict_data$end_event)

# create conflict dummy
df$conflict = NA
for(i in 1:nrow(df)) {
  df$conflict[i] = as.numeric(
    sum(
      conflict_data$countrycode == df$CO2[i] &
      conflict_data$start_event <= df$YEA[i] &
      conflict_data$end_event >= df$YEA[i],
      na.rm = T
    ) > 0
  )
}
rm(i, conflict_data, lookup_countrycodes)

# save data frame as RDS file
saveRDS(df, "prepared/df.rds")
