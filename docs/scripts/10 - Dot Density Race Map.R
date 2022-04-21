library(tidyverse)
library(CPALtools)
library(tidycensus)
library(sf)

datalib <- "C:/Users/micha/CPAL Dropbox/"
#datalib <- "E:/CPAL Dropbox/"

# Identify Variables for Mapping
race_vars <- c(
  Hispanic = "P2_002N",
  White = "P2_005N",
  Black = "P2_006N",
  Asian = "P2_008N"
)

# Get Data from Tidycensus
ntx_race <- get_decennial(
  geography = "tract",
  variables = race_vars,
  state = "TX",
  county = "Dallas County",
  geometry = TRUE,
  year = 2020
)

# Convert into Dots
ntx_dots <- as_dot_density(
  ntx_race,
  value = "value",
  values_per_dot = 50,
  group = "variable"
)

st_write(ntx_dots, "Data/Dallas Afterschool.gpkg", layer = "Dallas County Dot Density Population", delete_layer = TRUE)
