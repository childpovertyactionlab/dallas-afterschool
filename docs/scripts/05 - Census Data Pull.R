library(tidyverse)
library(sf)
library(tidycensus)
library(CPALtools)
library(rio)

acs20_b <- load_variables(2020, "acs5")
acs20_s <- load_variables(2020, "acs5/subject")

#list of the necessary variables to pull
acs_var <- c(
  tot_pop = "S1701_C01_001", #total population
  tot_fam = "S1702_C01_002", #total families (children under 18)
  fam_bp = "S1702_C02_002", #total families below poverty
  smhh = "S1702_C05_002", #single-mother household (children under 18)
  smhh_bp = "S1702_C06_002", #single-mother household below poverty (children under 18)
  mchh = "S1702_C03_002", #married-couple household (children under 18)
  mchh_bp = "S1702_C04_002", #married-couple household below poverty (children under 18)
  thh = "S1101_C01_001", #total households
  avg_fam = "S1101_C01_004", #average family size
  pop_u18 = "S1701_C01_002", #population under 18
  pop_u5 = "S0101_C01_002", #population under 5
  pop_5t9 = "S0101_C01_003", #population between 5 and 9
  pop_10t14 = "S0101_C01_004", #population between 10 and 14
  mi_smhh = "S1903_C03_021", #single-mother family median income (children under 18)
  med_inc = "B19013_001", #median household income
  bp_all = "S1701_C02_001", #total population below poverty
  bp_u18 = "S1701_C02_002", #population under 18 below poverty
  bp_u5 = "S1701_C02_003", #population under 5 below poverty
  unem ="S2301_C04_001", #unemployment rate
  internet = "S2801_C01_012", #total households with an internet subscription
  int_cell = "S2801_C01_016", #total households with only cell phone internet subscription
  travel = "B08013_001", #average travel time to work
  snap = "B19058_002" #total households receiving public assistance or food stamps/SNAP
  )

acs_tract <- get_acs(geography = "tract", 
                     state = "TX",
                     county = "Dallas County",
                     variables = acs_var,
                     year = 2020, 
                     survey = "acs5", 
                     output = "wide",
                     geometry = TRUE)

#st_crs(acs_tract)

st_write(acs_tract, "Data/Dallas Afterschool.gpkg", layer = "Dallas County ACS Variables", delete_layer = TRUE)
