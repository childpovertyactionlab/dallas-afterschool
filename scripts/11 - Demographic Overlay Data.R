#####  Load Libraries #####
library(tidycensus)
library(tidyverse)
library(sf)
#library(directlabels)
library(CPALtools)
#library(terra)
#library(magick)

datalib <- "E:/CPAL Dropbox/" # Michael Desktop Directory
#datalib <- "C:/Users/micha/CPAL Dropbox/"


##### Classify Objects and ACS Variables #####
acs_s <- load_variables(2020, "acs5/subject", cache = TRUE)
acs_b <- load_variables(2020, "acs5", cache = TRUE)

acs_var <- c(
  tot_pop = "B03002_001", #total population
  pop_u18 = "S1701_C01_002", #population under 18
  pop_bp = "S1701_C02_001", #population below poverty
  bp_u18 = "S1701_C02_002", #population under 18 below poverty
  wh_pop = "B03002_003", #white non-hispanic population under 18
  bl_pop = "B03002_004", #black population under 18
  hi_pop = "B03002_012", #hispanic population under 18
  as_pop = "B03002_006", #asian population under 18
  thh = "S2801_C01_001", #total households
  internet = "S2801_C01_012", #total households with an internet subscription
  int_cell = "S2801_C01_016", #total households with only cell phone internet subscription
  fam_u18 = "S1702_C01_002", #total family households with children under 18
  smhh = "S1702_C05_002", #single-mother household (children under 18)
  smhh_bp = "S1702_C06_002", #single-mother household below poverty (children under 18)
  mi_smhh = "S1903_C03_021", #single-mother family median income (children under 18)
  unem ="S2301_C04_001", #unemployment rate
  mhi = "B19013_001" #median household income
)

##### Pull Data from 2010 to 2020 #####
das_demo <- get_acs(geography = "tract", 
                    variables = acs_var,
                    state = "TX",
                    county = "Dallas County",
                    year = 2020,
                    survey = "acs5", 
                    output = "wide",
                    geometry = TRUE) %>%
  mutate(wh_per = wh_popE/tot_popE,
         bl_per = bl_popE/tot_popE,
         as_per = as_popE/tot_popE,
         hi_per = hi_popE/tot_popE,
         cpr = bp_u18E/pop_u18E,
         smhh_bpE = smhh_bpE/100,
         smhh_per = smhhE/fam_u18E,
         
         smhhbp_per = smhh_bpE/100,
         bp_per = pop_bpE/tot_popE,
         broadband = internetE-int_cellE,
         int_per = broadband/thhE,
         unemE = unemE/100
         
         )

##### Export to Data Folder
st_write(das_demo, "Data/ACS Demographic Metrics.geojson", delete_dsn = TRUE)
