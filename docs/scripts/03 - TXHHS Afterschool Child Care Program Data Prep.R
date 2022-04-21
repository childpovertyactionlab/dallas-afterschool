library(tidyverse)
library(sf)
library(tidycensus)
library(CPALtools)
library(rio)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
#direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import Texas HHS Afterschool Child Care Program Data #####
hhsafterschool <- import("Data/Texas HHS Afterschool Child Care (5 and Up).csv") %>%
  janitor::clean_names() %>%
  filter(county == "DALLAS")

hhs_geo <- hhsafterschool %>%
  mutate(fulladdress = paste(address, city, state, zip)) %>%
  tidygeocoder::geocode(address = fulladdress,
                        method = "arcgis") %>%
  select(-fulladdress)

hhs_sf <- hhs_geo %>%
  st_as_sf(coords = c(x = "long", y = "lat"),
           crs = 4326)

plot(hhs_sf["city"])

unique(hhs_sf$type)

##### Export to geopackage #####
st_write(hhs_sf, "Data/Dallas Afterschool.gpkg", layer = "Texas HHS Afterschool Programs", delete_layer = TRUE)
