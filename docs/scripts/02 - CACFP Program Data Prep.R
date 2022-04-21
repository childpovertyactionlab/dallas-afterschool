library(tidyverse)
library(sf)
library(tidycensus)
library(CPALtools)
library(rio)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
#direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import CACFP Program Data #####
CACFP_programs <- rio::import("Data/CACFP_Centers_2021-2022.csv") %>%
  janitor::clean_names() %>%
  mutate(geo_loc_data = str_remove(geo_loc_data, "POINT ")) %>%
  filter(geo_loc_data != "") %>%
  separate(geo_loc_data, into = c("lat", "lon"), sep = " ") %>%
  mutate(lat = str_replace(lat, "[()]", ""),
         lon = str_replace(lon, "[()]", "")) %>%
  select(ceid, type_of_agency:type_of_org, 
         ce_status, site_id:site_terminated_as_of_date, 
         site_county, site_street_address_line1:site_street_address_zip_code, 
         child_care, outside_school_hours, head_start:lon) %>%
  filter(site_county == "DALLAS") %>%
  separate(age_range_of_participants, into = c("age_min", "age_max"), sep = " - ") %>%
  mutate(age_min = as.numeric(word(age_min, 1)),
         age_max = as.numeric(word(age_max, 1)),
         site_city = toupper(site_street_address_city)) %>%
  st_as_sf(., coords = c("lat", "lon"), crs = 4326)

names(CACFP_programs)

plot(CACFP_programs["site_city"])

st_write(CACFP_programs, "Data/Dallas Afterschool.gpkg", layer = "CACFP Programs")
