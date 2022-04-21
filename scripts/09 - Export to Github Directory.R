##### Load Packages #####
library(tidyverse)
library(sf)
library(CPALtools)
library(geojsonio)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
#direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
#direct <- "C:/Users/Michael Lopez/Documents/"
directdb <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory
directgh <- "C:/Users/micha/Documents/"

st_layers("Data/Dallas Afterschool.gpkg")

dallasCounty <- st_read("Data/Dallas Afterschool.gpkg", layer = "Dallas County") %>%
  st_transform(crs = 4326)

highways <- st_read(paste0(directdb, "Data Library/NCTCOG/Highways_(2019).shp")) %>%
  st_transform(crs = 4326) %>%
  st_intersection(dallasCounty, .)
#plot(highways["CLASS"])

indexComp <- st_read("Data/Dallas Afterschool.gpkg", layer = "Component Index Analysis") %>%
  st_transform(crs = 4326)

rawInput <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Program All Variables") %>%
  st_transform(crs = 4326)

das_demo <- st_read("Data/ACS Demographic Metrics.geojson") %>%
  st_transform(crs = 4326)

ntx_dots <- st_read("Data/Dallas Afterschool.gpkg", layer = "Dallas County Dot Density Population") %>%
  st_transform(crs = 4326)

das_metrics <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Program All Variables") %>%
  st_transform(crs = 4326)

das_programs <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Programs") %>%
  st_transform(crs = 4326)

hhs_programs <- st_read("Data/Dallas Afterschool.gpkg", layer = "Texas HHS Afterschool Programs") %>%
  st_transform(crs = 4326)

cacfp_programs <- st_read("Data/Dallas Afterschool.gpkg", layer = "CACFP Programs") %>%
  st_transform(crs = 4326)

# export to geojson file
st_write(dallasCounty, paste0(directgh, "GitHub/dallas-afterschool/data/dallasCounty.geojson"), delete_dsn = TRUE)
st_write(highways, paste0(directgh, "GitHub/dallas-afterschool/data/dallasHighways.geojson"), delete_dsn = TRUE)

st_write(das_programs, paste0(directgh, "GitHub/dallas-afterschool/data/dasPrograms.geojson"), delete_dsn = TRUE)
st_write(hhs_programs, paste0(directgh, "GitHub/dallas-afterschool/data/hhsPrograms.geojson"), delete_dsn = TRUE)
st_write(cacfp_programs, paste0(directgh, "GitHub/dallas-afterschool/data/cacfpprograms.geojson"), delete_dsn = TRUE)

st_write(indexComp, paste0(directgh, "GitHub/dallas-afterschool/data/dasComponents.geojson"), delete_dsn = TRUE)
st_write(rawInput, paste0(directgh, "GitHub/dallas-afterschool/data/Input Attributes.geojson"), delete_dsn = TRUE)
st_write(das_demo, paste0(directgh, "GitHub/dallas-afterschool/data/ACS Demographic Metrics.geojson"), delete_dsn = TRUE)
st_write(ntx_dots, paste0(directgh, "GitHub/dallas-afterschool/data/dotdensityPopulation.geojson"), delete_dsn = TRUE)
st_write(das_metrics, paste0(directgh, "GitHub/dallas-afterschool/data/Calculated Tract Metrics.geojson"), delete_dsn = TRUE)
