##### Load Packages #####
library(tidyverse)
library(sf)
library(CPALtools)
library(rio)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
#direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import each layer from Geopackage #####
st_layers("Data/Dallas Afterschool.gpkg")

##### Import Datasets #####
standard <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Program All Variables")
index <- st_read("Data/Dallas Afterschool.gpkg", layer = "Component Index Analysis")

##### Make Maps #####
names(standard)
plot(standard["dasCap"], breaks = "jenks")
plot(standard["cacfpCap"], breaks = "jenks")
plot(standard["hhsCap"], breaks = "jenks")
plot(standard["dasDen"], breaks = "jenks")
plot(standard["cacfpDen"], breaks = "jenks")
plot(standard["hhsDen"], breaks = "jenks")

names(index)
plot(index["indexSupply"], breaks = "jenks")
