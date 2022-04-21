library(tidyverse)
library(sf)

dasComponents <- st_read(here::here("data/dasComponents.geojson")) %>%
  st_transform(crs = 4269) %>%
  select(GEOID, indexSupply, indexDemand, MainGrp)

names(dasComponents)

das_programs <- st_read(here::here("data/dasPrograms.geojson")) %>%
  st_transform(crs = 4269)

programAtt <- st_join(das_programs, dasComponents)

programAtt %>%
  st_drop_geometry(.) %>%
  rio::export("data/DAS Programs with Index Attributes.csv")
