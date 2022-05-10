library(tidyverse)
library(sf)


dasComponents <- st_read(here::here("data/dasComponents.geojson")) %>%
  st_transform(crs = "ESRI:102738") %>%
  select(GEOID, indexSupply, indexDemand, MainGrp)

names(dasComponents)

das_programs <- st_read(here::here("data/dasPrograms.geojson")) %>%
  st_transform(crs = "ESRI:102738")

programAtt <- st_join(das_programs, dasComponents)

programAtt %>%
  st_drop_geometry(.) %>%
  rio::export("data/DAS Programs with Index Attributes.csv")

#### priority areas and programs
dallasCounty <- st_read(here::here("data/dallasCounty.geojson")) %>%
  st_transform(crs = "ESRI:102738")

dasComponents <- st_read(here::here("data/dasComponents.geojson")) %>%
  st_transform(crs = "ESRI:102738") 

acsmetrics <- st_read("data/ACS Demographic Metrics.geojson")

dasmetrics <- st_read("data/Calculated Tract Metrics.geojson") %>%
  st_transform(crs = "ESRI:102738") %>%
  select(-NAME, -mi_smhhE, -unemE, -smhhbp_per, -bp_per, -cpr, -int_per) %>%
  left_join(., st_drop_geometry(dasComponents)) %>%
  left_join(., st_drop_geometry(acsmetrics)) %>%
  mutate(#TotSeats = hhsSeats+dasSeats+cacfpSeats,
    #TotPrograms = hhsPrograms+dasPrograms+cacfpPrograms,
    TotSeats = dasSeats,
    TotPrograms = dasPrograms,
    ProgSqMi = TotPrograms/AreaSqMi,
    eligDen = eligPop/AreaSqMi)

das_programs <- st_read(here::here("data/dasPrograms.geojson")) %>%
  st_transform(crs = "ESRI:102738") %>%
  .[dallasCounty, ]

priorityareas_dem <- dasmetrics %>%
  filter(brkdiff >= 2) %>%
  filter(mhiE <= mean(dasmetrics$mhiE, na.rm = TRUE)) %>%
  filter(eligPop >= mean(dasmetrics$eligPop, na.rm = TRUE)) %>%
  filter(cpr >= mean(dasmetrics$cpr, na.rm = TRUE)) %>%
  st_buffer(dist = 250)

priorityprog_dem <-   das_programs[priorityareas_dem, ] %>%
  mutate(AREATYPE = "In High Demand")

priorityareas_sup <- dasmetrics %>%
  filter(brkdiff <= -2) %>%
  filter(mhiE >= mean(dasmetrics$mhiE, na.rm = TRUE)) %>%
  filter(eligPop <= mean(dasmetrics$eligPop, na.rm = TRUE)) %>%
  filter(cpr <= mean(dasmetrics$cpr, na.rm = TRUE))

priorityprog_sup <-   das_programs[priorityareas_sup, ] %>%
  mutate(AREATYPE = "In High Supply")

sum(priorityareas_sup$dasPrograms)
sum(priorityareas_sup$dasSeats)
sum(priorityareas_sup$eligPop)

count(priorityareas_dem)
sum(priorityareas_dem$dasPrograms)
sum(priorityareas_dem$dasSeats)
sum(priorityareas_dem$eligPop)

priorityprog <- rbind(priorityprog_dem, priorityprog_sup)

rio::export(priorityprog, "data/DAS Programs in Priority Areas.csv")
