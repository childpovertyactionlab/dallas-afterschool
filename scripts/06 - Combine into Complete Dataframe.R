##### Load Packages #####
library(tidyverse)
library(sf)
library(CPALtools)
library(rio)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
#direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import each layer from Geopackage #####
st_layers("Data/Dallas Afterschool.gpkg")

# Dallas County Boundaries
dallasCounty <- st_read("Data/Dallas Afterschool.gpkg", layer = "Dallas County") %>%
  st_transform(crs = 6584)

# ACS Data for all Tracts within Dallas County
acsTract <- st_read("Data/Dallas Afterschool.gpkg", layer = "Dallas County ACS Variables") %>%
  st_transform(crs = 6584)

# Dallas County Census Tracts
dallasTract <- acsTract %>%
  select(GEOID, geom)

# Dallas After School Program Data
dallasAfter <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Programs") %>%
  st_transform(crs = 6584)

# Child and Adult Care Food Program Data 
cacfpDallas <- st_read("Data/Dallas Afterschool.gpkg", layer = "CACFP Programs (Cleaned)") %>%
  st_transform(crs = 6584)

# Texas HHS Afterschool Care Programs
hhsAfter <- st_read("Data/Dallas Afterschool.gpkg", layer = "Texas HHS Afterschool Programs (Cleaned)") %>%
  st_transform(crs = 6584)

##### Summarize Program Seats to Dallas County Tracts #####
dallasAfterSum <- dallasAfter %>%
  st_join(., dallasTract) %>%
  st_drop_geometry(.) %>%
  group_by(GEOID) %>%
  dplyr::summarise(dasPrograms = n(),
            dasSeats = sum(max_capacity)) %>%
  filter(!is.na(GEOID))

cacfpDallasSum <- cacfpDallas %>%
  filter(age_min < 18 & age_max >= 5) %>%
  filter(at_risk_afterschool_care_center == "Y") %>%
  st_join(., dallasTract) %>%
  st_drop_geometry(.) %>%
  group_by(GEOID) %>%
  dplyr::summarise(cacfpPrograms = n(),
            cacfpSeats = sum(total_participants_enrolled)) %>%
  filter(!is.na(GEOID))

hhsAfterSum <- hhsAfter %>%
  st_join(., dallasTract) %>%
  st_drop_geometry(.) %>%
  group_by(GEOID) %>%
  dplyr::summarise(hhsPrograms = n(),
            hhsSeats = sum(capacity)) %>%
  filter(!is.na(GEOID))

##### Join All Program Data to Census Tracts #####
enrichTract <- acsTract %>%
  left_join(., dallasAfterSum) %>%
  left_join(., cacfpDallasSum) %>%
  left_join(., hhsAfterSum) %>%
#  left_join(., htiTract) %>%
  mutate(dasPrograms = ifelse(is.na(dasPrograms), 0, dasPrograms),
         dasSeats = ifelse(is.na(dasSeats), 0, dasSeats),
         cacfpPrograms = ifelse(is.na(cacfpPrograms), 0, cacfpPrograms),
         cacfpSeats = ifelse(is.na(cacfpSeats), 0, cacfpSeats),
         hhsPrograms = ifelse(is.na(hhsPrograms), 0, hhsPrograms),
         hhsSeats = ifelse(is.na(hhsSeats), 0, hhsSeats),
         AreaSqMi = as.numeric(st_area(.))/2.788e+7) %>%
  select(-ends_with("M")) %>%
  mutate(eligPop = pop_5t9E+pop_10t14E,
         dasCap = dasSeats/eligPop*10000,
         cacfpCap = cacfpSeats/eligPop*10000,
         hhsCap = hhsSeats/eligPop*10000,
         dasDen = dasPrograms/AreaSqMi,
         cacfpDen = cacfpPrograms/AreaSqMi,
         hhsDen = hhsPrograms/AreaSqMi,
         eligPop = pop_5t9E+pop_10t14E,
         popDen = tot_popE/AreaSqMi,
         fambp_per = fam_bpE/100,
         mchhbp_per = mchh_bpE/100,
         smhhbp_per = ifelse(is.na(smhh_bpE/100), 0, smhh_bpE/100),
         bp_per = bp_allE/tot_popE,
         cpr = bp_u18E/pop_u18E,
         broadband = internetE-int_cellE,
         int_per = broadband/thhE,
         unemE = unemE/100,
         trvltime = travelE/tot_popE,
         snap_per = snapE/thhE,
         eligPopDen = eligPop/AreaSqMi
         )

names(enrichTract)
#plot(enrichTract["trvltime"], breaks = "jenks")
#plot(enrichTract["snap_per"], breaks = "jenks")

mutVar <- enrichTract %>%
  select(GEOID, NAME, med_incE, avg_famE, mi_smhhE, unemE, dasPrograms:hhsSeats, eligPop:cpr, int_per, trvltime, snap_per, AreaSqMi, eligPopDen, geom)

st_write(mutVar, "Data/Dallas Afterschool.gpkg", layer = "Afterschool Program All Variables", delete_layer = TRUE)

st_write(dallasTract, "Data/Dallas Afterschool.gpkg", layer = "Dallas County Tracts", delete_layer = TRUE)
