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

dallas_sf <- tigris::counties(state = "TX") %>%
  filter(GEOID == 48113) %>%
  st_transform(crs = 6584)

# Dallas After School Program Data
dallasAfter <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Programs") %>%
  st_transform(crs = 6584)

# Child and Adult Care Food Program Data 
cacfpDallas <- st_read("Data/Dallas Afterschool.gpkg", layer = "CACFP Programs") %>%
  st_transform(crs = 6584)

# Texas HHS Afterschool Care Programs
hhsAfter <- st_read("Data/Dallas Afterschool.gpkg", layer = "Texas HHS Afterschool Programs") %>%
  st_transform(crs = 6584)

dallasmini <- dallasAfter %>%
  select(account_name, account_id, billing_street:billing_zip) %>%
  rename(site_name = account_name,
         site_id = account_id,
         address = billing_street,
         city = billing_city,
         zip = billing_zip) %>%
  mutate(site_name = str_to_upper(site_name),
         address = str_to_upper(address),
         city = str_to_upper(city),
         site_id = str_to_upper(site_id),
         lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
#names(dallasmini)
  
cacfpmini <- cacfpDallas %>%
  select(ceid, site_name, site_street_address_line1, site_street_address_city, site_street_address_zip_code, site_license_number) %>%
  rename(site_name = site_name,
         site_id = site_license_number,
         address = site_street_address_line1,
         city = site_street_address_city,
         zip = site_street_address_zip_code) %>%
  mutate(site_name = str_to_upper(site_name),
         address = str_replace(str_to_upper(address), "[^[:alnum:]]", " "),
         city = str_to_upper(city),
         site_id = str_to_upper(site_id),
         lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
#names(cacfpmini)

hhsmini <- hhsAfter %>%
  select(operation_number, operation_caregiver_name:zip, facility_id) %>%
  rename(site_name = operation_caregiver_name,
         address = address,
         city = city,
         zip = zip,
         site_id = facility_id
         ) %>%
  mutate(site_name = str_to_upper(site_name),
         address = str_to_upper(address),
         city = str_to_upper(city),
         site_id = str_to_upper(site_id),
         lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
#names(hhsmini)

#####
hhsUnique <- inner_join(st_drop_geometry(hhsmini), st_drop_geometry(dallasmini), by = "site_name")
 
cacfpUnique <- inner_join(st_drop_geometry(cacfpmini), st_drop_geometry(dallasmini), by = "site_name")

##### 
hhsDive <- hhsAfter %>%
  mutate(cap_name = str_to_upper(operation_caregiver_name)) %>%
  filter(!cap_name %in% hhsUnique$site_name)

cacfpDive <- cacfpDallas %>%
  mutate(cap_name = str_to_upper(site_name)) %>%
  filter(!cap_name %in% cacfpUnique$site_name) %>%
  filter(site_status != "INACTIVE") %>%
  filter(at_risk_afterschool_care_center == "Y") %>%
  filter(site_termination_status == "") %>%
  filter(age_max > 5) %>%
  filter(age_min < 18) %>%
  .[dallas_sf, ]

plot(cacfpDive["age_min"])
plot(hhsDive["capacity"])
plot(dallasAfter["max_capacity"])

# Dallas After School Program Data
st_write("Data/Dallas Afterschool.gpkg", layer = "Afterschool Programs")

# Child and Adult Care Food Program Data 
st_write(cacfpDive, "Data/Dallas Afterschool.gpkg", layer = "CACFP Programs (Cleaned)")

# Texas HHS Afterschool Care Programs
st_write(hhsDive, "Data/Dallas Afterschool.gpkg", layer = "Texas HHS Afterschool Programs (Cleaned)")
