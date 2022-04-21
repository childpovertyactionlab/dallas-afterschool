library(tidyverse)
library(sf)
library(tidycensus)
library(CPALtools)
library(rio)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
#direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import Dallas Afterschool Program Data #####
afterschool <- import("Data/Dallas Afterschool Programs (2021-2022).csv") %>%
  mutate(latitude = ifelse(latitude == 0, NA, latitude),
         longitude = ifelse(longitude == 0, NA, longitude))

##### Filter out programs without latitude/longitude data and geocode #####
afterschool_co <- afterschool %>%
  filter(!is.na(longitude))

afterschool_na <- afterschool %>%
  filter(is.na(longitude)) %>%
  select(-longitude, -latitude) %>%
  mutate(fulladdress = paste(billing_street, billing_city, ",", billing_zip)) %>%
  tidygeocoder::geocode(address = fulladdress,
                        method = "arcgis") %>%
  select(-fulladdress) %>%
  rename(longitude = long,
         latitude = lat)

##### Join geocoded information into original dataset then transform into an sf object #####
afterschool_geo <- full_join(afterschool_co, afterschool_na) %>%
  st_as_sf(coords = c(x = "longitude", y = "latitude"),
           crs = 4326)

plot(afterschool_geo["max_capacity"])

##### Export to geopackage #####
st_write(afterschool_geo, "Data/Dallas Afterschool.gpkg", layer = "Afterschool Programs", delete_layer = TRUE)

##### Generate Plots to Explore Data #####
afterschool %>%
  group_by(facility_type) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count),
         facility_type = ifelse(facility_type == "", "NA", facility_type),
         facility_type = factor(facility_type, levels = c("Apartment Complex", "Child Care Center", "Community Center/Nonprofit", "Faith Community", "Municipal Building", "School", "NA"))) %>%
  ggplot(aes(x = facility_type, y = percent)) +
  geom_bar(stat = "identity", fill = "#008097") +
  geom_text(aes(label=count), color = "#6c6c6c", size = 10, position=position_dodge(width=0.9), vjust=-0.25, fontface = "bold") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.45)) +
  labs(
    title = "Facility Types in Dallas Afterschool Programs",
    subtitle = "",
    x = "",
    y = "",
    color = 'VARIABLE'
  ) +
  theme_cpal() +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        text = element_text(size = 24),
        legend.position = "none",
        axis.line = element_line(size = 0.7))

ggsave("Graphics/Facility Type Distribution.jpg", units = "px", width = 1600, height = 1000)

afterschool %>%
  group_by(fees_type) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count),
         fees_type = ifelse(fees_type == "", "NA", fees_type),
         fees_type = factor(fees_type, levels = c("Fee", "No Charge", "Sliding Scale", "NA"))) %>%
  ggplot(aes(x = fees_type, y = percent)) +
  geom_bar(stat = "identity", fill = "#008097") +
  geom_text(aes(label=count), color = "#6c6c6c", size = 10, position=position_dodge(width=0.9), vjust=-0.25, fontface = "bold") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.65)) +
  labs(
    title = "Fee Types in Dallas Afterschool Programs",
    subtitle = "",
    x = "",
    y = "",
    color = 'VARIABLE'
  ) +
  theme_cpal() +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        text = element_text(size = 24),
        legend.position = "none",
        axis.line = element_line(size = 0.7))

ggsave("Graphics/Fee Type Distribution.jpg", units = "px", width = 1600, height = 1000)

afterschool %>%
  group_by(advocacy_type) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count),
         advocacy_type = ifelse(advocacy_type == "", "NA", advocacy_type),
         advocacy_type = factor(advocacy_type, levels = c("For Profit", "Local Nonprofit", "National Nonprofit", "School Run", "NA"))) %>%
  ggplot(aes(x = advocacy_type, y = percent)) +
  geom_bar(stat = "identity", fill = "#008097") +
  geom_text(aes(label=count), color = "#6c6c6c", size = 10, position=position_dodge(width=0.9), vjust=-0.25, fontface = "bold") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.5)) +
  labs(
    title = "Advocacy Types in Dallas Afterschool Programs",
    subtitle = "",
    x = "",
    y = "",
    color = 'VARIABLE'
  ) +
  theme_cpal() +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        text = element_text(size = 24),
        legend.position = "none",
        axis.line = element_line(size = 0.7))

ggsave("Graphics/Advocacy Type Distribution.jpg", units = "px", width = 1600, height = 1000)