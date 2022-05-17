library(sf)
library(tidyverse)
library(CPALtools)

das_programs <- st_read(here::here("data/dasPrograms.geojson")) %>%
  st_transform(crs = "ESRI:102738")

dasComponents <- st_read(here::here("data/dasComponents.geojson")) %>%
  st_transform(crs = 4269) 

acsmetrics <- st_read("data/ACS Demographic Metrics.geojson")

dasmetrics <- st_read("data/Calculated Tract Metrics.geojson") %>%
  st_transform(crs = 4269) %>%
  select(-NAME, -mi_smhhE, -unemE, -smhhbp_per, -bp_per, -cpr, -int_per) %>%
  left_join(., st_drop_geometry(dasComponents)) %>%
  left_join(., st_drop_geometry(acsmetrics)) %>%
  mutate(#TotSeats = hhsSeats+dasSeats+cacfpSeats,
    #TotPrograms = hhsPrograms+dasPrograms+cacfpPrograms,
    TotSeats = dasSeats,
    TotPrograms = dasPrograms,
    ProgSqMi = TotPrograms/AreaSqMi,
    eligDen = eligPop/AreaSqMi)

typologytable <- dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(MainGrp) %>%
  summarise(TotalTracts = n(),
            AreaSqMi = sum(AreaSqMi),
            mhi = median(med_incE, na.rm = TRUE),
            eligPop = sum(eligPop),
            bp_u18 = sum(bp_u18E, na.rm = TRUE),
            pop_u18 = sum(pop_u18E, na.rm = TRUE),
            dasPrograms = sum(dasPrograms),
            dasSeats = sum(dasSeats),
            #cacfpPrograms = sum(cacfpPrograms),
            #cacfpSeats = sum(cacfpSeats),
            #hhsPrograms = sum(hhsPrograms),
            #hhsSeats = sum(hhsSeats),
            #totPrograms = sum(dasPrograms+cacfpPrograms+hhsPrograms),
            #totSeats = sum(dasSeats+cacfpSeats+hhsSeats)
  ) %>%
  mutate(poppro = round(eligPop/dasSeats, digits = 3),
         cpr = round(bp_u18/pop_u18, digits = 3))

typeSupply <- dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(typeSupply) %>%
  dplyr::summarise(Supply = n()) %>%
  #mutate(typeSupply = str_remove(typeSupply, " SUPPLY")) %>%
  rename(Classification = typeSupply)

typeDemand <- dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(typeDemand) %>%
  dplyr::summarise(Demand = n()) %>%
  #mutate(typeDemand = str_remove(typeDemand, " DEMAND")) %>%
  rename(Classification = typeDemand)

typeJoin <- left_join(typeSupply, typeDemand) %>%
  mutate(Demand = ifelse(is.na(Demand), 0, Demand),
         Supply = ifelse(is.na(Supply), 0, Supply),
         Order = ifelse(Classification == "VERY HIGH", 1, 
                        ifelse(Classification == "HIGH", 2, 
                               ifelse(Classification == "MODERATE", 3, 
                                      ifelse(Classification == "LOW", 4, 
                                             ifelse(Classification == "VERY LOW", 5, "ERROR")))))) %>%
  mutate(Classification = ifelse(Classification == "VERY HIGH", "Very High", 
                                 ifelse(Classification == "HIGH", "High", 
                                        ifelse(Classification == "MODERATE", "Moderate", 
                                               ifelse(Classification == "LOW", "Low", 
                                                      ifelse(Classification == "VERY LOW", "Very Low", "ERROR"))))))

#unique(dasmetrics$MainGrp)
dem_sup <- dasmetrics %>%
  filter(MainGrp == "Demand > Supply")

sup_dem <- dasmetrics %>%
  filter(MainGrp == "Demand < Supply")

demTsup <- dasmetrics %>%
  filter(MainGrp == "Demand = Supply")

priorityareas <- dasmetrics %>%
  filter(brkdiff >= 2) %>%
  filter(cpr >= mean(dasmetrics$cpr, na.rm = TRUE)) %>%
  filter(smhh_per >= mean(dasmetrics$smhh_per, na.rm = TRUE)) %>%
  filter(mhiE <= mean(dasmetrics$mhiE, na.rm = TRUE)) %>%
  filter(eligPop >= mean(dasmetrics$eligPop, na.rm = TRUE))

mean(dasmetrics$eligPop, na.rm = TRUE)
mean(dasmetrics$cpr, na.rm = TRUE)
mean(dasmetrics$smhh_per, na.rm = TRUE)
mean(dasmetrics$mhiE, na.rm = TRUE)

dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(Typology) %>%
  summarize(count = n())

dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(brkdiff) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = brkdiff, y = percent)) +
  geom_bar(stat = "identity") +
  theme_light()


dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(typeDemand) %>%
  summarize(count = n())

dasmetrics %>%
  st_drop_geometry(.) %>%
  group_by(typeSupply) %>%
  summarize(count = n())

cpr <- dem_sup %>%
  arrange(desc(cpr)) %>%
  slice_head(n = 40) %>%
  mutate(top_cpr = "Top 40 CPR") %>%
  st_transform(crs = "ESRI:102738")

smhh <- dem_sup %>%
  arrange(desc(smhh_per)) %>%
  slice_head(n = 40) %>%
  mutate(top_smhh = "Top 40 SMHH") %>%
  st_transform(crs = "ESRI:102738")

plot(cpr["geometry"])
plot(smhh["geometry"])

cpr %>%
  summarise(TotalTracts = n(),
            AreaSqMi = sum(AreaSqMi),
            pop_u18 = sum(pop_u18E),
            mhi = median(med_incE, na.rm = TRUE),
            eligPop = sum(eligPop),
            bp_u18 = sum(bp_u18E, na.rm = TRUE),
            pop_u18 = sum(pop_u18E, na.rm = TRUE),
            dasPrograms = sum(dasPrograms),
            dasSeats = sum(dasSeats)) %>%
  mutate(bp_u18/pop_u18)

smhh %>%
  summarise(TotalTracts = n(),
            AreaSqMi = sum(AreaSqMi),
            pop_u18 = sum(pop_u18E),
            mhi = median(med_incE, na.rm = TRUE),
            eligPop = sum(eligPop),
            bp_u18 = sum(bp_u18E, na.rm = TRUE),
            pop_u18 = sum(pop_u18E, na.rm = TRUE),
            dasPrograms = sum(dasPrograms),
            dasSeats = sum(dasSeats)) %>%
  mutate(bp_u18/pop_u18)

cpr_programs <- das_programs[cpr, ] %>%
  mutate(top = "Top 40 CPR")

smhh_programs <- das_programs[smhh, ] %>%
  mutate(top = "Top 40 SMHH")

tot_programs <- rbind(cpr_programs, smhh_programs)

tot_programs %>%
  st_drop_geometry(.) %>%
  rio::export("data/Top 40 Tracts SMHH and CPR.csv")

test <- dasmetrics %>%
  filter(indexSupply >= 75)

sum(dasmetrics$dasPrograms)
sum(dasmetrics$dasSeats)


sum(test$dasPrograms)
sum(test$dasSeats)

sum(test$dasPrograms)/sum(dasmetrics$dasPrograms)
sum(test$dasSeats)/sum(dasmetrics$dasSeats)

sum(test$eligPop)/sum(dasmetrics$eligPop)

sum(dasmetrics$eligPop)-sum(test$eligPop)

