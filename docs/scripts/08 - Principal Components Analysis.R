##### Load Packages #####
library(tidyverse)
library(sf)
library(CPALtools)
library(rio)
library(factoextra)
library(ggfortify)
library(ggbiplot)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
#direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import each layer from Geopackage #####
st_layers("Data/Dallas Afterschool.gpkg")

dallasTract <- st_read("Data/Dallas Afterschool.gpkg", layer = "Dallas County Tracts")

normAll <- rio::import("Data/Processed and Cleaned Data.csv") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(-popDen, -AreaSqMi, -(cacfpPrograms:hhsSeats), -(cacfpCap:hhsCap), -(cacfpDen:hhsDen), -eligPop)
#  select(-popDen, -AreaSqMi)

names(normAll)

GEOID <- normAll %>%
  select(GEOID)

scaleVariables <- left_join(dallasTract, normAll)

# Explore variables and determine total number of principal components
normData <- normAll %>%
  select(-GEOID)

#### Perform Principal Components Analysis on each component and evaluate correct dimensions #####
meanPCA <- function(vec) {
  m <- mean(vec, na.rm = TRUE)
  vec[is.na(vec)] <- m
  return(vec)
}

meanAll <- sapply(normAll[,c(2:18)], meanPCA)

pcaAll <- prcomp(meanAll, center = TRUE, scale = FALSE)  #scale false as normalization has already occurred
summary(pcaAll)                                                      #proportion of variance is the important indicator, it reveals which PC are the most valued.
fviz_eig(pcaAll)                                                     #scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(pcaAll,                                                 #plots all variables against first and sfamd dimension to visualize contribution of each variable to PCA.
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

ggbiplot(pcaAll, ellipse=TRUE, obs.scale = 1, var.scale = 1)
ggbiplot(pcaAll, ellipse=TRUE, choices=c(3,4), obs.scale = 1, var.scale = 1)

get_eigenvalue(pcaAll)

##### PCA by variable, extract quality of representation (cos2) of each variable ######
#cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
varAll <- get_pca_var(pcaAll)
varAllName <- rownames(varAll$cos2)
VcosAll <- as_tibble(cbind(varAllName, varAll$cos2))   

##### PCA by individual, extract quality of representation (cos2) of each campus #####
#cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
indAll <- get_pca_ind(pcaAll)
indAllCos <- cbind(GEOID, as_tibble(indAll$cos2))     #coordinates
cosAllI <- left_join(normAll, indAllCos)

##### Generate Index for each Component #####
summary(pcaAll)
#view(cosAllI)
name_rows(cosAllI)
VcosAll
colnames(cosAllI)

eigentable <- VcosAll %>%
  mutate(Dim.1 = round(as.numeric(Dim.1), digits = 3),
         Dim.2 = round(as.numeric(Dim.2), digits = 3),
         Dim.3 = round(as.numeric(Dim.3), digits = 3),
         diff1st = ifelse(Dim.1 >= Dim.2, "ONE", "TWO"),
         diff2nd = ifelse(Dim.2 >= Dim.3, "TWO", "THREE")) %>%
  select(varAllName, Dim.1, Dim.2, Dim.3, diff1st, diff2nd)

indexAll <- cosAllI %>%
  mutate(compSupply = ((dasCap+dasSeats+dasPrograms+dasDen)*Dim.1),
         compDemand = (((eligPopDen+trvltime+avg_famE+int_per+snap_per)*Dim.2)+((smhhbp_per+mchhbp_per+med_incE+cpr+fambp_per)*Dim.3))) %>%
  select(GEOID, compDemand, compSupply)

hist(indexAll$compSupply, breaks = 50)
hist(indexAll$compDemand, breaks = 50)

##### Join Component Index #####
stdIndex <- indexAll %>%
  mutate(compSupply = ((compSupply-min(compSupply, na.rm = TRUE))/(max(compSupply, na.rm = TRUE)-min(compSupply, na.rm = TRUE))),
         compDemand = ((compDemand-min(compDemand, na.rm = TRUE))/(max(compDemand, na.rm = TRUE)-min(compDemand, na.rm = TRUE))))

hist(stdIndex$compSupply, breaks = 50)
hist(stdIndex$compDemand, breaks = 50)

shapiro.test(stdIndex$compSupply)
shapiro.test(stdIndex$compDemand)

MASS::boxcox(lm((stdIndex$compSupply+0.000001)~1))
MASS::boxcox(lm((stdIndex$compDemand+0.000001)~1))

##### Normalize Index #####
skewIndex <- stdIndex %>%
  mutate(compDemand = compDemand^2)

hist(skewIndex$compSupply, breaks = 100)
hist(skewIndex$compDemand, breaks = 100)

##### Scale Each Census Tract #####
jenkDemand <- BAMMtools::getJenksBreaks(skewIndex$compDemand, k = 6)
jenkSupply <- BAMMtools::getJenksBreaks(skewIndex$compSupply, k = 6)

scaleIndex <- skewIndex %>%
  mutate(indexSupply = round(scales::rescale(compSupply, to = c(0, 100), from = range(compSupply, na.rm = TRUE))),
         indexDemand = round(scales::rescale(compDemand, to = c(0, 100), from = range(compDemand, na.rm = TRUE))),
         brkSupply = cut(compSupply, breaks = jenkSupply, right = FALSE, labels = FALSE, include.lowest = TRUE),
         typeSupply = ifelse(brkSupply == "5", "VERY HIGH",
                             ifelse(brkSupply == "4", "HIGH",
                                    ifelse(brkSupply == "3", "MODERATE",
                                           ifelse(brkSupply == "2", "LOW",
                                                  ifelse(brkSupply == "1", "VERY LOW", "ERROR"))))),
         brkDemand = cut(compDemand, breaks = jenkDemand, right = FALSE, labels = FALSE, include.lowest = TRUE),
         typeDemand = ifelse(brkDemand == "5", "HIGH",
                             ifelse(brkDemand == "4", "HIGH",
                                    ifelse(brkDemand == "3", "MODERATE",
                                           ifelse(brkDemand == "2", "LOW",
                                                  ifelse(brkDemand == "1", "VERY LOW", "ERROR"))))),
         types = paste0(typeSupply, " - ", typeDemand),
         Typology = ifelse(str_detect(types, "NA"), NA, types),
         MainGrp = ifelse(brkDemand > brkSupply, "Demand > Supply",
                          ifelse(brkDemand < brkSupply, "Demand < Supply",
                                 ifelse(brkDemand == brkSupply, "Demand = Supply", "ERROR"))),
         GEOID = as.character(GEOID)) %>%
  select(-types)

scaleIndex %>%
  group_by(MainGrp) %>%
  dplyr::summarise(n())

##### Join to Tracts and Plot #####
dallasTract <- st_read("Data/Dallas Afterschool.gpkg", layer = "Dallas County Tracts") %>%
  left_join(., scaleIndex)

plot(dallasTract["indexSupply"], breaks = "jenks")
plot(dallasTract["indexDemand"], breaks = "jenks")

plot(dallasTract["MainGrp"], breaks = "jenks")
plot(dallasTract["Typology"])

##### Export to Geopackage #####
st_write(dallasTract, "Data/Dallas Afterschool.gpkg", layer = "Component Index Analysis", delete_layer = TRUE)
