##### Load Packages #####
library(tidyverse)
library(sf)
library(CPALtools)
library(rio)
library(DescTools)
library(e1071)

##### Directory Selection #####
#direct <- "C:/Users/OwenWilson-Chavez/CPAL Dropbox/" # Owen Wilson-Chavez Laptop Directory
#direct <- "E:/CPAL Dropbox/" # Michael Lopez Desktop Directory
direct <- "C:/Users/micha/CPAL Dropbox/" # Michael Lopez Laptop Directory

##### Import each layer from Geopackage #####
st_layers("Data/Dallas Afterschool.gpkg")

compAll <- st_read("Data/Dallas Afterschool.gpkg", layer = "Afterschool Program All Variables") %>%
  st_drop_geometry(.) %>%
  select(-NAME) %>%
  mutate(GEOID = as.numeric(GEOID))

##### Conduct skewnesss test in order to examine distribution of data #####
# Ideal skewness should be betwee -1 and 1 for any given variable
sapply(compAll, e1071::skewness, na.rm = TRUE)

##### Winsorize any variables that appear to have significant outliers based on skewness test #####
winsAll <- compAll %>%
  mutate(dasCap   = Winsorize(dasCap  , na.rm = TRUE),
         cacfpCap = Winsorize(cacfpCap, na.rm = TRUE),
         hhsCap = Winsorize(hhsCap, na.rm = TRUE)
         )

sapply(winsAll, e1071::skewness, na.rm = TRUE)

##### Normalize data with shapiro test and box cox transformation if necessary
sapply(winsAll, shapiro.test)

MASS::boxcox(lm(winsAll$med_incE~1))
MASS::boxcox(lm(winsAll$mi_smhhE~1))
MASS::boxcox(lm((winsAll$unemE+0.0001)~1))
MASS::boxcox(lm((winsAll$dasPrograms+0.0001)~1))
MASS::boxcox(lm((winsAll$dasSeats+0.0001)~1))
MASS::boxcox(lm((winsAll$cacfpPrograms+0.0001)~1))
MASS::boxcox(lm((winsAll$cacfpSeats+0.0001)~1))
MASS::boxcox(lm((winsAll$hhsPrograms+0.0001)~1))
MASS::boxcox(lm((winsAll$hhsSeats+0.0001)~1))
MASS::boxcox(lm((winsAll$eligPop+0.0001)~1))

MASS::boxcox(lm((winsAll$dasCap+0.0001)~1))
MASS::boxcox(lm((winsAll$cacfpCap+0.0001)~1))
MASS::boxcox(lm((winsAll$hhsCap+0.0001)~1))

MASS::boxcox(lm((winsAll$cacfpDen+0.0001)~1))
MASS::boxcox(lm((winsAll$hhsDen+0.0001)~1))
MASS::boxcox(lm((winsAll$dasDen+0.0001)~1))

MASS::boxcox(lm((winsAll$popDen+0.0001)~1))

MASS::boxcox(lm((winsAll$fambp_per+0.0001)~1))
MASS::boxcox(lm((winsAll$mchhbp_per+0.0001)~1))
MASS::boxcox(lm((winsAll$smhhbp_per+0.0001)~1))

MASS::boxcox(lm((winsAll$bp_per+0.0001)~1))
MASS::boxcox(lm((winsAll$cpr+0.0001)~1))

MASS::boxcox(lm(winsAll$int_per~1))
MASS::boxcox(lm((winsAll$snap_per+0.0001)~1))
MASS::boxcox(lm(winsAll$AreaSqMi~1))
MASS::boxcox(lm((winsAll$eligPopDen+0.0001)~1))

##### Descriptive Stats for Each Dataframe #####
var <-  c("nbr.val", "nbr.null", "nbr.na", "min", "max", "range", "sum", "median", "mean", "SE.mean", "CI.mean.0.95", "var", "std.dev", "coef.var")
descAll <- cbind(var, pastecs::stat.desc(winsAll))

##### Normalize any Variables that did not pass previous test #####
normAll <- winsAll %>%
  mutate(med_incE = log(med_incE+0.00001),
         mi_smhhE = log(mi_smhhE+0.00001),
         unemE = sqrt(unemE+0.00001)*-1,
         dasPrograms = log(dasPrograms+0.00001),
         dasSeats = log(dasSeats+0.00001),
         cacfpPrograms = log(cacfpPrograms+0.00001),
         cacfpSeats = log(cacfpSeats+0.00001),
         hhsPrograms = log(hhsPrograms+0.00001),
         hhsSeats = log(hhsSeats+0.00001),
         eligPop = sqrt(eligPop+0.00001),
         
         dasCap = log(dasCap+0.00001),
         cacfpCap = log(cacfpCap+0.00001),
         hhsCap = log(hhsCap+0.00001),
         
         cacfpDen = log(cacfpDen+0.00001),
         hhsDen = log(hhsDen+0.00001),
         dasDen = log(dasDen+0.00001),

         popDen = sqrt(popDen+0.00001),
         fambp_per = sqrt(fambp_per+0.00001)*-1,
         mchhbp_per = log(mchhbp_per+0.00001)*-1,
         smhhbp_per = log(smhhbp_per+0.00001)*-1,

         bp_per = sqrt(bp_per+0.00001)*-1,
         cpr = sqrt(cpr+0.00001)*-1,
         
         int_per = (int_per+0.00001)^2,
         snap_per = sqrt(snap_per+0.00001)*-1,
         eligPopDen = sqrt(eligPopDen+0.00001)*-1
  )

##### Export Tables #####
rio::export(normAll, "Data/Processed and Cleaned Data.csv")

