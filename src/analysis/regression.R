# --------- Regression ------------------ #

#install.packages("dummies")
library(dummies)
library(dplyr)
library(stargazer)


# load data
airbnb <- read.csv("../../data/data.csv", sep = ',', na.strings = c("", "NA"))

#make dummy variables for the various neighborhoods 
table(airbnb$neighbourhood)
airbnb$neighbourhood <- as.factor(airbnb$neighbourhood)
airbnb <- dummy.data.frame(airbnb, "neighbourhood", sep = "_")

#make dummy for the COVID pandemic (1 if the date is March 2020 or later and 0 otherwise)
airbnb$date_dummy <- ifelse(airbnb$date > as.Date("2020-02-29", format = "%Y-%m-%d"),1,0)



colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Buitenveldert - Zuidas')] <- 'neighboorhood_Buitenvelder_Zuidas'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Oostelijk Havengebied - Indische Buurt')] <- 'neighbourhood_Oostelijk_Havengebied_Indische_Buurt'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_De Baarsjes - Oud-West')] <- 'neighbourhood_De_Baarsjes_Oud_West'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Gaasperdam - Driemond')] <- 'neighbourhood_Gaasperdam_Driemond'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_De Pijp - Rivierenbuurt')] <- 'neighbourhood_De_Pijp_Rivierenbuurt'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Geuzenveld - Slotermeer')] <- 'neighbourhood_Geuzenveld_Slotermeer'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_De Aker - Nieuw Sloten')] <- 'neighbourhood_De_Aker_Nieuw_Sloten'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Bos en Lommer')] <- 'neighbourhood_Bos_en_Lommer'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_IJburg - Zeeburgereiland')] <- 'neighbourhood_IJburg_Zeeburgereiland'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Bijlmer-Oost')] <- 'neighbourhood_Bijlmer_Oost'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Bijlmer-Centrum')] <- 'neighboorhood_Bijlmer_Centrum'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Oud-Noord')] <- 'neighbourhood_Oud_Noord'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Centrum-West')] <- 'neighbourhood_Centrum_West'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Noord-Oost')] <- 'neighbourhood_Noord_Oost'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Oud-Oost')] <- 'neighbourhood_Oud_Oost'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Oud-Noord')] <- 'neighbourhood_Oud_Noord'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Centrum-Oost')] <- 'neighbourhood_Centrum_Oost'
colnames(airbnb)[which(colnames(airbnb)=='neighbourhood_Noord-West')] <- 'neighbourhood_Noord_West'

# set regression

mdl_airbnb <-
  lm(
    num_reviews ~ neighboorhood_Bijlmer_Centrum +
    neighbourhood_Bijlmer_Oost +
    neighbourhood_Bos_en_Lommer+
    neighboorhood_Buitenvelder_Zuidas +
    neighbourhood_Centrum_Oost +
    neighbourhood_Centrum_West +
    neighbourhood_De_Aker_Nieuw_Sloten +
    neighbourhood_De_Baarsjes_Oud_West +
    neighbourhood_De_Pijp_Rivierenbuurt +
    neighbourhood_Gaasperdam_Driemond +
    neighbourhood_Geuzenveld_Slotermeer +
    neighbourhood_IJburg_Zeeburgereiland +
    neighbourhood_Noord_Oost +
    neighbourhood_Noord_West +
    neighbourhood_Oostelijk_Havengebied_Indische_Buurt +
    neighbourhood_Osdorp +
    neighbourhood_Oud_Noord +
    neighbourhood_Oud_Oost +
    neighbourhood_Slotervaart +
    neighbourhood_Watergraafsmeer +
    neighbourhood_Westerpark +
    neighbourhood_Zuid +
    date_dummy +
    month,
    data = airbnb)

summary(mdl_airbnb)

stargazer('../../gen/analysis/regression', mdl_airbnb, type = "html")
