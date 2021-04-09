# your R code goes here!
library(data.table)
install.packages("fastDummies")
library(fastDummies)
install.packages("greybox")
library(greybox)
library(tidyverse)
install.packages("stargazer")
library(stargazer)

#read the data in
data <- fread('../../data/data.csv')

#look at the data
head(data)

#as date
data$date <- as.Date.character(data$date, format = c("%Y-%m-%d"))

#Make a trend variable of the months
data$trend <- format(data$date, "%m")

#Make dummy variables of the neighbourhoods
data$neighbourhood <- gsub(data$neighbourhood, pattern=" ",replacement="_",fixed=T)
data <- fastDummies::dummy_cols(data, select_columns = "neighbourhood")

data <- data %>% 
  rename(
    neighbourhood_Oud_Noord = `neighbourhood_Oud-Noord`, 
    neighbourhood_Oud_Oost = `neighbourhood_Oud-Oost`, 
    neighbourhood_Oostelijk_Havengebied_Indische_Buurt = `neighbourhood_Oostelijk_Havengebied_-_Indische_Buurt`, 
    neighbourhood_Noord_West = `neighbourhood_Noord-West`, 
    neighbourhood_Noord_Oost = `neighbourhood_Noord-Oost`,
    neighbourhood_Bijlmer_Centrum = `neighbourhood_Bijlmer-Centrum`, 
    neighbourhood_Bijlmer_Oost = `neighbourhood_Bijlmer-Oost`,
    neighbourhood_Buitenveldert_Zuidas = `neighbourhood_Buitenveldert_-_Zuidas`,
    neighbourhood_De_Aker_Nieuw_Sloten = `neighbourhood_De_Aker_-_Nieuw_Sloten`,
    neighbourhood_De_Pijp_Rivierenbuurt = `neighbourhood_De_Pijp_-_Rivierenbuurt`, 
    neighbourhood_Gaasperdam_Driemond = `neighbourhood_Gaasperdam_-_Driemond`, 
    neighbourhood_IJburg_Zeeburgereiland =`neighbourhood_IJburg_-_Zeeburgereiland`, 
    neighbourhood_De_Baarsjes_Oud_West = `neighbourhood_De_Baarsjes_-_Oud-West`, 
    neighbourhood_Geuzenveld_Slotermeer = `neighbourhood_Geuzenveld_-_Slotermeer`, 
    neighbourhood_Centrum_Oost = `neighbourhood_Centrum-Oost`, 
    neighbourhood_Centrum_West = `neighbourhood_Centrum-West`
  )

#Make dummy variables for december
data$trend <- as.integer(data$trend)
data$christmas <- 0
data$christmas[data$trend == 12] <- 1

#Make a regression
regression_model <- lm(num_reviews ~ trend + christmas + neighbourhood_Zuid+ 
                         neighbourhood_Westerpark + neighbourhood_Watergraafsmeer +
                         neighbourhood_Slotervaart + neighbourhood_Oud_Oost + 
                         neighbourhood_Oud_Noord + neighbourhood_Osdorp+ 
                         neighbourhood_Oostelijk_Havengebied_Indische_Buurt + 
                         neighbourhood_Noord_West + neighbourhood_Noord_Oost+
                         neighbourhood_IJburg_Zeeburgereiland + neighbourhood_Geuzenveld_Slotermeer + 
                         neighbourhood_Gaasperdam_Driemond + neighbourhood_De_Pijp_Rivierenbuurt + 
                         neighbourhood_De_Baarsjes_Oud_West+ neighbourhood_De_Aker_Nieuw_Sloten + 
                         neighbourhood_Centrum_West + neighbourhood_Centrum_Oost + neighbourhood_Buitenveldert_Zuidas + 
                         neighbourhood_Bos_en_Lommer + neighbourhood_Bijlmer_Oost + neighbourhood_Bijlmer_Centrum
                         , data = data)
summary(regression_model)

#create directories
dir.create('../../gen/analysis/', recursive = TRUE)
save(regression_model,file="../../gen/analysis/model_results.RData")
