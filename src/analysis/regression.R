# your R code goes here!

library(readr)
getwd()
setwd('..')
getwd()
setwd('..')
getwd()
airbnbdata <- read_csv("data/data.csv")
View(airbnbdata)

#Dummy for neighbourhoods
#install.packages("dummies")
library(broom)
library(dummies)
library(dplyr)
library(fastDummies)


airbnbdata <- dummy_cols(airbnbdata, select_columns = 'neighbourhood')
# Step dummy for christmas holiday
airbnbdata <- airbnbdata %>% mutate(christmasholiday = ifelse(airbnbdata$month == 12, 1, 0))


# Estimate linear model
multi_regmodel <- lm(num_reviews ~ month + christmasholiday + `neighbourhood_Bijlmer-Centrum` + `neighbourhood_Bijlmer-Oost` + `neighbourhood_Bos en Lommer` + `neighbourhood_Buitenveldert - Zuidas` + `neighbourhood_Centrum-Oost` + `neighbourhood_Centrum-West` + `neighbourhood_De Aker - Nieuw Sloten` + `neighbourhood_De Baarsjes - Oud-West` + `neighbourhood_De Pijp - Rivierenbuurt` + `neighbourhood_Gaasperdam - Driemond` + `neighbourhood_Geuzenveld - Slotermeer` + `neighbourhood_IJburg - Zeeburgereiland` + `neighbourhood_Noord-Oost` + `neighbourhood_Noord-West` + `neighbourhood_Oostelijk Havengebied - Indische Buurt` + `neighbourhood_Osdorp` + `neighbourhood_Oud-Noord` + `neighbourhood_Oud-Oost` + `neighbourhood_Slotervaart` + `neighbourhood_Watergraafsmeer` + `neighbourhood_Westerpark` + `neighbourhood_Zuid`, data = airbnbdata)

summary(multi_regmodel)
```
#Make gen/analysis file
dir.create('../gen/analysis')


# stargazer 
library(tidyverse)
library(stargazer)

stargazer(multi_regmodel,
          title = "Regression Analysis",
          dep.var.caption = "Number of reviews",  
          dep.var.labels = "",  
          column.labels = c("Full model", "Outlier excluded"),
          notes.label = "Significance levels",  
          type="html",
          out="./../gen/analysis/regression.html"  
          )


