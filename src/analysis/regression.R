# your R code goes here!
library(dplyr)
library(fastDummies)
library(broom)
library(stargazer)

data <- read.csv("data/data.csv")
data <- dummy_cols(data, select_columns = 'neighbourhood') 
data <- mutate(data, covid = ifelse(data$year >= 2020 & data$month >= 3 , '1', '0'))
airbnb_lm <- lm(num_reviews ~ month + covid + `neighbourhood_Bijlmer-Centrum` + `neighbourhood_Bijlmer-Oost` + `neighbourhood_Bos en Lommer` + `neighbourhood_Buitenveldert - Zuidas` + `neighbourhood_Centrum-Oost` + `neighbourhood_Centrum-West` + `neighbourhood_De Aker - Nieuw Sloten` + `neighbourhood_De Baarsjes - Oud-West` + `neighbourhood_De Pijp - Rivierenbuurt` + `neighbourhood_Gaasperdam - Driemond` + `neighbourhood_Geuzenveld - Slotermeer` + `neighbourhood_IJburg - Zeeburgereiland` + `neighbourhood_Noord-Oost` + `neighbourhood_Noord-West` + `neighbourhood_Oostelijk Havengebied - Indische Buurt` + `neighbourhood_Osdorp` + `neighbourhood_Oud-Noord` + `neighbourhood_Oud-Oost` + `neighbourhood_Slotervaart` + `neighbourhood_Watergraafsmeer` + `neighbourhood_Westerpark` + `neighbourhood_Zuid`, data = data)


stargazer(airbnb_lm, title='Regression Results', type = 'html', out='gen/analysis/regression.html')
