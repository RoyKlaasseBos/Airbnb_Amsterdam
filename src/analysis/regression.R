#Install and load these packages
require(tidyverse)
require(fastDummies)
require(stargazer)

# Load in data
df <- read.csv("../../data/data.csv")

#Change date format with lubridate
df$date <- as.Date.character(df$date, format = c("%Y-%m-%d"))

#Make the trend variable for months
#There seems to already be a trend variable for month but I made a new one anyway
df$trend <- format(df$date, "%m")

#make dummy variables for neighbourhood
df <- dummy_cols(df, select_columns = "neighbourhood")

#Make new dummy variable for dates of March 2020 and later (COVID period)
df$COVID <- df$date>"2020-02-01"

#Convert the TRUE and FALSE to 1 and 0
df$COVID <- df$COVID *1

#Regression analysis with number of reviews as the dependent variable and month, neighbourhoods, and COVID as the indepent variables
regression <- lm(num_reviews ~ . -X -year -month -date, data = df)
summary(regression)

#Create directories gen and analysis
dir.create("../../gen")
dir.create("../../gen/analysis")

#Export regression results using stargazer package
stargazer(regression,
          title = "Results review regressions analysis",
          out = "../../gen/analysis/regression.html")

