# 1. Packages
library(tidyverse)
library(dplyr)
# install.packages("fastDummies")
library(fastDummies)
library(stargazer)


# 2. Data
airbnb <- read.csv("data/data.csv", sep = ',')
airbnb <- as.data.frame(airbnb)


# 3. Transformation
# Date format
airbnb$date <- as.Date.character(airbnb$date, format = c("%Y-%m-%d"))

# Trend variable for months
airbnb$trend <- format(airbnb$date, "%m")

# Dummy variables for neighbourhood
airbnb <- dummy_cols(airbnb, select_columns = "neighbourhood")

# Step dummy variable for holiday period
airbnb$holidays <- airbnb$month == "12"
airbnb$holidays <- airbnb$holidays *1


# 4. Regression Analysis
regression <- lm(num_reviews ~ . -X -year -month -date, data = airbnb)
summary(regression)


# 5. Directories
dir.create("gen")
dir.create("gen/analysis")


# 6. Output Export
stargazer(regression,
          title = "Results review regressions analysis",
          out = "gen/analysis/regression.html")