### load packages -------------------------------------------------------------
#install.packages('fastDummies')  uncomment if you dont have it yet
library(fastDummies)
library(dplyr)
library(stargazer)

### load data -----------------------------------------------------------------
data <- read.csv('../../data/data.csv')

### transform data ------------------------------------------------------------
# add trend variable for the month
data$trend <- data$month  # month already shows a number

# add dummy variables for the various neighborhoods
data <- dummy_cols(data, select_columns = 'neighbourhood')

# transform date to date format
data$date <- as.Date(data$date)

# add step dummy for COVID pandemic
data <- data %>%
  mutate(data, dummy_covid = ifelse(date >= '2020-03-01', 1, 0))

### apply regression ----------------------------------------------------------
regression_model <- lm(num_reviews ~ . - X - year - month - neighbourhood -
                         num_reviews - date, data=data)

### create directories -------------------------------------------------------
dir.create('../../gen')
dir.create('../../gen/analysis')

### output regression html ----------------------------------------------------
stargazer(regression_model, type='html', title='Regression Analysis',
          dep.var.labels="Number of Reviews", notes.label="",
          out='../../gen/analysis/regression.html')
