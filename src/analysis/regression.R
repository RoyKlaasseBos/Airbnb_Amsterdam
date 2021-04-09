# Load needed packages for the regression
library(fastDummies)
library(dplyr)
library(stargazer)
library(data.table)

### directories to be created
dir.create('../Airbnb_Amsterdam/gen')
dir.create('../Airbnb_Amsterdam/gen/analysis')

### load in the data needed
data <- fread('../Airbnb_Amsterdam/data/data.csv')

### transformation of the data: change month, add dummy for different neighbourhoods, transform date and add another dummy for Covid
data$trend <- data$month

data <- dummy_cols(data, select_columns = 'neighbourhood')
data$date <- as.Date(data$date)
data <- data %>%
  mutate(data, covid_dummy = ifelse(date >= '2020-03-01', 1, 0))

### Do the regression analysis
regression <- lm(num_reviews~trend + neighbourhood + covid_dummy, data=data)

### output in stargazar/html form
stargazer(regression, type='html', title='Question 11',
          dep.var.labels="Number of Reviews", notes.label="", style = 'commadefault',
          out='../Airbnb_Amsterdam/gen/analysis/regression.html')
          