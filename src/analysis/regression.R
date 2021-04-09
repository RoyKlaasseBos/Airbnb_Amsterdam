# install package if needed
# install.packages("fastDummies")

# load package
library(fastDummies)
library(stargazer)

# create directories
dir.create('../../gen')
dir.create('../../gen/analysis')

# load data
df <- read.csv("../../data/data.csv")

# create dummies for neighbourhood
df <- dummy_cols(df, select_columns='neighbourhood')

# create dummy COVID
df$date <- as.Date(df$date)

COVID <- vector()
for (i in 1:length(df$date)) {
  if (df$date[i] >= '2020-03-01') {
    COVID[i] <- 1 }
  else {COVID[i] <- 0}
}

df$COVID <- COVID

# run regression
regression_model <- lm(num_reviews ~ month + neighbourhood + COVID, data=df)
summary(regression_model)

# export regression estimates 
stargazer(regression_model, type='html', title='Regression analysis',
          dep.var.labels="Number of reviews", notes.label="",
          out='../../gen/analysis/regression_analysis.html')
