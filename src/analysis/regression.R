library(dplyr)
install.packages('here')
library(here)
install.packages('dummies')
library(dummies)
library(stargazer)

# 3. Include code (src/analysis/regression.R) to estimate a regression model. 
# As a dependent variable, use the number of Airbnb reviews in Amsterdam. As independent variables, please add the following:
#   Trend variable for the month (January = 1, February = 2, etc.)
# Dummy variables for the various neighborhoods 
# Step dummy for the COVID pandemic (1 if the date is March 2020 or later and 0 otherwise)
# 
# 4. Modify your code to export the regression estimates with the stargazer package (use the html format) to gen/analysis/regression.html so that I can immediately paste the results into my manuscript.

# Loading in the data
df <- read.csv(here("data", "data.csv"))

# Creating a matrix containing all dummy variables and scroe
dummies <- as.data.frame(dummy(data=df, x='neighbourhood'))

# Merging dummy matrix and df, removing old neighbourhood and X variable
df2 <- bind_cols(df, dummies) %>% 
  select(-c(neighbourhood, X))

# Cleaning the colnames a bit. White spaces need to be removes, snake_casing is optional but recommended.
colnames(df2) <- gsub('neighbourhood', '', colnames(df2))
colnames(df2) <- gsub(' - ','-', colnames(df2))
colnames(df2) <- gsub(' ', '_', colnames(df2))
colnames(df2) <- tolower(colnames(df2))

# Changing date type to work with
df2$date <- as.Date(df2$date)

# Adding column if covid (1) or not (0)
df2 <- df2 %>%
  mutate('covid'=ifelse(df2$date>="2020-03-01",'1', '0'))
  
# Removing variables not used for lineair model make coding easier and quicker :)
lm_df <- df2[,-c(1,4)]
  
# Creating regression model
model <- lm(num_reviews ~., lm_df)

# Creating directories
dir.create(here("gen"))
dir.create(here("gen", "analysis"))

# outputting html
stargazer(model, title="Linear model estimation", type='html', out=here("gen", "analysis", "regression.html"))



