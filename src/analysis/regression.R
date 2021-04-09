# Trend variable for the month
#df <- dataset$month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#Date<-c("January","February", "March","April","May","June", "July", "August", "September", "October", "November", "December")
#dataset <- data.frame(df, Date)
#dataset

#Read csv file repository
#df_1 = read.csv("data.csv")
#df_1

#Regression analysis stargazer
#install.packages("stargazer")
#linear.1 <- lm(num_reviews ~ df + dummy_cols2 + step_dummy,
               data=attitude)
#linear.2 <- lm(num_reviews ~ df + dummy_cols2 + step_dummy, data=attitude)
## create an indicator dependent variable, and run a probit model
#df_1$highnum_reviews <- (df_1$num_reviews > 70)
#regression.model <- glm(highnum_reviews ~ df + dummy_cols2 + step_dummy, data=attitude,
                    family = binomial(link = "probit"))

#library(stargazer)

#stargazer(dummy_cols2, summary=FALSE, rownames=FALSE)

#FINAL ATTEMPT
library(dplyr)
library(fastDummies)
library(stargazer)

#Read csv file repository
data_csv = read.csv("data.csv")
data_csv

#Dummy variables for various neighborhoods
#dummycolumns
dummy_cols(data_csv, select_columns = "neighboorhood")
dummy_cols

#add to dataset
data_csv <- dummy_cols
data_csv

#Get Stepdummy
getSeason <- function(df_1$date)
  WS <- as.Date("2015-03-31", format = "%Y-%m-%d") # march
SE <- as.Date("2015-03-31",  format = "%Y-%m-%d") #all else

d <- as.Date(strftime(df_1, format="%Y-%m-%d"))


ifelse (d >= WS | d < SE, 1,
        ifelse (0))
#this did not work

regression_airbnb <- lm(num_reviews ~ month + covid + d + "neighbourhood")

stargazer(regression_airbnb, title="regression airbnb Amsterdam", typ="html", out="gen/analysis/regression")