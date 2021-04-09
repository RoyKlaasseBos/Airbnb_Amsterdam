df <- read.csv('../../data/data.csv')

library(dplyr)
library(reshape2)
library(caret)
df$christmas <- ifelse(df$month == 12, 1, 0)
dmy <- dummyVars(~ neighbourhood, data = df)
neighbourhooddummy <- predict(dmy, newdata = df)
df <- cbind(df, neighbourhooddummy)
df1 <- df %>% select(3,5,7,8:29)

airbnb <- lm(num_reviews ~ .-num_reviews, data = df1)

dir.create('../../gen')
dir.create('../../gen/analysis')

library(stargazer)
stargazer(airbnb, title = "Figure 1: Regression estimates",
          type = "html", out = "../../gen/analysis/regression.html")
