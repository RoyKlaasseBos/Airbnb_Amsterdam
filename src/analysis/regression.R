# your R code goes here!
library(stargazer)
getwd()
airbnb <- read.csv("../../data/data.csv")

airbnb <- airbnb %>% mutate (christmas = ifelse(month == 12, 1, 0))

model1 <- lm(num_reviews ~ month + christmas + as.factor(neighbourhood) , airbnb)
summary(model1)
dir.create("../../gen")
dir.create("../../gen/analysis")

stargazer(model1,
          type = "html",
          title = "Regression Output",    
          out="../../gen/analysis/regression.html")
