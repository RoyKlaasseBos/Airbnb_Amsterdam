library(ggplot2)
library(broom)
library(dplyr)
library(fastDummies)
library(dummies)
install.packages("ggfortiy")
options(scipen=999)
data <- read.csv("/Users/woutervanakkeren/Desktop/Airbnb_Amsterdam/data/data.csv")

#create dummy for neighbourhood
data.new <- dummy.data.frame(data, names= c("neighbourhood"), sep= "_")

#create dummy for date
data.new$date <- as.Date(data.new$date)
data1 <- data.new %>% filter(date >= "2020-03-01")
data2 <- data.new %>% filter(date < "2020-03-01")
data1$stepdummy <- 1
data2$stepdummy <- 0
data.new <- rbind(data1, data2)

#remove unwanted variables
data.new <- select(data.new, -c(year, date, X))

# estimate linear model
mdl_reviews <- lm(num_reviews ~ . , data=data.new)
mdl_reviews

#outlier screening
leverage_influence <- mdl_reviews %>%
  augment() %>%
  select(num_reviews, month, leverage = .hat, cooks_dist = .cooksd) %>%
  arrange(desc(cooks_dist)) %>%
  head()
leverage_influence
data.new.clean <- data.new[-c(1165, 1143, 1121, 1231, 1553, 1384),]

#run new lm with clean dataset
mdl_reviews_clean <- lm(num_reviews ~ . , data=data.new.clean)


#model reporting
library(stargazer)

dir.create("gen")
dir.create('gen/analysis')

stargazer(mdl_reviews, mdl_reviews_clean,
          title = "Figure 1: Regression on the number of Airbnb reviews in Amsterdam",
          dep.var.caption = "Number of reviews",  
          dep.var.labels = "",  
          covariate.labels = c("Intercept"),  
          column.labels = c("Full model", "Outliers excluded"),
          notes.label = "Significance levels",  
          type="html",
          out="gen/analysis/regression.html"  
)