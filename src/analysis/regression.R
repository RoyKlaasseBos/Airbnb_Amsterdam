
# installing packages
print("Installing needed packages...")

if(!require(fastDummies)){
  install.packages("fastDummies",repos = "http://cran.us.r-project.org")
  library(fastDummies)
}

if(!require(stargazer)){
  install.packages("stargazer",repos = "http://cran.us.r-project.org")
  library(stargazer)
}

if(!require(here)){
  install.packages("here",repos = "http://cran.us.r-project.org")
  library(here)
}


# loading packages
print("Loading needed packages...")

library(dplyr)
library(data.table)
library(tidyverse)


######################
### LOADING DATA  ####
######################


# loading data
print("Loading Airbnb data from Amsterdam...")
print("To get in the Amsterdam mood: https://www.youtube.com/watch?v=L4Wd8CdF-hQ - Sevn Alias - In Amsterdam ft. Maan (Prod. Esko)")

data <- read.csv(here("data","data.csv"))


######################
###### DUMMIES  ######
######################


# Creating dummies for neighbourhood
print("Creating dummies...")
data$neighbourhood <- gsub(data$neighbourhood, pattern=" ", replacement="_", fixed=TRUE)
data <- fastDummies::dummy_cols(data, select_columns = "neighbourhood")

# Creating stepdummy for the Christmas holidays
print("To get in the Christmas mood: https://www.youtube.com/watch?v=aAkMkVFwAo - Mariah Carey - All I Want for Christmas Is You")
data <- data %>%
  mutate(christmas = ifelse(month == 12, 1, 0))


######################
#### COLUMNNAMES #####
######################

# Creating column names
colnames(data) = c("nr",
                      "year",
                      "month",
                      "neighbourhood",
                      "nrreviews",
                      "bijlmercentrum",
                      "bijlmeroost",
                      "bosenlommer",
                      "buitenveldertzuidas",
                      "centrumoost",
                      "centrumwest",
                      "deakernieuwsloten",
                      "debaarsjesoudwest",
                      "depijprivierenbuurt",
                      "gaasperdamdriemond",
                      "geuzenveldslotermeer",
                      "ijburgzeeburgereiland",
                      "noordoost",
                      "noordwest",
                      "oostelijkhavengebied",
                      "osdorp",
                      "oudnoord",
                      "oudoost",
                      "slotervaart",
                      "watergraafsmeer",
                      "westerpark",
                      "zuid",
                      "christmas")


######################
#### REGRESSION ######
######################


# Creating regression analysis
print("Creating regression...")
print("Yeah my inspiration was a little bit lost at this point...   https://www.youtube.com/watch?v=Aza3CQ31jI8 - Statistics Rap [Linear Regression]")

regression <- lm(nrreviews ~month + bijlmercentrum + bijlmeroost + bosenlommer + buitenveldertzuidas + centrumoost + centrumwest + deakernieuwsloten + debaarsjesoudwest + depijprivierenbuurt + gaasperdamdriemond + geuzenveldslotermeer + ijburgzeeburgereiland + noordoost + noordwest + oostelijkhavengebied + osdorp + oudnoord + oudoost + slotervaart + watergraafsmeer + westerpark + zuid + christmas, data)
summary(regression)


# Create GEN directory and saving regression file
print("Creating 'gen' directory...")
dir.create(here("gen"))
dir.create(here("gen", "analysis"))


######################
##### STARGAZER ######
######################


stargazer(regression,
          title = "Regression Analysis Airbnb Ams",
          dep.var.caption = "# of reviews",
          dep.var.labels = "",
          covariate.labels = "",
          notes.align = NULL,
          type = "html",
          out = "../../gen/analysis/regression_estimates.html")


