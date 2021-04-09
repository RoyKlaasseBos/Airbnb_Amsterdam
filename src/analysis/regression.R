# your R code goes here

##download packages 
# your R code goes here!
library(data.table)
install.packages("fastDummies")
library(fastDummies)
library(tidyverse)
library(dplyr)
library(stargazer)


#read the data in
dff <- read.csv("data.csv", sep=",")

#look at the data as a preview 

head(dff)
#View whole set

View(dff)


#there is already a trend variable for months namely month, so I will not create another one. 

#Now we will create dummy variables for neighbourhoods
dff$neighbourhood <- gsub(dff$neighbourhood, pattern=" ",replacement="_",fixed=T)

dff <- fastDummies::dummy_cols(dff, select_columns = "neighbourhood")

#Make dummy variables for december

dff <- dff %>%
  mutate(december = ifelse(month == 12, 1, 0))

#name collums 

colnames(dff) = c("x", 
                  "year",
                  "month",
                  "neighboorhoud",
                  "num_reviews",
                  "date",
                  "BijlmerC",
                  "BijlmerOost",
                  "BosLommer",
                  "Buitenveldert",
                  "CentrumOost",
                  "CentrumWest", 
                  "NieuwSloten",
                  "DeBaarjes",
                  "DePijp", 
                  "Gaasperdam",
                  "Geuzenveld",
                  "Ijburg",
                  "Noordoost",
                  "Noordwest",
                  "Indischebuurt",
                  "Osdorp",
                  "OudNoord",
                  "OudOost",
                  "Slotervaart",
                  "Watergraafsmeer",
                  "Westerpark",
                  "Zuid",
                  "december")


#As we now have the variables we need, we can create a regression model

regression_airbnb <- lm(num_reviews ~month + BijlmerC + 
                          BijlmerOost + BosLommer + Buitenveldert + 
                          CentrumOost + CentrumWest + NieuwSloten + 
                          DeBaarjes + DePijp + Gaasperdam + Geuzenveld + 
                          Ijburg + Noordoost +  Noordwest + Indischebuurt + 
                          Osdorp + OudNoord + OudOost + Slotervaart + 
                          Watergraafsmeer + Westerpark + Zuid + december 
                          , data=dff)

summary(regression_airbnb)

#also a stargazer table

stargazer(regression_airbnb,
          title = "Regression airbnb",
          dep.var.caption = "regression outcome",  
          dep.var.labels = "",  
          covariate.labels = "",  
          column.labels = c("Full model"),
          notes.label = "Significance levels",  
          type="html",
          out="output.html"  
          )



#create directories
dir.create('../../gen/analysis/', recursive = TRUE)
save(regression_airbnb,file="../../gen/analysis/regression_airbnb.RData")