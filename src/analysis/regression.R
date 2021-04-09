# your R code goes here!

```{r}
#Read in dataset
df <- read.csv("data/data.csv")

#Create dummy variables
install.packages("mlr")
library(mlr)
df_dum <- createDummyFeatures(df$neighbourhood, cols = "neighbourhood")
summer <- ifelse(df$month == 6 | df$month == 7 | df$month == 8, 1, 0)
neighb_1 <- ifelse(df$neighbourhood =="Bijlmer-Centrum", 1, 0)
neighb_2 <- ifelse(df$neighbourhood =="Bijlmer-Oost", 1, 0)
neighb_3 <- ifelse(df$neighbourhood =="Bos en Lommer", 1, 0)
neighb_4 <- ifelse(df$neighbourhood =="Buitenvelder - Zuidas", 1, 0)
neighb_5 <- ifelse(df$neighbourhood =="Centrum-Oost", 1, 0)
neighb_6 <- ifelse(df$neighbourhood =="De Aker - Nieuw Sloten", 1, 0)
neighb_7 <- ifelse(df$neighbourhood =="De Baarsjes - Oud-West", 1, 0)
neighb_8 <- ifelse(df$neighbourhood =="De Pijp - Rivierenbuurt", 1, 0)
neighb_9 <- ifelse(df$neighbourhood =="Gaasperdam - Driemond", 1, 0)
neighb_10 <- ifelse(df$neighbourhood =="Geuzenveld - Slotermeer", 1, 0)
neighb_11 <- ifelse(df$neighbourhood =="IJburg - Zeeburgereiland", 1, 0)
neighb_12 <- ifelse(df$neighbourhood =="Noord-Oost", 1, 0)
neighb_13 <- ifelse(df$neighbourhood =="Noord-West", 1, 0)
neighb_14 <- ifelse(df$neighbourhood =="Oostelijk Havengebied - Indische Buurt", 1, 0)
neighb_15 <- ifelse(df$neighbourhood =="Osdorp", 1, 0)
neighb_16 <- ifelse(df$neighbourhood =="Oud-Noord", 1, 0)
neighb_17 <- ifelse(df$neighbourhood =="Oud-Oost", 1, 0)
neighb_18 <- ifelse(df$neighbourhood =="Slotervaart", 1, 0)
neighb_19 <- ifelse(df$neighbourhood =="Watergraafsmeer", 1, 0)
neighb_20 <- ifelse(df$neighbourhood =="Westerparkr", 1, 0)
neighb_21 <- ifelse(df$neighbourhood =="Zuid", 1, 0)

df_reg <- data.frame(X = df$X, year = df$year, summer = summer, month = df$month, neighb_1 = neighb_1, neighb_2 = neighb_2, neighb_3 = neighb_3, neighb_4 = neighb_4, neighb_5 = neighb_5, neighb_6 = neighb_6, neighb_7 = neighb_7, neighb_8 = neighb_8, neighb_9 = neighb_9, neighb_10 = neighb_10, neighb_11 = neighb_11, neighb_12 = neighb_12, neighb_13 = neighb_13, neighb_14 = neighb_14, neighb_15 = neighb_15, neighb_16 = neighb_16, neighb_17 = neighb_17, neighb_18 = neighb_18, neighb_19 = neighb_19, neighb_20 = neighb_20, neighb_21 = neighb_21, num_reviews = df$num_reviews, date = df$date)

model <- lm(num_reviews ~ summer + month + neighb_1 + neighb_2 + neighb_3 + neighb_4 + neighb_5 + neighb_6 + neighb_7+ neighb_8 + neighb_9+ neighb_10+ neighb_11+ neighb_12 + neighb_13 + neighb_14 + neighb_15+ neighb_16+ neighb_17+ neighb_18+ neighb_19+ neighb_20+ neighb_21, data = df_reg)
summary(model)

install.packages("stargazer")
library(stargazer)

stargazer(model, type='text', title="Regression Results", out="gen/analysis/regression.html")
