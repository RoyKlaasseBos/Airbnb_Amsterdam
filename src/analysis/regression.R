##Import the data
library(fastDummies)
library(dplyr)
library(stargazer)
library(ggplot2)
# DATA
df <- read.csv("../../data/data.csv")
df$holidays <- ifelse(df$month == 12, 1, 0)
df$dummies <- dummy_cols(df, select_columns = 'neighbourhood')


# Tabels text and html
stargazer(df,
          title = "Figure 1:Stargazer Table",
          dep.var.caption = "Number of Reviews",  
          dep.var.labels = "",  
          covariate.labels = c("Month", "Neighbourhood", "Holiday"),  
          column.labels = c("Full model"),
          notes.label = "Significance levels",  
          type="text",
          out="output.html"  
)


stargazer(df,
          title = "Figure 1:Stargazer Table",
          dep.var.caption = "Number of Reviews",  
          dep.var.labels = "",  
          covariate.labels = c("Month", "Neighbourhood", "Holiday"),  
          column.labels = c("Full model"),
          notes.label = "Significance levels",  
          type="html",
          out="output.html"  
)


lmmonth <- lm(month~num_reviews, data = df)
lmholiday <- lm(holidays~num_reviews, data = df)
summary(lmmonth)
summary(lmholiday)


stargazer(lmmonth, lmholiday, 
          title = "Month and holidays  effect on number of reviews",
          dep.var.caption = "Number of reviews", 
          covariate.labels = c("Month", "Holiday"), 
          notes.label = "significance levels", 
          type = "text", 
          out = "output.html")

stargazer(lmmonth, lmholiday, 
          title = "Month and holidays  effect on number of reviews",
          dep.var.caption = "Number of reviews", 
          covariate.labels = c("Month", "Holiday"), 
          notes.label = "significance levels", 
          type = "html", 
          out = "output.html")

library(ggplot2)
#Regression month and reviews

ggplot(df, aes(x = month, y = num_reviews)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, aes(color="Full model")) +
  geom_smooth(method = "lm", se = FALSE, data = df)  +
  labs(x = "Month", y = "Number of reviews") +  
  ggtitle("Figure 2: Linear trend between IV and DV") +
  scale_colour_manual(name="Legend", values=c("red", "blue"))


#Regression holidays and reviews

ggplot(df, aes(x = holidays, y = num_reviews)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, aes(color="Full model")) +
  geom_smooth(method = "lm", se = FALSE, data = df)  +
  labs(x = "Holidays", y = "Number of reviews") +  
  ggtitle("Figure 2: Linear trend between IV and DV") +
  scale_colour_manual(name="Legend", values=c("red", "blue"))


#Regression neighborhoods and reviews
ggplot(df, aes(x = neighbourhood, y = num_reviews)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, aes(color="Full model")) +
  geom_smooth(method = "lm", se = FALSE, data = df)  +
  labs(x = "Neighbourhood", y = "Number of reviews") +  
  ggtitle("Figure 2: Linear trend between IV and DV") +
  scale_colour_manual(name="Legend", values=c("red", "blue"))



