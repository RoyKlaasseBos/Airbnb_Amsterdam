##Import the data

df <- read.csv("../../data/data.csv")
library(ggplot2)
library(ggfortify)
library(broom)
library(dplyr)


library(dummies)
library(fastDummies)
library(recipes)
df$holidays <- ifelse(df$month == 12, 1, 0)
df <- dummy_cols(df, select_columns = 'neighbourhood')
df$dummyn <- factor(df$neighbourhood)
df$dummyn[1:15]

library(stargazer)

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
ggplot(df, aes(x = dummyn, y = num_reviews)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, aes(color="Full model")) +
  geom_smooth(method = "lm", se = FALSE, data = df)  +
  labs(x = "Neighbourhood", y = "Number of reviews") +  
  ggtitle("Figure 2: Linear trend between IV and DV") +
  scale_colour_manual(name="Legend", values=c("red", "blue"))



