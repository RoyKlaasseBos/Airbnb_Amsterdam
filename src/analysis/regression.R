# your R code goes here!
library(ggplot2)
library(ggfortify)
library(broom)
library(dplyr)
library(stargazer)

airbnb <- read.csv("../../data/data.csv")


# estimate linear model
mdl_airbnb <- lm(month ~ num_reviews, data=airbnb)

# check linear model assumptions
autoplot(
  mdl_airbnb,
  which = 1:3,
  nrow = 1,
  ncol = 3
)

#screening for outliers
leverage_influence <- mdl_airbnb %>%
  augment() %>%
  select(month, num_reviews, leverage = .hat, cooks_dist = .cooksd) %>%
  arrange(desc(cooks_dist))
print(leverage_influence)

dir.create("../../gen/analysis")
#Model reporting
stargazer(mdl_airbnb,
          title = "Figure 1: reviews vs month ",
          dep.var.caption = "Number of reviews",  
          dep.var.labels = "num_reviews",  
          covariate.labels = c("months"),  
          column.labels = c("Full model"),
          notes.label = "Significance levels",  
          type="text",
          out="../../gen/analysis/regression.html"  
)


#Visualisation of the data
ggplot(airbnb, aes(month, num_reviews)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, aes(color="Full model")) +
  #geom_smooth(method = "lm", se = FALSE, data = cars_cleaned,  aes(color="Outlier excluded"))  +
  labs(x = "months (dummy) ", y = "number of reviews") +  
  ggtitle("Figure 2: Linear trend between months and number of reviews") +
  scale_colour_manual(name="Legend", values=c("red", "blue"))
