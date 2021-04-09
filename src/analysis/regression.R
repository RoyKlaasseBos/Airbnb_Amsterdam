# your R code goes here!

---
  title: "regression"
output: pdf_document
---
  
  ##import packages

library(ggplot2)
library(ggfortify)
library(broom)
library(dplyr)
library(varhandle)
library(stargazer)


##import data

airbnb_df <- read.csv("../../data/data.csv", sep=",")


##preparation before making the regression (creating dummies)

table(airbnb_df$neighbourhood)

airbnb_dummy <- to.dummy(airbnb_df$neighbourhood, "neighbourhood")

dummy_df <- cbind(airbnb_df, dummy = airbnb_dummy)

dummy_df_month <- dummy_df %>% 
  mutate(month2 = month)

airbnb_complete <- dummy_df_month %>% 
  mutate(christmas = ifelse(month2 == "12", 1,0))


##Evaluate Model Assumptions

# estimate linear model

mdl_airbnb <- lm(num_reviews ~ month, data=airbnb_complete)

# check linear model assumptions
autoplot(
  mdl_airbnb,
  which = 1:3,
  nrow = 1,
  ncol = 3
)


##outlier screening

leverage_influence <- mdl_airbnb %>%
  augment() %>%
  select(num_reviews, month, leverage = .hat, cooks_dist = .cooksd) %>%
  arrange(desc(cooks_dist)) %>%
  head()



##Model Reporting

pdf("../../gen/analyses/model_reporting.pdf")
stargazer(mdl_airbnb,
          title = "Model reporting airbnb Amsterdam",
          dep.var.caption = "number of reviews",  
          dep.var.labels = "",  
          covariate.labels = c("month", "christmas","dummy.neighbourhood.Bijlmer-Centrum", "dummy.neighbourhood.Bijlmer-Oost", "dummy.neighbourhood.Bos_en_Lommer", "dummy.neighbourhood.Buitenveldert_-_Zuidas", "dummy.neighbourhood.Centrum-Oost", "dummy.neighbourhood.Centrum-West", "dummy.neighbourhood.De_Aker_-_Nieuw_Sloten", "dummy.neighbourhood.De_Baarsjes_-_Oud-West", "dummy.neighbourhood.De_Pijp_-_Rivierenbuurt", "dummy.neighbourhood.Gaasperdam_-_Driemond", "dummy.neighbourhood.Geuzenveld_-_Slotermeer", "dummy.neighbourhood.IJburg_-_Zeeburgereiland", "dummy.neighbourhood.Noord-Oost", "dummy.neighbourhood.Noord-West", "dummy.neighbourhood.Oostelijk_Havengebied_-_Indische_Buurt", "dummy.neighbourhood.Osdorp", "dummy.neighbourhood.Oud-Noord", "dummy.neighbourhood.Oud-Oost", "dummy.neighbourhood.Slotervaart", "dummy.neighbourhood.Watergraafsmeer", "dummy.neighbourhood.Westerpark", "dummy.neighbourhood.Zuid"),  
          column.labels = c("Full model", "Outlier excluded"),
          notes.label = "Significance levels",  
          type="pdf",
          out="output.pdf"  
)




##Visualize Linear Relationships

pdf("../../gen/analyses/regression.html")
ggplot(airbnb_complete, aes(month, num_reviews)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, aes(color="Full model")) +
  geom_smooth(method = "lm", se = FALSE, data = airbnb_complete,  aes(color="Outlier excluded"))  +
  labs(x = "month", y = "Stop distance (m)") +  
  ggtitle("Figure 2: Linear trend between reviews and date") +
  scale_colour_manual(name="Legend", values=c("red", "blue"))
