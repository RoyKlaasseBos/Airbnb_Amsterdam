
# Load dataset
df_amsterdam <- read.csv("../../data/data.csv")

# Instal packages
#install.packages('fastDummies')
library('fastDummies')

#Creating directory /gen/analysis/
dir.create("../../gen/analysis")

# 1. Creating a trend variables
# Aren't the trends variables already done? Because the months are already numbered 1, 2 etc.

# 2. Creating dummy variable for the various neighborhoods
df_amsterdam <- dummy_cols(df_amsterdam, select_columns = 'neighbourhood')

# 3. Creating dummy variables for the Christmas month December.
df_amsterdam$christmas_holiday <- ifelse(df_amsterdam$month == '12', 1, 0)

# Creating the lineair model
lm_df_amsterdam <- lm(num_reviews~month + christmas_holiday + neighbourhood, data=df_amsterdam)

#Creating the lineair model in Stargazer
stargazer(lm_df_amsterdam,
          title = "Figure 1: Lineair model Amsterdam",
          dep.var.caption = "Number of reviews",  
          notes.label = "Significance levels",  
          type="html",
          out=" gen/analysis/regression.html")
