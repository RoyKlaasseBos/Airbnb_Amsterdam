#needed packages
require(fastDummies)
require(dplyr)
require(stargazer)
require(substr)

# read in the csv
df <- read.csv("../../data/data.csv")

# add dummies for neighborhoods
df$neighbourhood <- gsub(" ", "", df$neighbourhood)
df$neighbourhood <- gsub("-", "_", df$neighbourhood)
df <- dummy_cols(df, select_columns = "neighbourhood")

# add dummies for summer months
df <- df %>%
  mutate(summer = ifelse(month == 6 | month == 7 | month == 8, 1, 0))

# add months to end dataset
df$months <- df$month

# paste colnames
frmla <- as.formula(paste(colnames(df)[5], 
                    paste(colnames(df)[8:ncol(df)], sep = "", collapse = "+"), 
                    sep = " ~ "))

# regression
reg <- lm(frmla, df)
summary(reg)

# labels for in table
labels <- paste(colnames(df[8:30]))
labels <- gsub("_", " ", labels)
labels <- substr(labels, 15, nchar(labels))
labels <- labels[1:21]

# create directory
dir.create("../../gen")
dir.create("../../gen/analysis")

# stargazer
stargazer(reg, 
          title = "Figure 1: Regression analysis",
          dep.var.caption = "Number of reviews",
          dep.var.labels = "",
          covariate.labels = c(labels,  "summer", "months"),
          notes.label = "Significance level",
          type = "html",
          out= "../../gen/analysis/regression.html")
