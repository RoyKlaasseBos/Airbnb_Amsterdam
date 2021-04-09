#needed packages
require(fastDummies)
require(dplyr)
require(stargazer)
require(substr)

# read in the csv
df <- read.csv("../../data/data.csv")

# add year factor (see trend)
df$year_factor <- df$year - 2015

# add dummies for neighborhoods
df$neighbourhood <- gsub(" ", "", df$neighbourhood)
df$neighbourhood <- gsub("-", "_", df$neighbourhood)
df <- dummy_cols(df, select_columns = "neighbourhood")

# add trend variable
df$trend <- df$year_factor * 12 + df$month

# add dummies for summer months
df <- df %>%
  mutate(summer = ifelse(month == 6 | month == 7 | month == 8, 1, 0))

# paste colnames
frmla <- as.formula(paste(colnames(df)[5], 
                    paste(colnames(df)[9:ncol(df)], sep = "", collapse = "+"), 
                    sep = " ~ "))

# regression
reg <- lm(frmla, df)
summary(reg)

# labels for in table
labels <- paste(colnames(df[9:30]))
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
          covariate.labels = c(labels, "trend", "summer"),
          notes.label = "Significance level",
          type = "html",
          out= "../../gen/analysis/regression.html")
