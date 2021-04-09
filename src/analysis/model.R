# Load results
load("../../gen/analysis/model_results.RData")

# Load in additional package to export to latex table
require(stargazer) 

# Export to latex table (omits f-stat since messes up table)
stargazer(regression_model,out="../../gen/analysis/regression.html",
            title = "Regression Model", label = "model:regression",
            omit.stat=c("f")) 
