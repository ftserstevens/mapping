# Packages ----------------------------------------------------------------

library(rstan)
library(xtable)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


colMedians = function(X) {
  dims = dim(X)[2]
  medians = 1:dims
  for (i in 1:dims) {
    medians[i] = median(X[,i])
  }
  return(medians) }

# load data ---------------------------------------------------------------


use2 = T
accuracy = "raw_average_1for1"



step2 = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_",accuracy,".rds"))
df = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"df/step2df_",accuracy,".rds"))

posterior_samples <- as.data.frame(step2)
prob_beta_positive <- sapply(names(posterior_samples), function(param) {
  mean(posterior_samples[[param]] > 0)
})

# Create a table summary
table <- summary(step2, probs = c(.1, .5, .9))
rownames(table$summary) <- c("intercept", "Sex:Female", "sigma Age", 
                             "Age:-18", "Age:19-29", "Age:30-39", "Age:40+", "sigma State",
                             paste0(rep("State:", 51), levels(df$state_short)), "sigma Party",
                             "Party: Democrat", "Party: Neutral", "Party: Republican",
                             "State WhitePopulation", "State UndergradPopulation", "State PopulationDensity",
                             "lp__")
table = as.data.frame(table$summary)

# Add the P(Beta > 0) column to the table
table$P_Beta_Positive <- prob_beta_positive

# Create LaTeX code
latex_code <- xtable(table, 
                     caption = "Summary of the posterior distribution of likelihood of spreading fake news online using the aggregated ratings of Democratic survey participants", 
                     label = paste0("tab:model_summary_",accuracy,if(use2){"_use2"}))

# Print LaTeX code
print(latex_code, type = "latex", include.rownames = TRUE, include.colnames = TRUE, 
      caption.placement = "top", 
      add.to.row = list(pos = list(0), command = c("\\hline\n")))



