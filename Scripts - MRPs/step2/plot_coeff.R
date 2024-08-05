# Packages ----------------------------------------------------------------

library(rstan)
library(parallel)
library(mltools)
library(ggplot2)
library(bayesplot)
library(ggridges)
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



use2 = F
accuracy = "raw_average_1for1"


step2 = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_",accuracy,".rds"))
step2
df = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"df/step2df_",accuracy,".rds"))



step2_params = rstan::extract(step2)
alpha = step2_params$alpha
beta_sex = step2_params$beta
gamma_age = step2_params$gamma
delta_state = step2_params$delta
zeta_party = step2_params$zeta
eta_statecov = step2_params$eta
sex_coef = step2_params$sex_coef


# plot coefs --------------------------------------------------------------



mcmc_areas(as.data.frame(
  rstan::extract(step2, pars= c('zeta'))), prob = 0.5, prob_outer = 0.9) +
  scale_y_discrete(labels = c("zeta.1" = "Democrat",
                              "zeta.2" = "Neutral",
                              "zeta.3" = "Republican")) + theme_minimal()

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('eta')))) +  
  scale_y_discrete(labels = c("eta.1" = "White ‰",
                              "eta.2" = "Undergrad Percentage",
                              "eta.3" = "Population Density"))

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('alpha')))) +
  scale_y_discrete(labels ="Intercept")

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('sex_coef')))) +
  scale_y_discrete(labels ="Female")

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('gamma')))) + theme_minimal() +
  scale_y_discrete(labels =c("-18","19-29","30-39","40+"))


mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c("delta")))[,order(colMedians(as.data.frame(rstan::extract(step2, pars= c("delta")))))]) +
  scale_y_discrete(labels =levels(df$state_short)[order(colMedians(as.data.frame(rstan::extract(step2, pars= c("delta")))))])

 
