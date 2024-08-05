# Packages ----------------------------------------------------------------
library(ipumsr)
library(terra)
library(zipcodeR)
library(rstan)
library(fastDummies)
library(dplyr)
library(parallel)
library(usmap)
library(usmapdata)
library(mltools)
library(maps)
library(xtable)
library(readr)
library(stringr)
library(stringi)
library(survey)
library(cdlTools)
library(mltools)
library(ggplot2)
library(bayesplot)
library(ggthemes)
library(ggridges)



rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}




# load data ---------------------------------------------------------------
accuracy = "raw_average_1for1"
use2 = T



step2 = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_",accuracy,".rds"))
df = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"df/step2df_",accuracy,".rds"))
step2_params = rstan::extract(step2)
alpha = step2_params$alpha
beta_sex = step2_params$beta
gamma_age = step2_params$gamma
delta_state = step2_params$delta
zeta_party = step2_params$zeta
eta_statecov = step2_params$eta
sex_coef = step2_params$sex_coef



# digitize census ---------------------------------------------------------

census =readRDS("./census data/census.rds")

levels(census$STATE) == levels(df$state_short)

census = census %>% 
  mutate(STATE = as.numeric(STATE),
         AGE = as.numeric(AGE),
         SEX = as.numeric(SEX),
         PARTY = as.numeric(PARTY)) %>%
  group_by(STATE) %>%
  mutate(weight = N / sum(N)) %>%
  ungroup()





# make predictions --------------------------------------------------------



preds= array(NA, c(nrow(census), nrow(alpha)))
pi_preds = array(NA, c(nrow(census), nrow(alpha)))
ilogit = function(x) {exp(x) /(1+exp(x))}


n_census <- nrow(census)
n_alpha <- nrow(alpha)
fixed_eff =  c("white_perc", "undergrad_perc", "pop")


pb <- txtProgressBar(min = 0, max = n_census, style = 3)


for(i in 1:n_census) { #for evey row in census
  census_sex.i = as.numeric(census[i,"SEX"])
  census_age.i = as.numeric(census[i,"AGE"])
  census_state.i = as.numeric(census[i,"STATE"])
  census_party.i = as.numeric(census[i,"PARTY"])
  census_fe.i = as.numeric(census[i,fixed_eff])
  
  for (j in 1:n_alpha) { # for every iteration of estimated coef
    
    ## calculate fe
    sum_fe = sum(eta_statecov[j,] * census_fe.i)
      
    
    preds[i,j] =  as.numeric(
      alpha[j] + 
        sex_coef[j]* (census_sex.i-1) + 
        gamma_age[j, census_age.i] + 
        delta_state[j, census_state.i] +
        zeta_party[j,census_party.i] +
        sum_fe)
    
    
    
    pi_preds[i,j] = ilogit(preds[i,j])
  }
  setTxtProgressBar(pb, i)
  
}


# post-stratify to state probability--------------------------------------------------


#preds are preds[census, iteration]
state_PS = as.data.frame(array(NA, c(51,nrow(alpha)))) # [nstate, niterations]
rownames(state_PS) = levels(df$state_short)
w_estimate =  census$weight * pi_preds

pb <- txtProgressBar(min = 0, max = nrow(alpha), style = 3)

for (i in 1:nrow(alpha)) {
  state_PS[,i] = as.data.frame(cbind(state = census$STATE, estim = w_estimate[,i])) %>%
    dplyr::group_by(state) %>% 
    dplyr::summarise(estim = sum(estim))  %>% 
    dplyr::select(estim)
  setTxtProgressBar(pb, i)
}
saveRDS(state_PS, paste0("./Scripts - MRPs/step2/state_posterior/probability/",if(use2){"using2/"},accuracy,".rds"))



# post-stratify to state logged odds--------------------------------------------------


#preds are preds[census, iteration]
state_PS_lo = as.data.frame(array(NA, c(51,nrow(alpha)))) # [nstate, niterations]
rownames(state_PS_lo) = levels(df$state_short)
w_estimate =  census$weight * preds

pb <- txtProgressBar(min = 0, max = nrow(alpha), style = 3)

for (i in 1:nrow(alpha)) {
  state_PS_lo[,i] = as.data.frame(cbind(state = census$STATE, estim = w_estimate[,i])) %>%
    dplyr::group_by(state) %>% 
    dplyr::summarise(estim = sum(estim))  %>% 
    dplyr::select(estim)
  setTxtProgressBar(pb, i)
}
saveRDS(state_PS_lo, file = paste0("./Scripts - MRPs/step2/state_posterior/logged_odds/",if(use2){"using2/"},accuracy,".rds"))



# get the state intervals - probability & logged odds-------------------------------------------

### probability.
state_dist = as.data.frame(t(apply(t(state_PS), 2, function(x) quantile(x, probs = c(0.10, 0.50, 0.90)))))
saveRDS(object = state_dist, file =paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},accuracy,".rds"))

### logged odds
state_dist_lo = as.data.frame(t(apply(t(state_PS_lo), 2, function(x) quantile(x, probs = c(0.10, 0.50, 0.90)))))
saveRDS(object = state_dist_lo, file =paste0("./Scripts - MRPs/step2/state_intervals/logged_odds/",if(use2) {"using2/"},accuracy,".rds"))




