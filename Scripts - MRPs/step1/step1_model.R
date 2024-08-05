library(rstan)


if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}

# Rstan Options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# load data ---------------------------------------------------------------

stan_list = readRDS("./step1_stanlist.rds")
model_name = 'logit_ordered_polinteraction_CTXT_tree10_8feb'


# run model ---------------------------------------------------------------


run = T

if(run) {
  
  model_step1 <-stan(file = "./stan models step1/step1.stan" ,
                             data = stan_list, 
                             iter = 1000,  
                             warmup =500,  
                             thin = 10,
                             chains = 7,
                             control= list(max_treedepth=10),
                             init = 'random',
                             verbose = T) 
  
  
  saveRDS(model_step1, paste0("./stan trained models step1/",model_name,".rds"))
  
  
}