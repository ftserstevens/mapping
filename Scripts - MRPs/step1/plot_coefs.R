library(rstan)
library(ggplot2)
library(bayesplot)
library(data.table)
library(dplyr)


if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

input_list = readRDS("./step1_stanlist.rds")
model_name = 'logit_ordered_polinteraction_CTXT_tree10_8feb'
step1 = readRDS(paste0("./stan trained models step1/",model_name,".rds"))
con_names =  gsub("context_","",colnames(input_list$x_ctxt))
df = as.data.table(readRDS("./step1_preprocessed.rds"))


# posterior distribution plotting ----------------------------------------


ctxt = mcmc_intervals(as.data.frame(
  rstan::extract(step1, pars= c('ctxt'))), prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) + 
  scale_y_discrete(labels = con_names) 
ctxt
ggsave(
  filename = "./Scripts - MRPs/step1/context_plots/step1_ctxt_main.pdf",
  plot = ctxt,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 8,
  units = c("in"))

ctxt_int = mcmc_intervals(as.data.frame(
  rstan::extract(step1, pars= c('lambda_CTXTparty')))[,c(1:30,61:90)], prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) +
  scale_y_discrete(labels = paste(c(rep("Democrat:",30,),(rep("Republican:",30))),rep(con_names,2))) 

ggsave(
  filename = "./Scripts - MRPs/step1/context_plots/step1_ctxt_interaction.pdf",
  plot = ctxt_int,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 8,
  units = c("in"))

party = mcmc_intervals(as.data.frame(
  rstan::extract(step1, pars= c('party'))), prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) +
  scale_y_discrete(labels = paste(c("Dem","Neutral","Rep")))
ggsave(
  filename = "./Scripts - MRPs/step1/context_plots/step1_party_main.pdf",
  plot = party,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 8,
  units = c("in"))



alpha = mcmc_intervals(as.data.frame(
  rstan::extract(step1, pars= c('alpha'))), prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) +
  scale_y_discrete(labels = paste(c("Intercept")))


age = mcmc_intervals(as.data.frame(
  rstan::extract(step1, pars= c('age'))), prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) +
  scale_y_discrete(labels = paste(c("-18","18-29",'30-39','40+')))


gen = mcmc_intervals(as.data.frame(
  rstan::extract(step1, pars= c('sex'))), prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) +
  scale_y_discrete(labels = paste(c("Female","Male")))



# id params review --------------------------------------------------------


ids = as.data.frame(rstan::extract(step1, pars= c('id')))



# Dem & rep Difference ----------------------------------------------------

dems = as.data.frame(rstan::extract(step1, pars= c('lambda_CTXTparty')))[,c(1:30)]
dem_main = as.data.frame(rstan::extract(step1, pars= c('party')))[,c(1)]

reps = as.data.frame(rstan::extract(step1, pars= c('lambda_CTXTparty')))[,c(61:90)]
rep_main = as.data.frame(rstan::extract(step1, pars= c('party')))[,c(3)]

ctxt_main = as.data.frame(rstan::extract(step1, pars= c('ctxt')))
diff= (dems+dem_main + ctxt_main ) - (reps+ rep_main + ctxt_main) 

order_diff = order(colMeans(diff))

diff = diff %>% select(order_diff)



demrep_ctxtdiff =mcmc_intervals(diff, prob = 0.5, prob_outer = 0.9) +
  theme_minimal() + geom_vline(xintercept = 0) +   scale_y_discrete(labels = con_names[order_diff])
ggsave(
  filename = "./Scripts - MRPs/step1/context_plots/step1_demrep_ctxtdiff.pdf",
  plot = demrep_ctxtdiff,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 8,
  units = c("in"))



