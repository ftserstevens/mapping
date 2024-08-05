library(rstan)
library(ggplot2)
library(bayesplot)
library(data.table)
library(dplyr)
library(xtable)


if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

input_list = readRDS("./step1_stanlist.rds")
model_name = 'logit_ordered_polinteraction_CTXT_tree10_8feb'
step1 = readRDS(paste0("./stan trained models step1/",model_name,".rds"))
con_names =  gsub("context_","",colnames(input_list$x_ctxt))
df = as.data.table(readRDS("./step1_preprocessed.rds"))


# latex model -------------------------------------------------------------


sig = as.data.frame(rstan::extract(step1, pars = c('theta','alpha',"party","sigma_party",
                                                   "sex",'sigma_sex',"state","sigma_state","age", "sigma_age",
                                                   "ctxt","lambda_CTXTparty","lp__")))


Pbeta_zero = function(col) {
  mean(col > 0)
}

vec =vector()
for(i in 1:ncol(sig)) {
  vec = c(vec, Pbeta_zero(sig[,i]))
}
names(sig)
vec

a = summary(step1, probs=c(.1,.5,.9), pars = c('theta','alpha',"party","sigma_party",
                                               "sex",'sigma_sex',"state","sigma_state","age", "sigma_age",
                                               "ctxt","lambda_CTXTparty","lp__"))
rownames(a$summary) = c("Theta[1]","Theta[2]","Theta[3]","Intercept",
                        "Party: Democrat", "Party: Neutral", "Party: Republican", "sigma Party",
                        "Sex:Female", "Sex:Male", "sigma Sex",
                        paste0( rep("State:", 51), levels(df$state_short))  ,"sigma State",
                        "Age:-18", "Age:19-29" ,"Age:30-39" , "Age:40+", "sigma Age",
                        sub("context_","",colnames(input_list$x_ctxt)), sub(" V1_",": ",sub("context_","",colnames(input_list$x_ctxtbyparty))),
                        "lp__"
)

table_latex = as.data.frame(a$summary)
table_latex$Significance = vec

latex_code <- xtable(table_latex, caption = paste0("Step1"), label = paste0("tab:model_summary"))
latex_code
