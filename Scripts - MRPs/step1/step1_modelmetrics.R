library(rstan)
library(data.table)
library(dplyr)

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.



model_name = 'logit_ordered_polinteraction_CTXT_tree10_8feb'
step1 = readRDS(paste0("./stan trained models step1/",model_name,".rds"))
df = as.data.table(readRDS("./step1_preprocessed.rds"))
census = readRDS("./census data/census.rds")[,1:5] #no need for state level info
census = as.data.table(lapply(census, function(x) as.numeric(x))) ### set the census to have numeric columns instead of factors - will use for matching.
input_list = readRDS("./step1_stanlist.rds")
source("./Scripts - MRPs/predict_stratframe.R") #precit fram function.




### extract params from step 1 model
y_pred =     as.data.frame(rstan::extract(step1, pars = c('y_pred')))
alpha =      as.data.frame(rstan::extract(step1, pars = c('alpha')))
theta =      as.data.frame(rstan::extract(step1, pars = c('theta')))
sex =        as.data.frame(rstan::extract(step1, pars = c('sex')) )
age =        as.data.frame(rstan::extract(step1, pars = c('age')) )
state =      as.data.frame(rstan::extract(step1, pars = c('state')) )
party =      as.data.frame(rstan::extract(step1, pars = c('party')) )
id =         as.data.frame(rstan::extract(step1, pars = c('id')) )
ctxt =       as.data.frame(rstan::extract(step1, pars = c('ctxt')))
ctxt_bypolp =as.data.frame(rstan::extract(step1, pars = c('lambda_CTXTparty')))



# sample model estimate (raw equivalent) ----------------------------------------------------

dim(input_list$x_ctxt * ctxt)
dim(ctxt)
dim(input_list$x_ctxt)

df$y_pred = colMeans(y_pred)


aggregated = df %>% group_by(id_str) %>% summarise(naive_raw = mean(assessment_linear),
                                      model_raw = mean(y_pred))

plot(aggregated$naive_raw, aggregated$model_raw)
cor(aggregated$naive_raw, aggregated$model_raw)



# stratified - estimates, 1for1 (balanced equivalent) and woc (raked equivalent)----------------------


### participant Main Effects
ME_participants_model = predict_stratframe(census = census, alpha, age, state, party)

### Tweet Main effects
##formatted per assessment - need tweet format
ME_tweet_model = as.data.frame(input_list$x_ctxt %*% t(as.matrix(as.data.frame(ctxt)))) + #context main effects 
  t(id)[input_list$g_id,] #tweet random intercept
ME_tweet_model$id_str_num = df$id_str_num #technically unnecessary as all param combinatios are unique but safety precaution
ME_tweet_model = as.matrix(distinct(ME_tweet_model)[, -grep(pattern = 'id_str_num',colnames(ME_tweet_model))])


####computations - indexing for party in census
index_party = ifelse(census$PARTY ==1, 1,
                     ifelse(census$PARTY ==2, 31,61))
dems = census$PARTY==1
neus = census$PARTY==2
reps = census$PARTY==3

young = census$AGE<=3
old = census$AGE ==4

male  =census$SEX ==2
female = census$SEX==1


#### create context matrix for all tweets
tweet_context <- df %>%
  group_by(id_str) %>%
  summarise(across(starts_with("context_"), mean))
tweet_context= as.matrix(tweet_context[,-(grep("id_str",colnames(tweet_context)))])



###init vectors
unique_ids = length(unique(df$id_str))
woc_estimates = numeric(unique_ids)
dem_estimates = numeric(unique_ids)
neu_estimates = numeric(unique_ids)
rep_estimates = numeric(unique_ids)


woc_discestimates = numeric(unique_ids)
dem_discestimates = numeric(unique_ids)
neu_discestimates = numeric(unique_ids)
rep_discestimates = numeric(unique_ids)


young_discestimates = numeric(unique_ids)
old_discestimates = numeric(unique_ids)

male_discestimates = numeric(unique_ids)
female_discestimates = numeric(unique_ids)





pb <- txtProgressBar(min = 0, max = unique_ids, style = 3)
for (t in 1:unique_ids) {
  estimates.t = matrix(data = NA, nrow = nrow(census), ncol = nrow(alpha))
  estimates_discrete.t = matrix(data = NA, nrow = nrow(census), ncol = nrow(alpha))
  for (c in 1:nrow(census)) {
    
    #estimate of synthtic c for tweet t
    estimates.t[c,] =  
      ME_participants_model[c,] + as.vector(ME_tweet_model[t,]) +
      rowSums(t(t(ctxt_bypolp[,index_party[c]:(index_party[c]+29)]) * 
                  as.vector(tweet_context[t,])))
    
    estimates_discrete.t[c,] = 
      ifelse(estimates.t[c,] >= theta[,3],4,
           ifelse(estimates.t[c,] >= theta[,2],3,
                  ifelse(estimates.t[c,] >= theta[,1],2,1)))
    
  }
  woc_estimates[t] = sum((rowMeans(estimates.t) * census$N))/sum(census$N)
  dem_estimates[t] = sum((rowMeans(estimates.t[dems,]) * census$N[dems]))/sum(census$N[dems])
  neu_estimates[t] = sum((rowMeans(estimates.t[neus,]) * census$N[neus]))/sum(census$N[neus])
  rep_estimates[t] = sum((rowMeans(estimates.t[reps,]) * census$N[reps]))/sum(census$N[reps])
  
  woc_discestimates[t] = sum((rowMeans(estimates_discrete.t) * census$N))/sum(census$N)
  dem_discestimates[t] = sum((rowMeans(estimates_discrete.t[dems,]) * census$N[dems]))/sum(census$N[dems])
  neu_discestimates[t] = sum((rowMeans(estimates_discrete.t[neus,]) * census$N[neus]))/sum(census$N[neus])
  rep_discestimates[t] = sum((rowMeans(estimates_discrete.t[reps,]) * census$N[reps]))/sum(census$N[reps])
  
  
  young_discestimates[t] = sum((rowMeans(estimates_discrete.t[young,]) * census$N[young]))/sum(census$N[young])
  old_discestimates[t] = sum((rowMeans(estimates_discrete.t[old,]) * census$N[old]))/sum(census$N[old])
  male_discestimates[t] = sum((rowMeans(estimates_discrete.t[male,]) * census$N[male]))/sum(census$N[male])
  female_discestimates[t] = sum((rowMeans(estimates_discrete.t[female,]) * census$N[female]))/sum(census$N[female])
  
  setTxtProgressBar(pb, t)
}


estim =as.data.frame(cbind(id_str = as.character(unique(df$id_str)),
                           id_str_num = unique(df$id_str_num),
                           woc_estimates,
                           woc_discestimates,
                           dem_estimates,
                           dem_discestimates,
                           neu_estimates,
                           neu_discestimates,
                           rep_estimates,
                           rep_discestimates,
                           model_raw = aggregated$model_raw,
                           young_discestimates,
                           old_discestimates,
                           male_discestimates,
                           female_discestimates))



saveRDS(estim,"./estimations.rds")



# quick visual ------------------------------------------------------------
library(corrplot)
estim = readRDS("./estimations.rds")
estim = as.data.table(lapply(estim, function(x) as.numeric(x)))
corrplot(cor(estim[,-c(1:3,5,7,9)]), method ="number")
corrplot(cor(estim[,-c(1:3,5,7,9)]), method ="color")


