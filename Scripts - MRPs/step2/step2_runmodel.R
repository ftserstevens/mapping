# Packages ----------------------------------------------------------------

library(rstan)
library(stringr)
library(ggplot2)

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

# load df ----------------------------------------------------------------

df = readRDS("~/Documents/tweet_woc/merged_step2.rds")
df_og = df ##og dataset


# create select vec ----------------------------------------------------------
input_acuracy = names(df)[c(grep('raw_average',names(df)),
                            grep('raked',names(df)),
                            which(names(df) %in% str_subset(names(df), "(?<!_)model_")))][-c(10,11,13,15,17)]

input_count = c(gsub("raw_average","count", x= names(df)[grep('raw_average',names(df))]),
                rep('count',length(grep('raked',names(df)))),
                rep('count',length(which(names(df) %in% str_subset(names(df), "(?<!_)model_")))))[-c(10,11,13,15,17)]


# select df ---------------------------------------------------------------


input_acuracy


  
i = 4 ### select the dependent var with i index.
use2 =T

accuracy = input_acuracy[i]
f_count_ty = input_count[i]

f_count = c(0) #min count per tweet


# select correct alphas & counts ------------------------------------------
##remove NA & make num  
df$alpha = as.numeric(df[[accuracy]])
df$count = as.numeric(df[[f_count_ty]])
df = df[!is.na(df$alpha),]
df = df[!is.na(df$count),]

#mincount at least = to n
df = df[df$count >= f_count,]


#dependent to at least 10%
dependent =  quantile(df$alpha, .1, na.rm = T)


if(use2) {
  dependent = 2  
}

print(accuracy)
table(df$alpha <=2)/nrow(df)


### fixed effects at state level
X_statecov = as.matrix(df[c(
  "white_perc", 
  "undergrad_perc", 
  "pop")])




# run stan ----------------------------------------------------------------
stan_list = list(
  age = as.numeric(factor(df$age)), #age
  female = ifelse(df$sex =="female",1,0), ##female dummy
  sex = ifelse(df$sex =="female",2,1), ##female dummy
  state = as.numeric(df$state),
  party = as.numeric(df$party),
  misc = ifelse(is.na(df$misinfo_exposure_score) == T, mean(df$misinfo_exposure_score, na.rm = T),df$misinfo_exposure_score),
  N = nrow(df),
  y = ifelse(df$alpha <= dependent, 1,0),
  n_S = 51, #total amount of states
  n_P =length(unique(df$party)),
 
  X_statecov = X_statecov,
  D_statecov = dim(X_statecov)[2]
)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())




run = T
  if (run) {
    print(accuracy)
    print(paste(nrow(df), "rows in the data"))
    print(paste(mean(df$count), "reviews per tweet"))
            step2 <-stan(
            file = "./stan models step2/step2.stan",
            #file = "./stan models step2/step2a includemisc.stan",
            data = stan_list, 
           iter =  if(use2 | accuracy == "raw_average_onlyrep" |
                      accuracy == "raw_average_1for1") {8000} else{2500},  
           warmup =  if(use2 | accuracy == "raw_average_onlyrep" |
                        accuracy == "raw_average_1for1") {4000} else{1250},
            control= if(accuracy == "raw_average_onlyrep" |
                          accuracy == "raw_average_1for1") {list(max_treedepth=20)} else(list(max_treedepth=15)),
           
            
            thin = 5,
            seed = 111,
            chains = 10,
            #control= list(max_treedepth=10),
            init = 'random',
            verbose = F,open_progress =F) 
            if(!use2) {
              saveRDS(step2, paste0("./stan trained models step2/step2_",accuracy,".rds"))
              saveRDS(df,paste0("./stan trained models step2/df/step2df_",accuracy,".rds"))
            }
            if(use2){
              saveRDS(step2, paste0("./stan trained models step2/using2/step2_",accuracy,".rds"))
              saveRDS(df,paste0("./stan trained models step2/using2/df/step2df_",accuracy,".rds"))
            }
          
  }

