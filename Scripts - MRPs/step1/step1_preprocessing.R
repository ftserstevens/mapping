
# Packages ----------------------------------------------------------------
library(dplyr)
library(mltools)
library(cdlTools)
library(readr)
library(stringr)
library(ggplot2)



#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#c() 

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}



# Data loading -----------------------------------------------------


df = readRDS("./qualt_long.rds")


# trans from assessment and confidence -----------------------------------------------------


#### accuracy assessment
df$assessment_notaccurate_bin <- ifelse(df$assessment =="Not at all accurate",1,0)
df$assessment_notveryaccurate_bin <- ifelse(df$assessment =="Not very accurate",1,0)
df$assessment_someaccurate_bin <- ifelse(df$assessment =="Somewhat accurate",1,0)
df$assessment_veryaccurate_bin <- ifelse(df$assessment =="Very accurate",1,0)
df$assesement = ordered((as.factor(df$assessment)), levels = c(
  "Not at all accurate", "Not very accurate" ,  "Somewhat accurate"  , "Very accurate" ))
df$assessment_linear = as.numeric(df$assesement)



###confidence assessment
df$confidence = ordered((as.factor(df$confidence)), levels = c(
  "Not confident at all", "Not very confident" ,  "Somewhat confident"  , "Very confident" ))
df$confidence_linear=as.numeric(df$confidence)

# factorize parameters ----------------------------------------------------

df$id_str = as.factor(df$id_str) ### R only has 64 bits for double hence 16 numbers- we transform to factor
df$id_str_num = as.numeric(df$id_str)

df$sex = as.factor(df$sex)
df$sex_num = as.numeric(df$sex)

df$pol_party = factor(ifelse(df$pol_party == "Other", "No preference",df$pol_party))
levels(df$pol_party) = c("Democrat","Neutral","Neutral","Republican") ##bring back to 3 levels.
df$pol_party_num = as.numeric(df$pol_party)


### factorize age
df$age = as.integer(df$age) 
df$age_cat <- cut(df$age, breaks = c(-Inf, 18, 30, 40, 99), 
                           labels = c("-18", "18-30", "31-40", "41-99"), right = FALSE)
df$age_cat_num = as.numeric(df$age_cat)


### factorize states to FIP
df$State = ifelse(df$State == "District of Columbia", "DC",df$State)
df$State = factor(df$State)

df$state = factor(cdlTools::fips(gsub(" ","", df$State), to ="Abbreviation"))
state_list = readRDS("./state_list.rds") 
df = merge(df, state_list, by = "state") ### add state_num to df.



# add context -------------------------------------------------------------

context_topics <- readRDS("./Scripts - get merge external/external_data/context_topics.rds")
df = merge(df, context_topics,
           by = "id_str")

##### matrices for context-party interactions 

X_POLP = as.matrix(one_hot(data.table::as.data.table(factor(df$pol_party)))) ## one hot of the the pol party
X_CTXT = as.matrix(df[, names(context_topics)[-1]])

interact_matrix = function(X1, X2) {
  if(dim(X1)[1] != dim(X2)[1]) {
    stop('Matrices have different nrow')
  }
  return_matrix = matrix(nrow = dim(X1)[1], ncol = dim(X1)[2]* dim(X2)[2])
  colnames(return_matrix) = rep(NA, dim(X1)[2]* dim(X2)[2])
  for(i in 1:dim(X2)[2]) {
    start_replace.i = (1 + (i-1)* dim(X1)[2])
    end_replace.i = (1 + (i-1)* dim(X1)[2]) + (dim(X1)[2]-1)
    return_matrix[,start_replace.i:end_replace.i] = X2[,i] * X1
    colnames(return_matrix)[start_replace.i:end_replace.i] = paste(colnames(X1),colnames(X2)[i])
  }
  return(return_matrix)
}


X_CTXTbypolp = interact_matrix(X1 = X_CTXT, X2 = X_POLP)




# stan list for model --------------------------------------------------------------


stan_list = list(
  y = as.vector(df$assessment_linear),
  K = 4, #cutoffs
  N = nrow(df),
  
  x_ctxt = as.matrix(X_CTXT),
  D_ctxt = dim(X_CTXT)[2],
  
  x_ctxtbyparty = as.matrix(X_CTXTbypolp),
  D_ctxtbyparty = dim(as.matrix(X_CTXTbypolp))[2],
  
  M_state = 51, ##force 51 states - only 49 in OG data frame
  g_state = df$state_num,
  
  M_party = length(unique(df$pol_party)),
  g_party = df$pol_party_num,
  
  M_age = length(unique(df$age_cat_num)),
  g_age = df$age_cat_num,
  
  M_sex = length(unique(df$sex_num)),
  g_sex = df$sex_num,
  
  M_id = length(unique(df$id_str)),
  g_id = df$id_str_num
  
  
)


saveRDS(df, "./step1_preprocessed.rds")
saveRDS(stan_list, "./step1_stanlist.rds")

