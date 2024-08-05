library(rstan)
library(xtable)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}



remove_ext = function(models) {sub("step2_","",sub(".rds","",models))}

get_summary = function(model, param, percentiles = (c(.1,.5,.9))) {
  a = unlist(rstan::extract(model, pars = param, permuted = TRUE))
  quantiles = quantile(a, probs = c(.1,.5,.9))
  SigPos = mean(a > 0)
  Sig = if(SigPos >= .8 | SigPos <= .2) {"Yes"} else{"No"}
  Direction = if(Sig == "Yes") {
    if(SigPos > .5) {"Positive"} else{"Negative"}
  } else{
    ""
  }
  return(c(as.vector(quantiles), SigPos,Sig,Direction))
}

create_df = function() {
  df <- data.frame(Model = character(), Threshold = character(),
                   "10%" = numeric(), `50%` = numeric(), `90%` = numeric(), 
                   'P(beta > 0)' = numeric(), Significant = character(), Direction = character(), 
                   stringsAsFactors = FALSE)
  return(df)
}



coefs = c("alpha","sex_coef","zeta[1]","zeta[2]","zeta[3]", 
          "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", 
          "eta[1]", "eta[2]", "eta[3]")


models = list.files("./stan trained models step2")[-c(1,12)]
names = c(
  "Model Balanced","Model Democrat","Model Sample","Model Republican", "Model Population",
  "Naive Population","Naive Balanced","Naive Democrat","Naive Republican","Naive Sample")


# create dataframe --------------------------------------------------------


for (j in coefs) {
  df_coef = create_df()
for (i in seq_along(models)) {
  model = readRDS(paste0("./stan trained models step2/",models[i]))
  df_coef[i,] = c(names[i], "10%", get_summary(model = model, param = j))

}
for (i in seq_along(models)) {
  model = readRDS(paste0("./stan trained models step2/using2/",models[i]))
  df_coef[10+i,] = c(names[i], "≤2", get_summary(model = model, param = j))  
}
  df_coef[,3] = round(as.double(df_coef[,3]),2)
  df_coef[,4] = round(as.double(df_coef[,4]),2)
  df_coef[,5] = round(as.double(df_coef[,5]),2)
  df_coef[,6] = round(as.double(df_coef[,6]),2)
  names(df_coef)[3:5] = c("10%","50%","90%")
  df_coef = df_coef[order(df_coef$P.beta...0.),]
  
  saveRDS(df_coef,
          paste0("./Scripts - MRPs/step2/table_coefsummary/",j,".rds"))
  print(paste("done with coef:",j))
}


# create table in latex ---------------------------------------------------


df = readRDS(paste0("./Scripts - MRPs/step2/table_coefsummary/",
                    "eta[3]",
                    ".rds"))
xtable_data = xtable(df, caption = "Summary table for the intercept covariate")
print(xtable_data, caption.placement = "top", include.rownames = FALSE)

