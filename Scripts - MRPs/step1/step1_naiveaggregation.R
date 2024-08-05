
# Packages ----------------------------------------------------------------
library(ipumsr)
library(terra)
library(zipcodeR)
library(rstan)
library(fastDummies)
library(dplyr)
library(parallel)
library(mltools)
library(gridExtra)
library(anesrake)
library(readr)
library(stringr)
library(jsonlite)
library(stringi)
library(survey)
library(cdlTools)
library(lme4)
library(ggplot2)
library(caret) 
library(mltools)
library(rlist)
library(jsonlite)
library(bayesplot)

if (!grepl("tweet_woc", getwd())) {setwd("~/Documents/tweet_woc/")}


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
ilogit = function(x) {exp(x)/(1+ exp(x)) }


df = readRDS("./step1_preprocessed.rds")
df$Candidate2020 = ifelse(is.na(df$Candidate2020), "No Vote",df$Candidate2020)
df$N = 1


table(df$party_registration)
table(df$pol_party)

# define functions --------------------------------------------------------

quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

aggregate = function(df, weights = F) {
  if(weights == F) {return(
  df %>% 
    dplyr::group_by(id_str) %>%  
    dplyr::summarise(raw_average = mean(assessment_linear), 
                     raw_sd = sd(assessment_linear), 
                     count = sum(N), 
                     raw_confidence = mean(confidence_linear))
  )}

    
  
  if(weights) { return(
    df = df %>% dplyr::group_by(id_str) %>% 
      dplyr::summarize(raked_average = weighted.mean(assessment_linear, weight), 
                       raked_confidence = weighted.mean(confidence_linear, weight))
    )}
    
    
}
### calculate the raw_average per id_str for col assessement_linear

raw_1for1 = function(df, nseed = 0) {
  set.seed(nseed)
  
  
  #df_onlyvote = df[(df$Candidate2020 %in% c("Joe Biden", "Donald Trump")),]
  df_onlyparty = df[(df$pol_party %in% c("Democrat", "Republican")),]
  
  df_onlyparty$pol_party = factor(df_onlyparty$pol_party)
  all_idstr = unique(df_onlyparty$id_str)
  all_count = rep(NA, length(all_idstr))
  df_1for1_raw = df_onlyparty[0,] #empty df init
  
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = length(all_idstr), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar

  for (i in 1:length(all_idstr)) {
    id_str.i = unique(df_onlyparty$id_str)[i]
    df_idstr.i = df_onlyparty[df_onlyparty$id_str == id_str.i,]
    table.i = table(df_idstr.i$pol_party)
    min_1for1 = min(table.i)
    all_count[i] =  (min_1for1*2)
    
    if(0 %in% table.i) {next}## exclude id_str if no representation of 1 vote
    
    df_1for1_raw.b = df_onlyparty[0,] #empty df init
    for (b in 1:10) {#bootstrap 

      df_idstr.i.biden = dplyr::sample_n(
        df_idstr.i[df_idstr.i$pol_party == "Democrat",], size = min_1for1)
      df_idstr.i.trump = dplyr::sample_n(
        df_idstr.i[df_idstr.i$pol_party == "Republican",], size = min_1for1)
    
      df_1for1_raw.b = rbind(df_1for1_raw.b, df_idstr.i.biden, df_idstr.i.trump)
    }
    
    
    
    df_1for1_raw.b = aggregate(df_1for1_raw.b)
    df_1for1_raw.b = df_1for1_raw.b[, -c(grep("count",names(df_1for1_raw.b)))] ##remove count as it's 1for1
    df_1for1_raw = rbind(df_1for1_raw, df_1for1_raw.b)
    
    setTxtProgressBar(pb, i)
  }  
  
  #df_1for1_raw = aggregate(df_1for1_raw)
  #df_1for1_raw = df_1for1_raw[, -c(grep("count",names(df_1for1_raw)))] ##remove count as it's 1for1
  
  
  df_count = as.data.frame(cbind(as.character(all_idstr), all_count))
  names(df_count) = c('id_str','count')
  
  df_1for1_raw = merge(df_1for1_raw, df_count,
                       by = "id_str", all.x = T, all.y = T)
  
  close(pb)
  return(df_1for1_raw)
}


rake_totbal = function(df) {
  
  df_rake = df
  
  df_rake$case_id = 1:nrow(df_rake)
  df_rake$Candidate2020 = factor(df_rake$Candidate2020)
  df_rake$sex = factor(df_rake$sex)
  
  age_categories <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  age_breaks <- c(18, 25, 35, 45, 55, 65, Inf)
  df_rake$age_category <- cut(df_rake$age, breaks = age_breaks, labels = age_categories, right = FALSE)
  df_rake$age_category[df_rake$age < 18] <- "18-24"
  
  
  
  sex_levels = levels(df_rake$sex)
  age_levels = levels(df_rake$age_category)
  vote_levels = levels(df_rake$Candidate2020)
  
  
  # 66.8 participation rate of US 2020 - https://www.census.gov/newsroom/press-releases/2021/2020-presidential-election-voting-and-registration-tables-now-available.html
  
  vote_target = c(.668 *46.8, .668* 1.9,.668 * 51.3, 100-66.8)
  names(vote_target) = c("Donald Trump","Independent candidate","Joe Biden","No Vote")
  
  sex_target = c(0.5076142, 0.4923858)
  names(sex_target) = c("Female", "Male")
  
  age_target = c(0.149, 0.169, 0.162, 0.152, 0.160, 0.208)
  names(age_target) = c("18-24","25-34","35-44","45-54","55-64","65+")

  targets = list(vote_target, sex_target, age_target)
  names(targets) <- c("Candidate2020", "sex","age_category")

  rakeout = anesrake::anesrake(inputter = targets, 
                               dataframe = df_rake, 
                               caseid = df_rake$case_id, 
                               verbose = F)
  
  df_rake$weight = rakeout$weightvec
  
  df_rake = aggregate(df_rake, weights = T)
  
  
  return(df_rake)
  
  
  
}
#rake for a given dist of party, gender and age

# calculate naive metrics -------------------------------------------------------------------




#Sample
saveRDS(aggregate(df), "./scripts - MRPs/aggregated_accuracy/raw_average.rds")
#Balanced


df_1for1 = raw_1for1(df)
saveRDS(df_1for1, "./Scripts - MRPs/aggregated_accuracy/raw_1for1.rds")### takes about 20 mins
#remove 2nd for loop to make faster (replicate function ?)



#Population
saveRDS(rake_totbal(df), "./Scripts - MRPs/aggregated_accuracy/raked_totbal.rds")


#Partisan
dem = df$pol_party == "Democrat"
rep = df$pol_party == "Republican"

saveRDS(aggregate(df[dem,]), "./Scripts - MRPs/aggregated_accuracy/raw_average_dem.rds")
saveRDS(aggregate(df[rep,]), "./Scripts - MRPs/aggregated_accuracy/raw_average_rep.rds")


#Age
old = df$age >= mean(df$age)
saveRDS(aggregate(df[old,]), "./Scripts - MRPs/aggregated_accuracy/raw_average_old.rds")
saveRDS(aggregate(df[!old,]), "./Scripts - MRPs/aggregated_accuracy/raw_average_young.rds")


#female
sexf = df$sex =='Female'
sexm = df$sex == "Male"
saveRDS(aggregate(df[sexf,]), "./Scripts - MRPs/aggregated_accuracy/raw_average_female.rds")
saveRDS(aggregate(df[sexm,]), "./Scripts - MRPs/aggregated_accuracy/raw_average_male.rds")





