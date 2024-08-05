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
library(data.table)
library(cdlTools)
library(geojsonio)
library(tidyr)
library(sf)


#https://rapidapi.com/mescalcapi-mescalcapi-default/api/mescalc/ get misinformation score.



rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

setwd("~/Documents")
if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


# recreate final dataset by tweets ----------------------------------------------
df_tweets <- fromJSON("./tweets_inputdata/tweets_jan_minmonth5/df_full_Corona_JAN1_2023.json", flatten = T)
names(df_tweets) = gsub("\\.","_", names(df_tweets))
names(df_tweets) = gsub("_y", "_author", colnames(df_tweets))
names(df_tweets) = gsub("_x", "_tweet", colnames(df_tweets))
df_tweets = df_tweets[!names(df_tweets) == "tweets$id"]


# merging M3 --------------------------------------------------------

m3_df <- read.csv("./Scripts - get merge external/external_data/m3_results.csv",
                  numerals = c("no.loss"))  #R reads value innccorecctly because integer is too big; --> use no.loss

names(m3_df)
m3_df <- m3_df %>% dplyr::rename(id_str = id) %>% distinct(id_str, .keep_all = T)

m3_df$sex = ifelse(m3_df$gender_male >= m3_df$gender_female, "male","female")

m3_df$age = apply(m3_df[,grep("age" ,names(m3_df)),], 1, function(x) {which(x == max(x))})
m3_df$age = ifelse(m3_df$age == 1, "(-1,18]", ifelse(m3_df$age == 2, "(18,30]", ifelse(m3_df$age == 3, "(30,40]", "(40,99]")))
  



df_tweets = merge( df_tweets, m3_df,
                   by = 'id_str',
                   all.X = T, all.y = T)

print(paste(nrow(df_tweets), "after m3 merge"))









# import model based metrics ----------------------------------------------


model_estim = readRDS("./estimations.rds")[,-c(2)]
names(model_estim)[-c(1,10)] = paste0("model_",names(model_estim)[-c(1,10)])


model_estim <- model_estim %>%
  mutate(id_str = as.character(id_str)) %>%
  mutate_at(vars(-id_str), as.numeric) %>%
  mutate(model_1for1 = (model_dem_discestimates + model_rep_discestimates)/2)


df_tweets = merge(df_tweets, model_estim,
      by = "id_str")

nrow(df_tweets)
# raked imports -----------------------------------------------------------


imports = c("./Scripts - MRPs/aggregated_accuracy/raked_totbal.rds"
)


suffixes = c(""

             )

for (i in seq_along(imports)) {
  import_df = readRDS(imports[i])
  import_df$raked_average = as.numeric(import_df$raked_average)
  names(import_df)[2:3] =  paste0(names(import_df)[2:3], suffixes[i])
  df_tweets = merge(df_tweets, import_df,
                    by.x = "id_str",
                    by.y = "id_str",
                    all.x = T, all.y = F,
                    suffixes = c("",suffixes[i]))
  
}

  

# import raw averages ----------------------------------------------


imports = c("./Scripts - MRPs/aggregated_accuracy/raw_average.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_1for1.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_average_dem.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_average_rep.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_average_old.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_average_young.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_average_female.rds",
            "./Scripts - MRPs/aggregated_accuracy/raw_average_male.rds"
           )



suffixes = c("",
             "_1for1",
             "_onlydem",
             "_onlyrep",
             "_old",
             "_young",
             '_female',
             '_male'
             )

for (i in seq_along(imports)) {
  import_df = readRDS(imports[i])
  import_df$raw_average = as.numeric(import_df$raw_average)
  names(import_df)[2:3] =  paste0(names(import_df)[2:3], suffixes[i])
  df_tweets = merge(df_tweets, import_df,
                    by.x = "id_str",
                    by.y = "id_str",
                    all.x = T, all.y = F,
                    suffixes = c("",suffixes[i]))
  
}

nrow(df_tweets)

df_tweets$count

# merging botometer -------------------------------------------------------

botometer <- fromJSON("./Scripts - get merge external/external_data/botometer_results.json",
                      flatten = T)

botometer <- botometer %>% 
  dplyr::rename(author_id = user.user_data.id_str, 
         screen_name = user.user_data.screen_name) %>% 
  distinct(botometer, botometer$author_id, .keep_all = T)


df_tweets <- merge(df_tweets, botometer,
                    by = c("author_id"),
                   all.x = T, all.y = F)

print(paste(nrow(df_tweets), "after botometer merge"))



# Merging geo -------------------------------------------------------------

geo = fromJSON("./Scripts - get merge external/external_data/geo_results.json", 
                flatten = T)[,1:8]
#geo$state = sub(".*, ", "", geo$full_name)# Extract characters after pattern
#geo$state = substr(geo$full_name, nchar(geo$full_name)-1, nchar(geo$full_name))


geo = geo %>% dplyr::rename(geo.place_id = id) %>% 
  distinct(geo.place_id, .keep_all = T)

geo$state <- NA

usa <- readRDS("./census data/usa_sfmap.rds")


# compare points
for (i in 1:nrow(geo)) {
  coords <- unlist(geo$centroid[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
  sp::proj4string(point) <- sp::proj4string(usa)
  polygon_check <- sp::over(point, usa)
  geo$state[i] <- as.character(polygon_check$NAME)
}




geo$state[which(is.na(geo$state))] = "Maryland"

geo$state = cdlTools::fips(gsub(" ","", geo$state), to ="Abbreviation") #Name
geo$state[which(is.na(geo$state))] = "DC"
geo$state = factor(geo$state)

geo = geo[,c("geo.place_id","full_name","centroid","state")]
names(geo) = c("geo_place_id","place_name","centroid","state")

df_tweets <- merge(df_tweets, geo,
                   by = 'geo_place_id',
                   all.x = T)

df_tweets = df_tweets[-which((is.na(df_tweets$state))),]
df_tweets = df_tweets[-which(df_tweets$state=="PR"),]

df_tweets$centroid <- substr(df_tweets$centroid, 3, nchar(df_tweets$centroid) -1)
a <- str_split_fixed(df_tweets$centroid, ",", 2)
df_tweets$lon <- as.numeric(a[,1])
df_tweets$lat <- as.numeric(a[,2])

df_tweets$state = factor(df_tweets$state)
df_tweets$state_short = factor(df_tweets$state)
df_tweets$state = as.numeric(df_tweets$state_short)



print(paste(nrow(df_tweets), "after geo merge"))






# merging misinformaiton data ---------------------------------------------



mis = fromJSON("./Scripts - get merge external/external_data/misinformation_results.json", flatten = T)
mis$twitter_user_id = as.character(mis$twitter_user_id)
mis = mis %>% dplyr::rename(author_id = twitter_user_id ) %>% dplyr::select(-c(2,6,8))




df_tweets = merge(x = df_tweets, y = mis,
                  all.x = T, all.y = F,
                  by= 'author_id',
                  suffix.y = "mis")

df_tweets$party = ifelse(df_tweets$partisan_score<=-.25,"Democrat",
                   ifelse(df_tweets$partisan_score >= .25, "Republican", 'Neutral'))



df_tweets$party = factor(ifelse(is.na(df_tweets$party), "Neutral" ,df_tweets$party)) ### no party is a level 





# merging context ---------------------------------------------------------

context = readRDS("./Scripts - get merge external/external_data/context_topics.rds")
df_tweets = merge(df_tweets, context,
                  by = "id_str")


nrow(df_tweets)


# prep ipums data -------------------------------------------------


ipums = fread("./census data/census_ipums.csv")
state_fip <- data.frame(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 60, 66, 69, 72, 78),
  STATE = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI")
)
ipums = dplyr::left_join(ipums,state_fip, by = 'STATEFIP') ## replace fips by state abr
ipums$WHITE = ifelse(ipums$RACE == 1, 1, 0) ## bin for white ethincity

ipums$UNDERGRAD = ifelse(ipums$GRADEATT == 0, NA, ifelse(ipums$GRADEATT >= 6, 1, 0))

nrow(df_tweets)


# merge witheperc ---------------------------------------------------------



ethwhite = ipums %>% dplyr::group_by(STATE) %>% dplyr::summarize(mean(WHITE)) %>% 
  dplyr::rename( white_perc = `mean(WHITE)`,state = STATE) %>%
  mutate(state = as.factor(state))
df_tweets$state_short

df_tweets = merge(df_tweets, ethwhite, 
                  all.x = T, all.y = F,
                  by.x = "state_short",
                  by.y = "state")


# merge edu by state ------------------------------------------------------

edu = ipums[!is.na(ipums$UNDERGRAD),] %>% dplyr::group_by(STATE) %>% dplyr::summarize(mean(UNDERGRAD)) %>% 
  dplyr::rename(state = STATE, undergrad_perc = `mean(UNDERGRAD)`)


df_tweets = merge(df_tweets, edu,
                             all.x = T, all.y = F,
                             by.x = "state_short",
                             by.y = "state")


nrow(df_tweets)



# pop density -------------------------------------------------------------

##ipums desity is wacky since it's done by PUMA
density = ipums %>% dplyr::group_by(STATE) %>% dplyr::summarize(mean(DENSITY)) %>% 
  dplyr::rename( tot_dens = `mean(DENSITY)`,state = STATE)

#from: https://www.census.gov/data/tables/time-series/dec/density-data-text.html
pop = readxl::read_excel("./census data/population-density.xlsx")
names(pop) = c("state","pop_count","pop","pop_rank")
pop$state = ifelse(pop$state == 'District of Columbia', "DC",pop$state)
pop$state = as.factor(cdlTools::fips(gsub(" ","", pop$state), to ="Abbreviation")) #Name


df_tweets = merge(df_tweets, pop,
                  all.x = T, all.y = F,
                  by.x = "state_short",
                  by.y = "state")


nrow(df_tweets)





# standardize fixed effects -----------------------------------------------

### standardize fixed effects
df_tweets[c("white_perc","undergrad_perc","pop")] <- 
  lapply(df_tweets[c("white_perc","undergrad_perc","pop")],
         function(x) c(scale(x)))



# save R object(step2a) -----------------------------------------------------------


saveRDS(df_tweets, file = "./merged_step2.rds")

