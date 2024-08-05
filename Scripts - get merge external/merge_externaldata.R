
# Merging_2.0 -------------------------------------------------------------
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(igraph)


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.


setwd("/Users/francois/Documents/tweet_woc")




# presdential data --------------------------------------------------------


load("./Scripts - get merge external/1976-2020-president.RData" )
president = x
rm(x) #will clear all objects includes hidden objects.


# Tweet data --------------------------------------------------------------


df_tweets <- fromJSON("./tweets_inputdata/tweets_jan_minmonth5/df_full_Corona_JAN1_2023.json", flatten = T)
df_tweets <- distinct(df_tweets, df_tweets$id_str, .keep_all = T)


#### merging qualtrics -------------------------------------------------------

qualt = read.csv("Scripts - qualtrics merges/qualt_long.csv")


# merging M3 --------------------------------------------------------

m3 = read.csv("./Scripts - get merge external/external_data/m3_results.csv",
  numerals = c("no.loss"))  #R reads value innccorecctly because integer is too big; --> use no.loss

m3 = m3 %>% dplyr::rename(id_str = id)
m3 = dplyr::distinct(m3, m3$id_str, .keep_all = T)
m3 = m3[,-c(1,11)] #remove no info columns

df_tweets = left_join(df_tweets, m3,
  by = "id_str")
df_tweets = distinct(df_tweets, df_tweets$id_str, .keep_all = T)




# Merging geo -------------------------------------------------------------

geo <- fromJSON('./Scripts - get merge external/external_data/geo_results.json', 
                flatten = T)
geo <- geo %>%
  rename(geo.place_id = id)
geo <- distinct(geo, geo.place_id, .keep_all = T)

df_tweets <- left_join(df_tweets, geo,
                    by = 'geo.place_id')




# merging botometer -------------------------------------------------------

#Botometer API
botometer <- fromJSON("./Scripts - get merge external/external_data/botometer_results.json",
                      flatten = T)

botometer <- botometer %>% 
  rename(author_id = user.user_data.id_str,
         screen_name = user.user_data.screen_name)
botometer <- distinct(botometer, botometer$author_id, .keep_all = T)

df_tweets <- left_join(df_tweets, botometer,
                    by = c("screen_name"))




# merge BERT --------------------------------------------------------------
bert_tweets <- read.csv("/Users/francois/Documents/Python/data_GEO_BOT_M3_BERT/output/BERT_results_tweets.csv",
                    numerals = c("no.loss"))

bert_tweets <- distinct(bert_tweets, id_str, .keep_all = T)


bert_description <- read.csv("/Users/francois/Documents/Python/data_GEO_BOT_M3_BERT/output/BERT_results_description.csv",
                        numerals = c("no.loss"))

bert_description <- distinct(bert_description, author_id, .keep_all = T)


df_tweets <- left_join(df_tweets, bert_tweets,
                    by = "id_str")
df_tweets <- left_join(df_tweets, bert_description, 
                    by = "author_id")
