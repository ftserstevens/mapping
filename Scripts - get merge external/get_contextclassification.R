# library -----------------------------------------------------------------

library(plyr)
library(dplyr)
library(corrplot)
library(jsonlite)
library(httr)
library(tidyr)
library(stringi)
library(dismo)
library(maps)
library(ggplot2)
library(academictwitteR)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
setwd("/Users/francois/Documents/tweet_woc/")
tweets = fromJSON("./tweets_inputdata/tweets_jan_minmonth5/df_full_Corona_JAN1_2023.json", flatten = T)



context_vector = as.vector(NA)
tweet_vector = as.vector(NA)
tweet_idvec = as.vector(NA)


for(i in 1:nrow(tweets)) {
  names_cntxt = tweets$context_annotations[[i]]$entity.name[]
  context_vector <- c(context_vector,names_cntxt)
  
  number_twt = rep(i, length(names_cntxt))
  tweet_vector = c(tweet_vector, number_twt)
  
  tweet_id <- rep(tweets$id[i], length(names_cntxt))
  tweet_idvec <- c(tweet_idvec, tweet_id)
  
  if ((i/1000) == round(i/1000)) {print(paste0(round(i/nrow(tweets),2)*100, '%  ', Sys.time()))}
  if (i == nrow(tweets)) {print("done")}
  
}




#tweets that are not i there have no context annotations.
context <- cbind(context_vector, tweet_idvec)
context <- as.data.frame(context[-1,]) #remove the NA init.
context$context_vector = as.factor(context$context_vector)
context = distinct(context)
context$one = 1
context_dummies = context %>% tidyr::pivot_wider(names_from = context_vector, values_from = one, values_fill = 0)


#select the most represented contexts
context_rep = as.data.frame(colSums(context_dummies[,-1]))
context_rep$context = rownames(context_rep)
names(context_rep) = c("freq","context")
context_rep =context_rep[order(context_rep$freq, decreasing = T),][-(1:3),]
context_top = context_rep[1:50,]
rownames(context_top) =  1:nrow(context_top)

###calculate best context, low correlation (as in inlfation in the US == inflation)

dummies = as.matrix(context_dummies[,context_top$context])
crossprod = (t(dummies) %*% dummies)/
  diag((t(dummies) %*% dummies))
corrplot(crossprod, is.corr = F)

for (topic_init in 1:dim(crossprod)[2]) {
  if(exists("toberemoved")) {rm(toberemoved)}
  for(i in 1:dim(crossprod)[1]) {
    if(topic_init == i) {
      next
      }
    if(crossprod[topic_init,i] >.6 & crossprod[i,topic_init] >.6) {
      #print(paste("cols:",colnames(crossprod)[topic_init],"and",
      #  colnames(crossprod)[c(i)],'are highly co-occurent'))
      if(!exists("toberemoved")) {toberemoved = i} else {
        toberemoved = c(toberemoved,i)
        }
      }
  }
  if(exists("toberemoved")) {crossprod = crossprod [-toberemoved,-toberemoved]}
  if(topic_init >= dim(crossprod)[2]) {
    break
    }
  }
  

corrplot(crossprod[-1,-1], is.corr = F)


context_names = colnames(crossprod)[2:31] #we remove political figures as it is overly present.
selected_context = context_dummies[,c("tweet_idvec",context_names)] # select the contexts from OG data and also id_str
selected_context

colnames(selected_context) = c("id_str", paste0("context_", colnames(selected_context[2:dim(selected_context)[2]])))


saveRDS(selected_context, "./Scripts - get merge external/external_data/context_topics.rds")





