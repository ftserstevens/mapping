library (ggplot2)
library(data.table)
library(dplyr)

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.



df = as.data.table(readRDS("./step1_preprocessed.rds"))
census = readRDS("./census data/census.rds")[,1:5] #no need for state level info
census = as.data.table(lapply(census, function(x) as.numeric(x))) ### set the census to have numeric columns instead of factors - will use for matching.
tweets = readRDS("~/Documents/tweet_woc/merged_step2.rds")


# census numbers ----------------------------------------------------------



sex=group_by(census, SEX) %>% summarise(N = sum(N)) %>% mutate(N = N/sum(N))
age=group_by(census, AGE) %>% summarise(N = sum(N)) %>% mutate(N = N/sum(N))



female =  sex$N[1]
age_18 =age$N[1]
age_30 =age$N[2]
age_40 =age$N[3]
age_99 =age$N[4]
Biden = .513
Trump = .468
Turnout = .666
###from ANES
Democrat = 0.3455314 
Neutral =  0.3450483  
Republican = 0.3094203

# Reviewers numbers -------------------------------------------------------


female_s = mean(df$sex_num == 1, na.rm = T)
age_18_s =mean(df$age_cat_num == 1, na.rm = T)
age_30_s =mean(df$age_cat_num == 2, na.rm = T)
age_40_s =mean(df$age_cat_num == 3, na.rm = T)
age_99_s =mean(df$age_cat_num == 4, na.rm = T)
biden_s = mean(df$Candidate2020 == "Joe Biden", na.rm = T)
trump_s = mean(df$Candidate2020 == "Donald Trump", na.rm = T)
turnout_s = mean(df$Vote2020 == "Yes", na.rm = T)
democrat_s = (table(df$pol_party)/nrow(df))[1]
neutral_s = (table(df$pol_party)/nrow(df))[2]
republican_s = (table(df$pol_party)/nrow(df))[3]





# Tweet authors numbers ---------------------------------------------------

tweets$age = as.numeric(factor(tweets$age))

female_t = mean(tweets$sex == "female")
age_18_t = mean(tweets$age == 1, na.rm = T)
age_30_t =mean(tweets$age == 2, na.rm = T)
age_40_t =mean(tweets$age == 3, na.rm = T)
age_99_t =mean(tweets$age == 4, na.rm = T)
democrat_t = mean(tweets$party == "Democrat", na.rm = T)
republican_t = mean(tweets$party == "Republican", na.rm = T)
neutral_t = mean(tweets$party == "Neutral", na.rm = T)





category = c("Female","-18","19-29","30-39","40+",
             "Biden votes","Trump votes","Turnout", 
             "Democrat" , "Republican", "Neutral")
census_prop = c(female ,age_18  ,age_30 ,age_40 ,age_99 ,Biden  ,Trump ,Turnout, Democrat,Republican, Neutral)
sample_prop = c(female_s ,age_18_s ,age_30_s ,age_40_s ,age_99_s ,biden_s ,trump_s ,turnout_s, democrat_s, republican_s, neutral_s)
tweet_prop = c(female_t ,age_18_t ,age_30_t ,age_40_t ,age_99_t ,10 ,10 ,10, democrat_t, republican_t, neutral_t)  


# plotting ----------------------------------------------------------------


### matchplot sample 

df_plot = as.data.frame(cbind(category,census_prop, sample_prop,tweet_prop))
df_plot$census_prop = as.numeric(df_plot$census_prop)
df_plot$sample_prop = as.numeric(df_plot$sample_prop)
df_plot$tweet_prop = as.numeric(df_plot$tweet_prop)

# nudging the text to limit text overlap
df_plot$nnudge_s = c(-.2,-.2,-0,-.2,-0,+.8,+0.5,+0.5,-.2,+1,+1)
df_plot$vnudge_s = c(+.2, 0,-.5,.5,-.5,-.5,-.5,-.5,0,0,-.3 )


correlation = cor(df_plot$sample_prop, df_plot$census_prop)
mae = mean(abs(df_plot$sample_prop - df_plot$census_prop))


matchplot_sample = ggplot(data= df_plot, map = aes( x= sample_prop, y = census_prop, label = category)) + 
  geom_point() +  
  geom_abline(intercept = 0, slope = 1, color = "darkblue") +
  geom_text(size = 6, hjust = df_plot$nnudge_s, vjust = df_plot$vnudge_s) +
  #geom_point(data= df_plot, map = aes( x= tweet_prop, y = census_prop, label = category), color = "blue" ) +  
  geom_text(x = .16, y = .16, size = 6, label = "Census and sample are matched", alpha = .2,color = "darkblue", angle = 45, hjust = -1, vjust = -1) +
  xlim(0, 1) + ylim(0, 1) + theme_minimal() + xlab("Sample proportions") +
  ylab("Census proportions") +
  theme(axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
  annotate("text", x = 0.05, y = 0.95, label = paste("Correlation:", round(correlation, 2)), hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.05, y = 0.90, label = paste("MAE:", round(mae, 2)), hjust = 0, vjust = 1, size = 6)
matchplot_sample
ggsave(filename = "./Scripts - MRPs/matchplot_figures/matchplot_sample.pdf", 
       plot = matchplot_sample, width = 7.2, height = 7.2, units = "in")

### matchplot twitter

# nudging the text to limit text overlap
df_plot$nnudge_t = c(-.1,-.2,-0,-.2,-0,0,+0,0,-0.1, +1.1 ,+1)
df_plot$vnudge_t = c(.1,0,-.5,.7,-.1,0,0,0,0,0,-.1 )

correlation_twitter = cor(df_plot$tweet_prop[-c(6:8)], df_plot$census_prop[-c(6:8)]) ### -6:8 removes the rows not present for the Twitter comparison.
mae_twitter = mean(abs(df_plot$tweet_prop[-c(6:8)] - df_plot$census_prop[-c(6:8)])) ### turnout, Biden & Trump votes



matchplot_twitter = ggplot(data= df_plot, map = aes( x= tweet_prop, y = census_prop, label = category)) + 
  geom_point() +  
  geom_abline(intercept = 0, slope = 1, color = "darkblue") +
  geom_text(size = 6,vjust = df_plot$vnudge_t, hjust = df_plot$nnudge_t) +
  geom_text(x = 0.03, y = 0.03, size = 6, label = "Census and tweet authors are matched", alpha = .2,color = "darkblue", angle = 45, hjust = -1, vjust = -1) +
  xlim(0, 1) + ylim(0, 1) + theme_minimal() + xlab("Twitter proportions") +
  ylab("Census proportions") + 
  annotate("text", x = 0.05, y = 0.95, label = paste("Correlation:", round(correlation_twitter, 2)), hjust = 0, vjust = 1, size = 6) +
  annotate("text", x = 0.05, y = 0.90, label = paste("MAE:", round(mae_twitter, 2)), hjust = 0, vjust = 1, size = 6) +
  theme(axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))
matchplot_twitter 
ggsave(filename = "./Scripts - MRPs/matchplot_figures/matchplot_twitter.pdf", 
       plot = matchplot_twitter, width = 7.2, height = 7.2, units = "in")

