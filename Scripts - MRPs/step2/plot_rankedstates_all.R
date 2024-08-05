library(ggplot2)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}

use2 = F
lo = F


files = list.files(paste0("./Scripts - MRPs/step2/state_intervals/",if(lo){"logged_odds/"}else("probability/"),if(use2) {"using2/"}), pattern = ".rds")
f_names = substr(files, 1, nchar(files)-4)



for (i in seq_along(files)) {
  if(i == 1) {
    df = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/",if(lo){"logged_odds/"}else("probability/"),if(use2) {"using2/"},files[i]))[,]
    df$state = rownames(df)
    df$med_order = rank(df$`50%`)
    df = df[,c("state","med_order")]
    names(df)[2] = paste0("med_order_", f_names[i])
    next
    }
  df.i = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/",if(lo){"logged_odds/"}else("probability/"),if(use2) {"using2/"},files[i]))[,]
  df.i$state = rownames(df.i)
  df.i$med_order = rank(df.i$`50%`)
  df.i = df.i[,c("state","med_order")]
  names(df.i)[2] = paste0("med_order_", f_names[i])
  df = merge(df, df.i, by = 'state')
    
}



df$avg_nb = rowMeans(df[,-c(
  grep("state", names(df)),
  grep("med_order_model_dem_discestimates", names(df)),
  grep("med_order_model_rep_discestimates", names(df)),
  grep("med_order_raw_average_1for1", names(df)),
  grep("med_order_raw_average_onlydem", names(df)),
  grep("med_order_raw_average_onlyrep", names(df))
)])



df$state <- factor(df$state, levels = df$state[order(df$avg_nb)])
statesdist_all = ggplot(data = df, mapping = aes(x = avg_nb, y = state)) +
  geom_point()  +theme_minimal() +
  geom_point(mapping = aes(x=df[,"med_order_model_1for1"], y = state), shape =1, alpha = .25) +
  geom_point(mapping = aes(x=df[,"med_order_model_raw"], y = state), shape =1 ,alpha = .25)  +
  geom_point(mapping = aes(x=df[,"med_order_model_woc_discestimates"], y = state), shape =1 ,alpha = .25)  +
  geom_point(mapping = aes(x=df[,"med_order_raked_average"], y = state), shape =1 ,alpha = .25)  +
  geom_point(mapping = aes(x=df[,"med_order_raw_average"], y = state), shape =1 ,alpha = .25) +
  geom_point(mapping = aes(x=df[,"med_order_raw_average_onlydem"], y = state), shape =3 ,alpha = 1, size = 2, color = "blue") +
  geom_point(mapping = aes(x=df[,"med_order_raw_average_onlyrep"], y = state), shape =3 ,alpha = 1, size = 2, color = "red") +
  geom_errorbar(aes(xmin = pmin(df[,"med_order_raw_average_onlydem"], df[,"med_order_raw_average_onlyrep"]), 
                    xmax = pmax(df[,"med_order_raw_average_onlydem"], df[,"med_order_raw_average_onlyrep"]), 
                    width = 0.1), alpha = .4 ) + 

  xlab("Average Rank of State") + ylab("State")

ggsave(statesdist_all,
       units = "in",height =12,width = 8,
       filename = paste0("./Scripts - MRPs/step2/plots_rankedstates/stateorder_all",if(use2) {"_use2"},if(lo) {"_lo"},".pdf"))

  
