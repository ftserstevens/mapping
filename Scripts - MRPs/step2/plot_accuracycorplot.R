library(ggplot2)
library(ggthemes)
library(ggtext)
library(corrplot)
library(tidyr)
library(dplyr)


# metric plotting ----------------------------------------------------------
df = readRDS("~/Documents/tweet_woc/merged_step2.rds")


# corr matrix -------------------------------------------------------------


mat = as.data.frame(df[df$count >= (0), c("raw_average",
                                          "raw_average_1for1",
                                          "raked_average",
                                          "model_raw", "model_1for1","model_woc_discestimates",
                                          "raw_average_onlydem",
                                          "raw_average_onlyrep",
                                          "model_dem_discestimates","model_rep_discestimates",
                                          "raw_average_young" , "raw_average_old",
                                          "raw_average_male", "raw_average_female"
                                          )])


names(mat) = c("Naive Sample","Naive Balanced", "Naive Population" ,
               "Model Sample","Model Balanced","Model Population",
               "Naive Democrat","Naive Republican",
               "Model Democrat","Model Republican",
               "Naive Young","Naive Old","Naive Male","Naive Female"
               )
mat = mat[complete.cases(mat),]

pdf("./Scripts - MRPs/step2/accuracy_corrmat/corrplot.pdf", width = 10, height = 10)
corrplot::corrplot(cor(mat), method = "color", type = "full", 
                   #order = 'hclust', 
                   diag = T, 
                   cl.pos = "n",
                   addCoef.col = 'black', outline = T,   tl.col = "black")


corrplot::corrplot(cor.mtest(mat)$p, method = "color", type = "lower", 
                   #order = 'hclust', 
                   add = T, 
                   
                   diag = F,
                   tl.pos = "n",
                   cl.pos = "n",
                   addCoef.col = 'black', outline = T,   tl.col = "black",
                   col =c(rep('white' , 11),rep('#ee6b6e', 9))
)

rect(xleft = 0.5, xright = 6.5, ybottom = 8.5, ytop = 14.5, col = NA, border = "orange", lwd = 3)
#text(x = .01, y = 8.3, labels = "Wisdom of the Crowds Metrics", pos = 4, col = "orange", font = 2)

dev.off()

# expanded corrmat --------------------------------------------------------


# corr matrix -------------------------------------------------------------

mat = as.data.frame(df[df$count >= (0), c("raw_average",
                                          "raw_average_1for1",
                                          "raked_average",
                                          "model_raw", "model_1for1","model_woc_discestimates",
                                          "raw_average_onlydem",
                                          "raw_average_onlyrep",
                                          "model_dem_discestimates","model_rep_discestimates",
                                          "raw_average_young" , "raw_average_old",
                                          "model_young_discestimates", "model_old_discestimates",
                                          "raw_average_male", "raw_average_female",
                                          "model_male_discestimates", "model_female_discestimates"
)])


names(mat) = c("Naive Sample","Naive Balanced", "Naive Population" ,
               "Model Sample","Model Balanced","Model Population",
               "Naive Democrat","Naive Republican",
               "Model Democrat","Model Republican",
               "Naive Young","Naive Old","ModelYoung","Model Old",
               "Naive Male","Naive Female", "Model Male","Model Female"
)
mat = mat[complete.cases(mat),]


corrplot::corrplot(cor(mat), method = "color", type = "full", 
                   #order = 'hclust', 
                   diag = T, 
                   cl.pos = "n",
                   addCoef.col = 'black', outline = T,   tl.col = "black")


corrplot::corrplot(cor.mtest(mat)$p, method = "color", type = "lower", 
                   #order = 'hclust', 
                   add = T, 
                   
                   diag = F,
                   tl.pos = "n",
                   cl.pos = "n",
                   addCoef.col = 'black', outline = T,   tl.col = "black",
                   col =c(rep('white' , 11),rep('#ee6b6e', 9))
)





