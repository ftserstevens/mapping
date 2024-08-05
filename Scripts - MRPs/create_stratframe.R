# Packages ----------------------------------------------------------------
library(ipumsr)
library(zipcodeR)
library(fastDummies)
library(dplyr)
library(mltools)
library(gridExtra)
library(readr)
library(stringr)
library(jsonlite)
library(stringi)
library(survey)
library(cdlTools)
library(ggplot2)
library(caret) 
library(mltools)
library(rlist)
library(data.table)
library(cdlTools)
library(geojsonio)
library(tidyr)





rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


# census prep -------------------------------------------------------------
census <- readRDS("./census data/census_ipums.rds")

census = census %>% mutate(AGE = cut(AGE, breaks=c(-1, 18, 30, 40, 99)))
census$N = 1

census_gb = census %>% group_by(STATEFIP, SEX, AGE) %>% summarise(sum(N)) %>% rename (N = `sum(N)`) 

census_gb$STATE = as.factor(cdlTools::fips(census_gb$STATEFIP, to ="Abbreviation")) #Name
census_gb$AGE = as.factor(census_gb$AGE)
census_gb$SEX = as.factor(ifelse(census_gb$SEX == 1, "male","female"))
census_gb = census_gb %>% relocate(N, .before = SEX)

census_gb = data.table::as.data.table(census_gb[,-c(1)])
census_gb = census_gb %>% relocate(N, .before = SEX)




# registered voters -------------------------------------------------------


anes <- data.table::fread("Scripts - get merge external/anes.csv")

table(anes$vote20)/nrow(anes)
anes$vote20 = ifelse(anes$V201033 == 1, "Biden20",
                     ifelse(anes$V201033 == 2, "Trump20", "other20"))

anes$party = factor(ifelse(anes$V201231x <= 0, "Neutral",
                    ifelse(anes$V201231x <= 2, "Democrat",
                           ifelse(anes$V201231x <= 5, "Neutral", "Republican"))))
table(anes$party)/nrow(anes)

anes$gender = ifelse(anes$V201600 == -9, "Refused",ifelse(anes$V201600 == 1, "male","female"))
#anes = anes[anes$party != 'no answer' | anes$gender == "Refused",]

{ anes_states = as.data.frame(cbind(
  as.integer(unique(anes$V203000)[order(unique(anes$V203000))]),
  c("Alabama","Alaska","Arizona",
    "Arkansas", "California","Colorado",
    "Connecticut" ,"Delaware",
    "Washington DC",
    "Florida" ,"Georgia", "Hawaii",
    "Idaho", "Illinois","Indiana",
    "Iowa","Kansas","Kentucky",
    "Louisiana" ,"Maine" ,"Maryland",
    "Massachusetts","Michigan","Minnesota",
    "Mississippi", "Missouri", "Montana",
    "Nebraska","Nevada","New Hampshire",
    "New Jersey", "New Mexico" , "New York",
    "North Carolina", 'North Dakota' ,"Ohio",
    "Oklahoma", "Oregon","Pennsylvania",
    "Rhode Island","South Carolina","South Dakota",
    "Tennessee","Texas","Utah",
    "Vermont","Virginia","Washington",
    "West Virginia" ,"Wisconsin","Wyoming")))
  names(anes_states) = c("V203000","state")
  anes_states$V203000 = as.integer(anes_states$V203000)
} ### ANES state codebook



anes = merge(anes, anes_states,
             by = "V203000") ###merge states.
anes$N_anes = 1
anes = anes[,c("gender","party","state",
               #"vote20",
               "N_anes")]
anes = anes[(anes$gender != "Refused" &
               anes$party !="no answer"),]

### check for non represented table
## create list of all possible combinations
cross_joined_table <- expand.grid(gender = unique(anes$gender), 
                                  party = unique(anes$party), 
                                  state = unique(anes$state)
                                  #,vote20 = unique(anes$vote20)
                                  )
cross_joined_table$N_anes = 0 # set non existent case = 0

anes_temp = merge(anes, cross_joined_table,
                  by = c("gender","party","state"
                         #,"vote20"
                         ), all.y = T)

anes_temp$N_anes = ifelse(is.na(anes_temp$N_anes.x),0,1)
anes = anes_temp[, -c('N_anes.x', 'N_anes.y')]

print(paste(table(anes$N_anes)[1], "unrepresented classes in anes datset, adding them as n=0" ))





anes_gb = anes %>% group_by(gender,party,state) %>% summarise(N_anes = sum(N_anes)) 
anes_gb$state = as.character(anes_gb$state)
anes_gb$state = ifelse(anes_gb$state == "Washington DC", "DC", anes_gb$state) # cld is weird with DC hence manual transformation
anes_gb$state = cdlTools::fips(gsub(" ","", anes_gb$state), to ="Abbreviation") #Name


anes_gb = anes_gb %>% pivot_wider(values_from = N_anes, names_from = party)

anes_gb$Democrat = ifelse(is.na(anes_gb$Democrat),0,anes_gb$Democrat)
anes_gb$Neutral = ifelse(is.na(anes_gb$Neutral),0,anes_gb$Neutral)
anes_gb$Republican =ifelse(is.na(anes_gb$Republican),0,anes_gb$Republican)



anes_gb = merge(census_gb, anes_gb, 
                by.y = c("state", "gender"),
                by.x = c('STATE', "SEX"))


dem = anes_gb$N * (anes_gb$Democrat/(anes_gb$Democrat + anes_gb$Neutral + anes_gb$Republican))
neu = anes_gb$N * (anes_gb$Neutral/(anes_gb$Democrat + anes_gb$Neutral + anes_gb$Republican))
rep = anes_gb$N * (anes_gb$Republican/(anes_gb$Democrat + anes_gb$Neutral + anes_gb$Republican))

anes_gb$Democrat = dem
anes_gb$Neutral = neu
anes_gb$Republican = rep

anes_gb = anes_gb %>% pivot_longer(cols = c(Democrat,Neutral, Republican), names_to = "PARTY")
anes_gb$N = anes_gb$value 
anes_gb = anes_gb %>% relocate(.before =STATE, N )
anes_gb = anes_gb[,-c(6)]

census = anes_gb

# add  and eth  rel .... to census -----------------------------------------------

##load Ipums
ipums = fread("./census data/census_ipums.csv")
state_fip <- data.frame(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 60, 66, 69, 72, 78),
  STATE = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI")
)
ipums = dplyr::left_join(ipums,state_fip, by = 'STATEFIP') ## replace fips by state abr


ipums$WHITE = ifelse(ipums$RACE == 1, 1, 0) ## bin for white ethincity
ipums$UNDERGRAD = ifelse(ipums$GRADEATT == 0, NA, ifelse(ipums$GRADEATT >= 6, 1, 0))

edu = ipums %>% dplyr::group_by(STATE) %>% 
  dplyr::summarize(undergrad_perc = mean(UNDERGRAD, na.rm =T))

ethwhite = ipums %>% dplyr::group_by(STATE) %>% dplyr::summarize(white_perc = mean(WHITE))


#from: https://www.census.gov/data/tables/time-series/dec/density-data-text.html
pop = readxl::read_excel("./census data/population-density.xlsx")[,c(1,3)]
names(pop) = c("STATE","pop")
pop$STATE = cdlTools::fips(gsub(" ","", pop$STATE), to ="Abbreviation") #Name
pop$STATE = ifelse(is.na(pop$STATE), "DC",pop$STATE) ## DC is NA so place it back in.

##from:: https://www.pewresearch.org/religion/religious-landscape-study/religious-tradition/evangelical-protestant/
rel = fread("./census data/religion_bystate.csv")
rel$state = cdlTools::fips(gsub(" ","", rel$state), to ="Abbreviation") #Name
rel$state = ifelse(is.na(rel$state), "DC",rel$state) ## DC is NA so place it back in.


census = merge(census,  edu,
               by = 'STATE',
               all.x = T, all.y = F)
census = merge(census,  ethwhite,
               by= 'STATE',
               all.x = T, all.y = F)
census = merge(census,  pop,
               by= 'STATE',
               all.x = T, all.y = F)
census = merge(census,  rel[,c("state","rel_perc")],
               by.x= 'STATE', by.y = 'state',
               all.x = T, all.y = F)




census = census %>% dplyr::relocate(N,.before = "STATE") %>%
  mutate(STATE = factor(STATE),
         SEX = factor(SEX),
         AGE = factor(AGE),
         PARTY = factor(PARTY))


### turn to factor and re-level to numerise the census
state_list = readRDS("./state_list.RDS") 
levels(census$STATE) = state_list$state 
levels(census$SEX) = c("female","male")
levels(census$PARTY) = c("Democrat","Neutral","Republican")

### standardize fixed effects
census[c("white_perc","undergrad_perc","pop")] <-  lapply(census[c("white_perc","undergrad_perc","pop")], 
                                                          function(x) c(scale(x)))

census

saveRDS(census, "./census data/census.rds")
