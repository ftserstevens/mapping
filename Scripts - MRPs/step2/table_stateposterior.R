library(rstan)
library(xtable)

accuracy ="raw_average_1for1"
use2 = T
lo = F ### always leave as false (logged odds instead of pi)


state_PS = readRDS(
  paste0("./Scripts - MRPs/step2/state_posterior/",
         if(lo) {"logged_odds/"} else("probability/"),
         if(use2) {"using2/"},
         accuracy,".rds"))

cenus = census =readRDS("./census data/census.rds")



# get the state tables in latex -------------------------------------------

### get the state distribution.
state_dist = as.data.frame(t(apply(t(state_PS), 2, function(x) quantile(x, probs = c(0.10, 0.50, 0.90)))))

state_n = census %>% dplyr::group_by(STATE) %>% dplyr::summarise(sum(N))
state_dist_amount = as.data.frame(apply(state_dist, 2, function(x) round(x * state_n[,2])))
names(state_dist_amount) = c("10%","50%","90%")
rownames(state_dist_amount) = rownames(state_dist)
state_dist_amount = state_dist_amount[order(state_dist_amount$`50%`,decreasing = T),]
state_dist_amount = state_dist_amount %>% mutate(
  `10%` = as.integer(`10%`),
  `50%` = as.integer(`50%`),
  `90%` = as.integer(`90%`)
)


state_dist = state_dist[order(state_dist$`50%`, decreasing = T),]



xtable(state_dist, label = paste0("statedist_",accuracy))
xtable(state_dist_amount[], label = paste0("statedist_shareramount_",accuracy))





