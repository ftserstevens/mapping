library(rstan)
library(dplyr)
library(parallel)
library(mltools)
library(ggplot2)
library(bayesplot)
library(ggridges)


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}



# compare Net & Balanced or Rep & Dem --------------------------------------------------

use2 = F


model_raw = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_raw_average.rds"))
model_bal = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_raw_average_1for1.rds"))
model_woc = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_model_woc_discestimates.rds"))
raw_dem = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_raw_average_onlydem.rds"))
raw_rep = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_raw_average_onlyrep.rds"))
model_dem = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_model_dem_discestimates.rds"))
model_rep = readRDS(paste0("./stan trained models step2/",if(use2) {"using2/"},"step2_model_rep_discestimates.rds"))


compare_parameter = c("zeta")
post_raw = as.data.frame(rstan::extract(model_raw, pars =compare_parameter))
post_bal = as.data.frame(rstan::extract(model_bal, pars =compare_parameter))
post_woc = as.data.frame(rstan::extract(model_woc, pars =compare_parameter))
postr_dem = as.data.frame(rstan::extract(raw_dem, pars = compare_parameter))
postr_rep = as.data.frame(rstan::extract(raw_rep, pars = compare_parameter))
postm_dem = as.data.frame(rstan::extract(model_dem, pars = compare_parameter))
postm_rep = as.data.frame(rstan::extract(model_rep, pars = compare_parameter))



post_raw$model = "Naive Sample"
post_bal$model = "Naive Balanced"
post_woc$model = "Model Population"
postr_dem$model = "Naive Democrat"
postr_rep$model = "Naive Republican"
postm_dem$model = "Model Democrat"
postm_rep$model = "Model Republican"



# Compare Partisan ----------------------------------------------------------------



combined_areas <- rbind(
  postr_dem, postr_rep,
  postm_dem, postm_rep
)


df_long <- combined_areas %>%
  tidyr::pivot_longer(cols = names(combined_areas)[1:(dim(combined_areas)[2]-1)], names_to = "name", values_to = "value")

limits = df_long %>% 
  dplyr::group_by(name, model) %>% 
  dplyr::summarise(quantile_h = quantile(value, c(0.975)),
                   quantile_l = quantile(value, c(0.025)))

df_long = merge(df_long, limits,
                by = c("name" ,"model"))
df_long = df_long[df_long$value >= df_long$quantile_l & df_long$value <= df_long$quantile_h,]


coef_compare = ggplot(df_long, aes(x = value, y = name, group = interaction(name, model), fill = interaction(model))) +
  geom_density_ridges(alpha = .5, scale = 1) + theme_minimal() +
  #scale_fill_manual(values = c("#08519c", "white", "darkgrey")) + 
  scale_fill_manual(values = c("#08519c", "red", "lightblue", "orange")) + 
  geom_vline(xintercept = 0, color = "red") +
  scale_y_discrete(labels = c("zeta.1" = "Democrat","zeta.2" = "Neutral","zeta.3" = "Republican")) + 
  xlab("logged odds") +ylab("") +
  theme(
    legend.position = "top",           # Move legend to the right
    legend.title = element_blank(),      # Remove legend title
    legend.text = element_text(size = 15),  # Make legend text bigger
    legend.key.size = unit(1.4, "lines"), # Make legend color swatches bigger
    axis.title.x = element_text(size = 15), # Make x-axis label bigger
    axis.text.y = element_text(size = 15) # Make y-axis labels bigger
  ) + guides(fill = guide_legend(nrow = 2))    # Arrange legend in 2 rows

coef_compare
ggsave(filename = paste0("./Scripts - MRPs/step2/plots_coefposterior/partisan_posterior",if(use2) {"_use2"},".pdf"), 
       plot = coef_compare, width = 6, height = 8, units = "in")



# compare WOC -------------------------------------------------------------



combined_areas <- rbind(
  post_bal,post_woc, post_raw
)


df_long <- combined_areas %>%
  tidyr::pivot_longer(cols = names(combined_areas)[1:(dim(combined_areas)[2]-1)], names_to = "name", values_to = "value")

limits = df_long %>% 
  dplyr::group_by(name, model) %>% 
  dplyr::summarise(quantile_h = quantile(value, c(0.975)),
                   quantile_l = quantile(value, c(0.025)))

df_long = merge(df_long, limits,
                by = c("name" ,"model"))
df_long = df_long[df_long$value >= df_long$quantile_l & df_long$value <= df_long$quantile_h,]


coef_compare = ggplot(df_long, aes(x = value, y = name, group = interaction(name, model), fill = interaction(model))) +
  geom_density_ridges(alpha = .5, scale = 1) + theme_minimal() +
  scale_fill_manual(values = c("#08519c", "white", "darkgrey")) + 
  geom_vline(xintercept = 0, color = "red") +
  scale_y_discrete(labels = c("zeta.1" = "Democrat","zeta.2" = "Neutral","zeta.3" = "Republican")) + 
  xlab("logged odds") +ylab("") +
  theme(
    legend.position = "top",           # Move legend to the right
    legend.title = element_blank(),      # Remove legend title
    legend.text = element_text(size = 15),  # Make legend text bigger
    legend.key.size = unit(1.4, "lines"), # Make legend color swatches bigger
    axis.title.x = element_text(size = 15), # Make x-axis label bigger
    axis.text.y = element_text(size = 15) # Make y-axis labels bigger
  ) + guides(fill = guide_legend(nrow = 2))    # Arrange legend in 2 rows

coef_compare
ggsave(filename = paste0("./Scripts - MRPs/step2/plots_coefposterior/woc_posterior",if(use2) {"_use2"},".pdf"),
       plot = coef_compare, width = 6, height = 8, units = "in")








