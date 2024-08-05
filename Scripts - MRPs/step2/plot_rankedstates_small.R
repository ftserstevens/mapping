rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}




# plot state distributions --------------------------------------------------
library(tidyr)
library(dplyr)



use2 = T

only_dem = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},"raw_average_onlydem.rds"))
only_rep = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},"raw_average_onlyrep.rds"))
for1 = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},"model_1for1.rds"))
woc = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},"model_woc_discestimates.rds"))
model_dem = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},"model_dem_discestimates.rds"))
model_rep = readRDS(paste0("./Scripts - MRPs/step2/state_intervals/probability/",if(use2) {"using2/"},"model_rep_discestimates.rds"))



only_dem$model = 'Naive Democrats'
only_dem$state = rownames(only_dem)
model_dem$model = 'Model Democrats'
model_dem$state = rownames(model_dem)
only_rep$model = 'Naive Republicans'
only_rep$state = rownames(only_rep)
model_rep$model = 'Model Republicans'
model_rep$state = rownames(model_rep)
for1$model = 'Model Balanced'
for1$state = rownames(for1)
woc$model = 'Model Population'
woc$state = rownames(woc)


combined_df <- rbind(woc, model_dem, model_rep, if(use2) {for1})
rownames(combined_df) = 1:nrow(combined_df)


# Reshape the dataframe to wide format
long_df <- combined_df %>%
  tidyr::pivot_longer(cols = c(`10%`, `50%`, `90%`), names_to = "percentile", values_to = "value") %>%
  tidyr::pivot_wider(names_from = "percentile", values_from = "value")

# Determine the order of states based on the 50% percentile values from Model 2
state_order <- long_df %>%
  filter(model == "Model Population") %>%
  arrange(desc(`50%`)) %>%
  pull(state)

# Update the state factor levels based on this order
long_df <- long_df %>%
  mutate(state = factor(state, levels = state_order))

# Define custom colors for each model
custom_colors <- c(
  "Model Democrats" = '#1f77b4',
  'Model Republicans' = '#d62728',
  'Model Population' = 'black',
  'Model Balanced' = 'orange'
    
  
)

# Plot using ggplot2
state_distplot =  ggplot(long_df, aes(x = state, color = model, group = model)) +
  geom_linerange(aes(ymin = `10%`, ymax = `90%`), position = position_dodge(width = 0.8)) +
  geom_point(aes(y = `50%`), position = position_dodge(width = 0.8)) +
  scale_color_manual(values = custom_colors) +
  coord_flip() +
  theme_minimal() +
  labs(x = "State",
       y = "Sharing Likelihood",
       color = "Model") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 12),
    legend.box = "horizontal"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(state_distplot, filename = paste0("./Scripts - MRPs/step2/plots_rankedstates/state_distplot(model)",if(use2) {"use_2"},".pdf"), units = "in",
       height = 14, width = 8)


