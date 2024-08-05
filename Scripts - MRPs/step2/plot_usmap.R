library(ipumsr)
library(terra)
library(scales)
library(dplyr)
library(usmap)
library(usmapdata)
library(mltools)
library(maps)
library(ggplot2)
library(ggthemes)
library(ggridges)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}




accuracy = "raw_average_onlyrep"
use2 = T
lo = F # no sense in making this true anymore.

state_PS.median = readRDS(paste0('./Scripts - MRPs/step2/state_intervals/',
                                 if(lo){"logged_odds/"} else("probability/"),
                                 if(use2) {"using2/"},
                                 accuracy,'.rds'))
state_PS.median$state = rownames(state_PS.median)
state_PS.median$median = state_PS.median$`50%`
state_PS.median$med_order = rank(state_PS.median$`50%`)
state_PS.median = state_PS.median[,c('state','median','med_order')]
 
quantile(state_PS.median$median, probs = c(.1,.9))



df = readRDS(paste0("./stan trained models step2/",
                    if(use2) {"using2/"},
                    "df/step2df_",accuracy,".rds"))


# plot predictions --------------------------------------------------------

us_map = usmap::us_map()
us_map$state = us_map$abbr

df_coord = as.data.frame(cbind(df$lon, df$lat))
names(df_coord) = c("lon",'lat')
df_coord = usmap::usmap_transform(df_coord)
merged_data <- dplyr::left_join(us_map, state_PS.median, by = "state")




# ranked_states -----------------------------------------------------------

# Plot the state polygons
map_us = ggplot(merged_data, aes(x = x, y = y)) + 
  geom_polygon(aes(fill = med_order, group = group)) +
  scale_fill_gradientn(colors = colorRampPalette(c("white", "#08306b"))(51), na.value = "grey50") + guides(fill="none") +
  geom_path(data = merged_data, aes(x = x, y = y, group = group), color = "black", size = 0.3) +
  ggthemes::theme_map() 
ggsave(plot = map_us,
  filename =paste0('./Scripts - MRPs/step2/plots_usmap/rank/',if(use2) {"using2/"},'USAmap_',accuracy,if(use2) {"_use2"},if(lo) {"_lo"},'.pdf'),
  width = 10, height = 8, units = 'in')



# continuous scale -----------------------------------------------------------

color_limits = quantile(state_PS.median$median, probs = c(.05,.95))
breaks <- c(quantile(state_PS.median$median, probs = c(.15)), 
            (color_limits[1]+ color_limits[2])/2, 
            quantile(state_PS.median$median, probs = c(.85)))
labels <- round(breaks, 2)

map_us = ggplot(merged_data, aes(x = x, y = y)) + 
  geom_polygon(aes(fill = squish(median, range = color_limits), group = group)) +
  scale_fill_gradientn(colors = c("white", "#08306b"), 
                       limits = color_limits, 
                       breaks = breaks, 
                       labels = labels, 
                       na.value = "grey50", 
                       name = "") +
  geom_path(data = merged_data, aes(x = x, y = y, group = group), color = "black", size = 0.3) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(4.85, "cm"),         # Adjust the width of the legend keys
    legend.key.height = unit(0.5, "cm"),# Adjust the height of the legend keys
    legend.box.just = "center",               # Center the legend box
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust the margin around the legend
    legend.text = element_text(size = 20)        # Make the labels on the legend bigger
    
    
    
  )
map_us


ggsave(plot = map_us,
       filename =paste0('./Scripts - MRPs/step2/plots_usmap/percentage/',
                        if(use2) {"using2/"},
                        'USAmap_',accuracy,if(use2) {"_use2"},
                        if(lo) {"_lo"},'.pdf'),
       width = 10, height = 8, units = 'in')

