#Navitoclax mouse weights
#11/3/2022
#Andrew Davis

#Navitoclax will be administered at 50mg/kg/day for two 7-day periods
#Mouse weights at the start of the experiment are imported
#Animal facility requests an average weight to estimate so that a single
# dose size can be used for all mice.  I want to see how this changes the
# effective dosages for each mouse

library(tidyverse)
library(ggbeeswarm)

mouse_data <- read_csv("navitoclax expt 11082022.csv")
mouse_data <- mouse_data[which(!is.na(mouse_data$`#`)),]
mouse_data$weight <- as.numeric(mouse_data$weight)
names(mouse_data)
aggregate(weight ~ Comments, data = mouse_data, FUN = "mean")
aggregate(weight ~ Comments, data = mouse_data, FUN = "median")
aggregate(weight ~ Comments, data = mouse_data, FUN = "sd")

mouse_graph_data <- mouse_data
mouse_graph_data$Comments <- gsub(pattern = "Aim 1 ", replacement = "Aim 1\n",x = mouse_data$Comments)
ggplot(mouse_graph_data, aes(y = weight, x = Comments, fill = Comments)) +
  geom_boxplot(alpha = 0.5) +
  geom_violin(alpha = 0.5) +
  geom_beeswarm(aes(col = Sex))
  
