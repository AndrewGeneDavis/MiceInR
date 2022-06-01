library(tidyverse)

# data import and cleaning
filename <- "All mice 06012022_AD.csv"
mice <- read_csv(filename)
mice$Age_Months <- mice$Age/30
mice$Age_Weeks <- mice$Age/7
mice <- filter(mice, `Cage #` != "<Empty>")
mice_non_breeders <- filter(mice, 
                            is.na(`Breeding Start`))
mice_non_breeders <- filter(mice_non_breeders, 
                            !((`Age_Months` > 3) & is.na(`Genotype`)))
mouse_groups <- unique(mice_non_breeders$Group)
active_group <- mouse_groups[grep("ACA", mouse_groups)]
active_group <- mouse_groups[grep("ARH", mouse_groups)]
active_group <- mouse_groups[grep("AHRTER", mouse_groups)]

graph_group <- mice_non_breeders[which(mice_non_breeders$Group == active_group),]


# graph ages in months
# by sex and by genotype
ggplot(graph_group, aes(x=Age_Months, fill=Sex)) +
  labs(title = filename, subtitle = graph_group$Group[1], x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)

ggplot(graph_group, aes(x=Age_Months, fill=Genotype)) +
  labs(title = filename, subtitle = graph_group$Group[1], x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)


# graph ages in weeks
# by sex and by genotype
ggplot(graph_group, aes(x=Age_Weeks, fill=Sex)) +
  labs(title = filename, subtitle = paste(graph_group$Group[1], "MNV agnostic"), x = "Age (Weeks - 7 Days)", y = "Count") +
  scale_x_continuous(breaks = seq(0,max(graph_group$Age_Weeks),3)) +
  geom_histogram(binwidth = 0.3333)

ggplot(graph_group, aes(x=Age_Weeks, fill=Genotype)) +
  labs(title = filename, subtitle = paste(graph_group$Group[1], "MNV agnostic"), x = "Age (Weeks - 7 Days)", y = "Count") +
  scale_x_continuous(breaks = seq(0,max(graph_group$Age_Weeks),3)) +
  geom_histogram(binwidth = 0.3333)


