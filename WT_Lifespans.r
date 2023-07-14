# Andrew Davis
# 12/20/2021
# WT mouse longevity, taken from data from the NIA
# https://www.nia.nih.gov/research/dab/aged-rodent-colonies-handbook/strain-survival-information
library(tidyverse)

Survival = rep(c(1, .9, .75, .5, .25, .1), 2)
BN_Males = c(0, 22, 27, 32, 34, 36)
BN_Females = c(0, 22, 27, 32, 35, 38)
B6_Males = c(0, 19, 24, 27, 30, 32)
B6_Females = c(0, 18, 22, 25, 28, 30)
Months = c(BN_Males, BN_Females, B6_Males, B6_Females)
Sex = c(rep("Male_BN", length(BN_Males)), rep("Female_BN", length(BN_Females)), rep("Male_B6", length(B6_Males)), rep("Female_B6", length(B6_Females)))

data <- data.frame(Survival, Months, Sex)


ggplot(data, aes(x = Months, y = Survival, group = Sex, col = Sex)) +
  geom_line(size = 1.5) +
  geom_point(col = 'Black') +
  labs(title = "Lifespan of C57bl/6") +
  scale_y_continuous(breaks = c(0,.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),limits = c(0,1))
