#Convert to het/hom
if(!require("tidyverse")){install.packages("tidyverse")};library(tidyverse)
if(!require("ggbeeswarm")){install.packages("ggbeeswarm")};library(ggbeeswarm)

TX_data <- data.frame(mouse_ID = c("1426", "1427", "1428", "1429", "1441", "1442", "1443", "1444", "1446", "1447", "1448", "1449", "1450"), data=c(5.811,7.04355,4.6004,4.146,7.1132,8.8459,7.78205,3.88015,5.5749,5.09865,2.85395,3.62735,3.17255),sex=c("Female","Female","Male","Male","Female","Female","Female","Male","Female","Female","Male","Male","Male"))
TX_data$source = "Transnetyx"
TX_data$relative_value <- qPCR_data$data/mean(qPCR_data[which(qPCR_data$sex == "Male"),]$data)

AD_data <- data.frame(mouse_ID = c("1250", "1250", "1250", "1251", "1251", "1251", "1252", "1252", "1252", "1253", "1253", "1253", "1254", "1254", "1254", "1255", "1255", "1255", "1256", "1256", "1256", "1257", "1257", "1257"), relative_value = c(2.5167924309276E-05, 2.5167924309276E-05, 2.5167924309276E-05, 0.950483818230157, 0.996897290028675, 0.98849393804432, 0.863735207600763, 1.37848197821634, 0.896700493742618, 2.70618699199473E-05, 2.70618699199473E-05, 0.000127379726659827, 1.66768118748916, 1.79950246171424, 1.74794208781466, 1.6725056334391, 1.71856590364469, 1.80175923800455, 2.54059799747182, 1.88638935148043, 1.95242636007278, 3.47769858000451, 1.87741014237485, 1.87370558379284), sex = c("Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female"))
AD_data$source = "AD"
mean(AD_data[which(AD_data$relative_value > 0.01 & AD_data$sex == "Male"),]$relative_value)
all_data <- aggregate(x = AD_data, relative_value ~ mouse_ID, FUN = mean)
all_data$sex <- aggregate(x = AD_data, sex ~ mouse_ID, FUN = unique)$sex
all_data$source <- aggregate(x = AD_data, source ~ mouse_ID, FUN = unique)$source

all_data <- rbind(all_data, TX_data[,c("mouse_ID", "relative_value", "sex", "source")])

ggplot(all_data, aes(x = sex, y = relative_value, fill = source, col = source)) +
  geom_beeswarm() +
  labs(title = "qPCR results for ACA")

names(all_data)
names(TX_data)
