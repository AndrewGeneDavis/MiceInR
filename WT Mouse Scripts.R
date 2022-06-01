#Scripting for WT mice from a pivot table from the various wt lines that are in our lab tracks system
#Author: Andrew Davis
#09/22/2020
library(tidyverse)
library(RColorBrewer)
if(!require(car)){install.packages("car")};library(car)

All_Mice <- read_csv("Mouse Colonies 05172022_AD.csv")
All_Mice <- All_Mice[which(All_Mice$`Cage #` != "<Empty>"),]
All_Mice$Age_Weeks <- All_Mice$Age/7
All_Mice$Age_Months <- All_Mice$Age/30
All_Mice$Age_Years <- All_Mice$Age/365
All_Mice <- mutate(.data = All_Mice, cage_unique = paste(Group,`Cage #`))
All_Mice$Comments <- gsub("\n", "", All_Mice$Comments)
All_Mice <- All_Mice[-which((All_Mice$Comments == "cull")),]

#View(All_Mice)
mouse_groups <- unique(All_Mice$Group)
lines <- unique(All_Mice$Group)
B4_lines <- lines[grep("Adams", lines)]
experimental_lines <- B4_lines[which(B4_lines != "Adams : 4133- WT:C57BL/6 (21-011)  (S-522)")]


### Mouse Numbers
WT_agnog_Mice <- All_Mice[which(All_Mice$Group == "WT (S-5)"|All_Mice$Group == "Adams : 4133- WT:C57BL/6 (21-011)  (S-522)"),]
Cas9_agnog_Mice <- All_Mice[which(All_Mice$Group == "rCAS (S-18)"),]
WT_MNV_free_Mice <- All_Mice[which(All_Mice$Group == "WT (MNV-Free Room 6107)  (S-24)"),]
Cas9_MNV_free_Mice <- All_Mice[which(All_Mice$Group == "CAS9 (MNV-Free Room 6107)  (S- 23)"),]
PTEN_Mice <- All_Mice[which(All_Mice$Group == "Pten (MNV-Free Room 6107) (S-25)"),]
STAT1_Mice <- All_Mice[which(All_Mice$Group == "STAT1 (S-21)"),]
Experimental_Mice <- All_Mice[which(All_Mice$Group == experimental_lines[1]),]
for(i in 2:length(experimental_lines)){
  Experimental_Mice <- rbind(Experimental_Mice,All_Mice[which(All_Mice$Group == experimental_lines[i]),])
}

Experimental_Mice[which(Experimental_Mice$Sex=="Female"),"Group"]


### Cages
cagenumbers <- c(
  length(unique(WT_agnog_Mice$cage_unique)),
  length(unique(Cas9_agnog_Mice$cage_unique)),
  length(unique(WT_MNV_free_Mice$cage_unique)),
  length(unique(Cas9_MNV_free_Mice$cage_unique)),
  length(unique(PTEN_Mice$cage_unique)),
  length(unique(STAT1_Mice$cage_unique)),
  length(unique(Experimental_Mice$cage_unique))
)
sum(cagenumbers)

Cull_list <- All_Mice[which(All_Mice$Age_Months >= 22),]
length(Cull_list$`Pedigree #`)

#Mouse Numbers
#22 mo and over in mnv-agnostic line
ggplot(Cull_list, aes(x = Age_Months, fill = Sex)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste("22 mo and over.", Cull_list$Group[1]), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#WT (S-5)
ggplot(WT_agnog_Mice, aes(x=Age_Months, fill=Comments, col = Sex)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(WT_agnog_Mice$Group[1], "MNV agnostic"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#rCAS (MNV-agnostic)
ggplot(Cas9_agnog_Mice, aes(x=Age_Months, fill=Comments, col = Sex)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(Cas9_agnog_Mice$Group[1], "MNV agnostic"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#WT MNV-free
ggplot(WT_MNV_free_Mice, aes(x=Age_Months, fill=Comments, col = Sex)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(WT_MNV_free_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
# ggsave(paste0("WT_MNV_free_Mice", mouse_groups[15], ".png"))
#CAS9 MNV-free
ggplot(Cas9_MNV_free_Mice, aes(x=Age_Months, fill=Comments, col = Sex)) +
  labs(title = "P01 (Adams) Lab Aging Colony", subtitle = paste(Cas9_MNV_free_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#PTEN
ggplot(PTEN_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "PTEN Colony", subtitle = paste(PTEN_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#STAT1
ggplot(STAT1_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "STAT1 Colony", subtitle = paste(STAT1_Mice$Group[1], "MNV agnostic"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#all others in experimental facility
ggplot(Experimental_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "Experimental Mice", subtitle = paste("Various line names"), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333) +
  scale_x_continuous(limits = c(0,max(Experimental_Mice$Age_Months)+2))


# Cage numbers
All_Mice %>% 
  mutate(line_cage = paste(Group, "cage", `Cage #`)) %>% 
  group_by(line_cage) %>% 
  tally() %>% 
  tally()

#ACA mice graph update

mouse_data_filename <- "ACA 05052022_AD.csv"
ACA_Mice <- read_csv(mouse_data_filename)
ACA_Mice_time <- as.Date(file.info(mouse_data_filename)[1,"mtime"])
ACA_Mice$Age_Months <- ACA_Mice$Age/30
ACA_Mice$is_breeder <- !is.na(ACA_Mice$`Breeding Start`)
#ACA_Mice <- ACA_Mice[which(!is.na(ACA_Mice$`Breeding Start`)),]
ACA_Mice <- ACA_Mice[which((ACA_Mice$Age_Months < 3 | !is.na(ACA_Mice$Genotype) & is.na(ACA_Mice$`Breeding Start`))),]


ggplot(ACA_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "ACA Colony", subtitle = paste(ACA_Mice$Group[1],", ",ACA_Mice_time), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)

ggplot(ACA_Mice, aes(x=Age_Months, fill=Genotype, col = Sex)) +
  labs(title = "ACA Colony", subtitle = paste(ACA_Mice$Group[1],", ",ACA_Mice_time), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)

ggplot(ACA_Mice, aes(x=Age_Months, fill=Comments, col = Genotype)) +
  labs(title = "ACA Colony", subtitle = paste(ACA_Mice$Group[1],", ",ACA_Mice_time), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)

#General mice graph update

mouse_data_filename <- "TP53BP1 05102022_AD.csv"
Mouse_Line <- read_csv(mouse_data_filename)
Mouse_Line_time <- as.Date(file.info(mouse_data_filename)[1,"mtime"])
Mouse_Line$Age_Months <- Mouse_Line$Age/30
Mouse_Line$is_breeder <- !is.na(Mouse_Line$`Breeding Start`)
Mouse_Line <- Mouse_Line[which((Mouse_Line$Age_Months < 3 | !is.na(Mouse_Line$Genotype) & is.na(Mouse_Line$`Breeding Start`))),]

ggplot(Mouse_Line, aes(x=Age_Months, fill=Sex)) +
  labs(title = Mouse_Line$Group[1], subtitle = paste(Mouse_Line$Group[1],", ",Mouse_Line_time), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)

ggplot(Mouse_Line, aes(x=Age_Months, fill=Genotype, alpha = !is_breeder, col = Sex)) +
  labs(title = Mouse_Line$Group[1], subtitle = paste(Mouse_Line$Group[1],", ",Mouse_Line_time), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)

ggplot(Mouse_Line, aes(x=Age_Months, fill=Comments, col = Genotype)) +
  labs(title = "ARH Colony", subtitle = paste(Mouse_Line$Group[1],", ",Mouse_Line_time), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)


#Looking at litters as of 06/03/2021
#Data was pulled from falcon web server: Wt 'litters'
#It looks like they didn't record any litters over 10, although the graph in their system clearly shows some over 10.
#After some work (below), it appears that Litter number and weaned number refer to the total number of litters a mating had,
# not the number of mice in the litter.  Which, to me, seems very very stupid.
#I will repull the data from labtracks instead of falcon

wt_litters <- read_csv("breedingVsAgeGrid.csv")
str(wt_litters$`Birth Date`)
wt_litters$`Birth Date` <- as.POSIXct(strptime(x = wt_litters$`Birth Date`, "%m/%d/%y"))
wt_litters$`Pup #` <- Litter_count
wt_litters$`Mating Name`
aggregate(wt_litters$`Pup #`,by = list(wt_litters$`Mating Name`), FUN='mean')
mean(wt_litters$`Pup #`)
#view(wt_litters)

wt_litters.lm <- with(wt_litters, lm(`Pup #` ~ `Dam Mating Age` + `Birth Date` + `Sire Mating Age` + `Mating Litter #`))

Anova(wt_litters.lm)

# We will instead make a simple projection based on 8 mice per litter
# We need a couple of numbers for this projection if we want to be accurate
# We need to know how many mice per litter we can expect and how many litters we can expect per mating
# We also need to know how often litters happen
# From below, our average mice per litter is 7, and our average litters per breeding is 10
# The above is wrong; it's actually the number of litters a mating had.  The column isn't clear and it doesn't have a helpful description anywhere.

# Average mice per litter over the litters in a breeding pair
litter_means <- aggregate(x = wt_litters$`Pup #`, FUN = "mean", by = list(wt_litters$`Mating Litter #`))
names(litter_means) <- c("Litter Number", "Avg Litter Size")
litter_means

ggplot(litter_means, aes(x = `Litter Number`, y = `Avg Litter Size`)) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(1,10), breaks = seq(1,10)) +
  scale_y_continuous(limits = c(1,10), breaks = seq(1,10)) +
  labs(title = "Number of Pups vs litter index", subtitle = "Averages among matings")

#This appears to be unhelpful, as I cannot distinguish individual matings
# ggplot(wt_litters, aes(x = `Mating Litter #`, y = `Pup #`, group = `Mating Name`)) +
#   geom_line(linetype = 3) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   scale_x_continuous(limits = c(1,10), breaks = seq(1,10)) +
#   scale_y_continuous(limits = c(1,10), breaks = seq(1,10)) +
#   labs(title = "Litters size vs litter number", subtitle = "the more litters a breeding pair had, the more mice per litter") +
#   guides(col = FALSE)

#according to our litters, we get an average of 6.3 mice per litter across 10 litters
mean(litter_means$`Avg Litter Size`)

ggplot(wt_litters, aes(x = `Birth Date`, y = `Pup #`, col = as.factor(`Mating Name`), group = `Mating Name`)) +
  scale_y_continuous(breaks = seq(1,13)) +
  geom_point() +
  geom_line() +
  guides(col = FALSE) +
  labs(title = "Litters in the WT colony")

#I want to take each row and compare it to the next
#If they have the same mating name, I want to store the difference in time in a new vector
#I also want to keep a record of the mating name vectors
#The data frame needs to be ordered by mating name and then by birth dates
# According to the below, we have a litter approximately every 22 days (around .6 months)

head(wt_litters)
wt_litters <- wt_litters[order(wt_litters$`Mating Name`,wt_litters$`Birth Date`),]

mating_names <- c(000000)
mating_distances <- c(wt_litters$`Birth Date`[2] - wt_litters$`Birth Date`[1])
first_birth_date <- c(wt_litters$`Birth Date`[1])
for(i in 1:(length(wt_litters$`Mating Name`)-1)){
  if(wt_litters$`Mating Name`[i] == wt_litters$`Mating Name`[i+1]){
    mating_names <- c(mating_names, wt_litters$`Mating Name`[i])
    mating_distances <- c(mating_distances, wt_litters$`Birth Date`[i+1]-wt_litters$`Birth Date`[i])
    first_birth_date <- c(first_birth_date, wt_litters$`Birth Date`[i])
  }
}
units(mating_distances) <- "days"
litter_distances <- tibble(mating_names, mating_distances, first_birth_date)
c(min(mating_distances), max(mating_distances))
mean(mating_distances)
median(mating_distances)
#view(litter_distances)

ggplot(litter_distances, aes(y = mating_distances)) +
  geom_histogram(binwidth = 3) +
  scale_y_continuous(breaks = ((seq(1:15)-1)*10)) +
  labs(title = "Distribution of time between litters",y = "Days") +
  coord_flip()

# Number of litters per breeding
litters_per_breeding <- aggregate(wt_litters$`Mating Litter #`, list(wt_litters$`Mating Name`), FUN = "max")
names(litters_per_breeding) <- c("Mating Litter #", "# of Litters")
ggplot(litters_per_breeding, aes(y = `# of Litters`)) +
  geom_density() +
  scale_y_continuous(limits = c(1,10), breaks = seq(1:10)) +
  coord_flip()
mean(litters_per_breeding$`# of Litters`)

# So, finally, we have a few useful pieces of information to build our new colony from:
# Average Mice in a litter
mean(wt_litters$`Pup #`)
mean(wt_litters$`Pup Male #`)
mean(wt_litters$`Pup Female #`)
# Average litters per mating

# Average time between litters
mean(mating_distances)

# Cage counts 5/2/2022
B6 <- c(43, 29, 27, 20, 16, 4, 2)
B4 <- c(23, 8, 7, 7, 6, 5, 4, 2, 2, 1)
sum(B4)
sum(B6, B4)
