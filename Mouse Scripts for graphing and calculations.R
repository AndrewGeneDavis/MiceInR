#Scripting for mice data from lab tracks system exported as a csv
#Author: Andrew Davis
#Started: 09/22/2020
#Last updated: 11/14/2022
if(!require("tidyverse")){install.packages("tidyverse")};library(tidyverse)
if(!require("ggbeeswarm")){install.packages("ggbeeswarm")};library(ggbeeswarm)
if(!require("RColorBrewer")){install.packages("RColorBrewer")};library(RColorBrewer)
if(!require(car)){install.packages("car")};library(car)
if(!require(ggpattern)){install.packages("ggpattern")};library(ggpattern)

#import data
All_Mice <- read_csv("All Mice 12142022_AD.csv")
import_date <- Sys.Date()


#some mice in the inventory have not been made invisible, but instead are listed in cages marked "<empty>".
#remove these non-existant mice
All_Mice <- All_Mice[which(All_Mice$`Cage #` != "<Empty>"),]

#make columns for various age conversions and remove newlines from comment data
All_Mice$Age_Weeks <- All_Mice$Age/7
All_Mice$Age_Months <- All_Mice$Age/30.41
All_Mice$Age_Years <- All_Mice$Age/365
All_Mice$Comments <- gsub("\n", "", All_Mice$Comments)
All_Mice$Group <- All_Mice$`Group Name`
All_Mice$Comments[which(is.na(All_Mice$Comments))] <- "Empty"
All_Mice$Comments[which(All_Mice$Comments == "cull1")] <- "cull"

#make unique cage numbers among groups so that we can calculate the number of cages in each group
Groups <- unique(All_Mice$Group)
All_Mice <- mutate(.data = All_Mice, cage_unique = paste(Group,`Cage #`))
Cages <- aggregate(cage_unique ~ Group, All_Mice, FUN=function(x) length(unique(x)))
Cages <- Cages[order(Cages$cage_unique, decreasing = T),]
ggplot(Cages, aes(x = Group, y = cage_unique)) +
  labs(title = "Cages in colonies", x = "Line", y = "Cage Count") +
  geom_bar(stat = 'identity')

#make separate data frame for P01 mice based on custom list
P01_liver_lines <- c("Pten (MNV-Free Room 6107) (S-25)", "TFAM - Breeding - Quar", "TFAM - Exp - Quar", "FAH KO - Quar (S-33)", "eGFP (S-43)", "STAT1 (S-21)", "FAH KO (S-44)", "rCAS (S-18)", "WT (MNV-Free Room 6107)  (S-24)", "CAS9 (MNV-Free Room 6107)  (S- 23)", "Adams/Feng: C57BL/6JN-COHORT- Lee-22-002", "Adams:(B-4) 8/23/22 CR Aaron C57BL/6", "Adams: C57BL/6JN-CR(Gout) AUF 21-008 (S-423) ( Young Ctrl)", "Adams :( B-4) 4133- WT:C57BL/6 (21-011)  (S-522)", "Adams: C57BL/6JN - CR - 19-100  (S-178) (S-178)", "Adams: (B-4) 8/23/22 CR Aaron C57BL/6JN NIA(free)", "Adams: C57BL/6 - NIA  (S-558)", "Adams:WT (22-034)")
P01_Mice <- All_Mice[All_Mice$Group %in% P01_liver_lines,]
P01_Mice_without_cull <- P01_Mice[which(P01_Mice$Comments!="cull"),]
P01_Cages <- aggregate(cage_unique ~ Group, P01_Mice, FUN=function(x) length(unique(x)))
P01_Cages <- P01_Cages[order(P01_Cages$cage_unique, decreasing = T),]
P01_Cages_without_cull <- aggregate(cage_unique ~ Group, P01_Mice_without_cull, FUN=function(x) length(unique(x)))
P01_Cages_without_cull <- P01_Cages_without_cull[order(P01_Cages_without_cull$cage_unique, decreasing = T),]

sum(P01_Cages$cage_unique)
sum(P01_Cages_without_cull$cage_unique)

ggplot(P01_Cages, aes(x = Group, y = cage_unique)) +
  labs(title = "Cages in colonies", x = "Line", y = "Cage Count") +
  geom_bar(stat = 'identity')



#View(All_Mice)
mouse_groups <- unique(All_Mice$Group)
B4_lines <- Groups[grep("Adams", Groups)]
experimental_lines <- B4_lines[which(B4_lines != "Adams : 4133- WT:C57BL/6 (21-011)  (S-522)")]

### Mouse Numbers
WT_agnog_Mice <- All_Mice[which(All_Mice$Group == "WT (S-5)"|All_Mice$Group == "Adams : 4133- WT:C57BL/6 (21-011)  (S-522)"),]
Cas9_agnog_Mice <- All_Mice[which(All_Mice$Group == "rCAS (S-18)"),]
WT_MNV_free_Mice <- All_Mice[which(All_Mice$Group == "WT (MNV-Free Room 6107)  (S-24)"),]
Cas9_MNV_free_Mice <- All_Mice[which(All_Mice$Group == "CAS9 (MNV-Free Room 6107)  (S- 23)"),]
PTEN_Mice <- All_Mice[which(All_Mice$Group == "Pten (MNV-Free Room 6107) (S-25)"),]
STAT1_Mice <- All_Mice[which(All_Mice$Group == "STAT1 (S-21)"),]
HET3_Mice <- All_Mice[which(All_Mice$Group == "HET3 (MNV-Free) (S-32)"),]
ACA_Mice <- All_Mice[which(All_Mice$Group == "ACA (S-19)"),]
TP53BP1_Mice <- All_Mice[which(All_Mice$Group == "TP53BP1 (MNV-Free Room 6107) (S-28)"),]
Experimental_Mice <- All_Mice[which(All_Mice$Group == experimental_lines[1]),]
for(i in 2:length(experimental_lines)){
  Experimental_Mice <- rbind(Experimental_Mice,All_Mice[which(All_Mice$Group == experimental_lines[i]),])
}
Experimental_Mice[which(Experimental_Mice$Sex=="Female"),"Group"]

#Graphs of Mice
#WT (S-5)
ggplot(WT_agnog_Mice, aes(x=Age_Months, fill=Comments, col = Sex)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(WT_agnog_Mice$Group[1], "MNV agnostic"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#rCAS (MNV-agnostic)
ggplot(Cas9_agnog_Mice, aes(x=Age_Months, fill=Comments, col = Sex)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(Cas9_agnog_Mice$Group[1], "MNV agnostic"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#WT MNV-free
ggplot(WT_MNV_free_Mice, aes(x=Age_Months, fill=Sex, col = Comments)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(WT_MNV_free_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#WT MNV-free minus cull mice
ggplot(WT_MNV_free_Mice[which(WT_MNV_free_Mice$Comments!="cull"),], aes(x=Age_Months, fill=Sex, col = Comments)) +
  labs(title = "P01 (Adams) Aging Colony", subtitle = paste(WT_MNV_free_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
# ggsave(paste0("WT_MNV_free_Mice", mouse_groups[15], ".png"))
#CAS9 MNV-free
ggplot(Cas9_MNV_free_Mice, aes(x=Age_Months, fill=Sex, col = Comments)) +
  labs(title = "P01 (Adams) Lab Aging Colony", subtitle = paste(Cas9_MNV_free_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#PTEN
ggplot(PTEN_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "PTEN Colony", subtitle = paste(PTEN_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#STAT1
ggplot(STAT1_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "STAT1 Colony", subtitle = paste(STAT1_Mice$Group[1], "MNV agnostic"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#all others in experimental facility
ggplot(Experimental_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "Experimental Mice", subtitle = paste("Various line names"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333) +
  scale_x_continuous(limits = c(0,max(Experimental_Mice$Age_Months)+2))
#HET3
ggplot(HET3_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "HET3 Colony", subtitle = paste(HET3_Mice$Group[1], "MNV-free"), x = "Age (Months - 30 Days)", y = "# of mice") +
  geom_histogram(binwidth = 0.3333)
#ACA
ggplot(ACA_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "ACA Colony", subtitle = paste(ACA_Mice$Group[1],", ",import_date), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)
#TP53BP1
ggplot(TP53BP1_Mice, aes(x=Age_Months, fill=Sex)) +
  labs(title = "TP53BP1 Colony", subtitle = paste(TP53BP1_Mice$Group[1],", ",import_date), x = "Age (Months - 30 Days)", y = "Count") +
  geom_histogram(binwidth = 0.3333)


# Cage numbers
All_Mice %>% 
  mutate(line_cage = paste(Group, "cage", `Cage #`)) %>% 
  group_by(line_cage) %>% 
  tally() %>% 
  tally()


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

# Get litter distribution of HET3 mice
HET3_Mice$parents <- mutate(.data = HET3_Mice, parents = paste(`Dam #`, `Sire #`))$parents
HET3_Mice$litter <- mutate(.data = HET3_Mice, litter = paste(parents, `Birth Date`))$litter
HET3_Mice$count = 1

#get the number of mice in each litter and the sex breakdown
#filter out the original parents and unsexed mice
HET3_Mice_clean <- HET3_Mice[which(!is.na(HET3_Mice$`Dam #`) & !is.na(HET3_Mice$Sex)),]
litter_counts <- aggregate(count ~ `Dam #` + `Sire #` + `Birth Date`, HET3_Mice_clean, FUN = function(x) length(x))
litter_counts$male <- aggregate(count ~ `Dam #` + `Sire #` + `Birth Date`, HET3_Mice_clean[which(HET3_Mice_clean$Sex == "Male"),], FUN = function(x) length(x))$count
litter_counts <- mutate(litter_counts, female = count-male)
litter_counts <- mutate(litter_counts, `% female` = round(1-male/count, digits = 2)*100)

#aggregate(count ~ `Dam #` + `Sire #` + `Birth Date` + Sex, HET3_Mice_clean, FUN = function(x) length(x))

write_csv(litter_counts, "UMHET3_Litter_counts.csv")

HET3_Mice_clean[which(HET3_Mice_clean$`Birth Date` == "10/26/2022"),]

NIAOrders <- read_tsv("NIA Orders.tsv")
names(NIAOrders)
NIAOrders <- mutate(NIAOrders, Request = paste0(NIAOrders$User, " ", NIAOrders$AUF, "\n", NIAOrders$Project))
ggplot(NIAOrders, aes(x = Request, y = `# of mice requested`, fill = Sex)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "NIA mouse requests", subtitle = "March 2023; we can order 80 mice total")
