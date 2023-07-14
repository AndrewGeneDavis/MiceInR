# Test export with 'locations' feature

if(!require("tidyverse")){install.packages("tidyverse")};library(tidyverse)
if(!require("readxl")){install.packages("readxl")};library(readxl)

# Save csv files as UTF-8 to avoid an import issue with em-dashes
# or subsitute em dashes with the commented line below this one
# b6_mice_raw$`Group Name` <- gsub("\x96", "", b6_mice_raw$`Group Name`)

#List of constants
{
  days_in_a_week <- 7
  days_in_a_year <- 365
  months_in_a_year <- 12
  days_in_a_month <- ((days_in_a_year*4)+1)/(months_in_a_year*4)                #days in a month (also accounting for leap years)
  widescreen <- c(13.33, 7.5)                                                    #Width by height in inches for 'powerpoint widescreen' ratio
  save_to_file <- TRUE
  #save_to_file <- TRUE
  #import_date <- "2023-04-24"
}

#Data input
{
  database_input_files <- c("B4mice_07122023_AD.xlsx", "B6mice_07122023_AD.xlsx")
  b4_mice_raw <- read_xlsx(database_input_files[1])
  import_date_b4 <- substring(file.info(database_input_files[1])[[4]], 1, 10)
  b6_mice_raw <- read_xlsx(database_input_files[2])
  import_date_b6 <- substring(file.info(database_input_files[2])[[4]], 1, 10)
  if(import_date_b4 == import_date_b6){
    import_date <- import_date_b4
  }else{
    print("Warning: input files' last-modified dates do not match, using current time as input time.")
    import_date <- Sys.Date()
  }
  
}

# short script for getting deaths of mice within a time period for AUF annual review animal numbers
{
  protocol_name <- "21-001"
  start_date <- as.Date("1/22/2022", format = "%m/%d/%Y")
  end_date <- as.Date("1/22/2023", format = "%m/%d/%Y")
  
  All_Mice <- rbind(b4_mice_raw, b6_mice_raw)                                   
  All_Mice$`Birth Date` <- as.Date(All_Mice$`Birth Date`,format = "%m/%d/%Y")   
  All_Mice$`Death Date` <- as.Date(All_Mice$`Death Date`,format = "%m/%d/%Y")
  protocol_mice <- All_Mice[which(All_Mice$`Protocol #` == protocol_name),]
  length(protocol_mice$`Protocol #`)
  plot(protocol_mice$`Birth Date`)
  plot(protocol_mice$`Death Date`)
  length(which(protocol_mice$`Death Date` >= start_date & protocol_mice$`Death Date` <= end_date))
  
}

#Script to remove dead mice (cage == "<Empty>") and combine mouse data from B4 and B6
{
  # For a quick look at cage numbers to see any anomolous values, use the 2 lines below this one
  # unique(b4_mice_raw$`Cage #`)
  # unique(b6_mice_raw$`Cage #`)
  
  # Remove dead mice and mice from other research groups
  # Currently, our one collaborator is on a line called "Adams/Feng", but I may need to watch out for an independently named lines in the future
  # It is not a guarantee that our B4 lines will all say Adams in them, one of Jin's may already say Feng instead of Adams
  b4_mice <- b4_mice_raw[which(b4_mice_raw$`Cage #` != "<Empty>"),]
  b4_mice <- b4_mice[grep("Adams", b4_mice$`Group Name`),]
  b6_mice <- b6_mice_raw[which(b6_mice_raw$`Cage #` != "<Empty>"),]
  
  # Make sure that the columns match names before attempting to combine B4 and B6
  if(!all(names(b4_mice) == names(b6_mice))){
    stop("B4 and B6 Column names do not match")
  }
  
  print("List of line names")
  print(unique(b4_mice$`Group Name`))

  #Make unique names for mouse cages, as cage numbers are duplicated across lines
  #e.g. each line has a cage #1, #2, etc.
  #make separate data frames for cage counting and not individual mouse numbers
  b4_mutate <- mutate(.data = b4_mice, unique_cage = paste(`Group Name`, `Cage #`))
  b6_mutate <- mutate(.data = b6_mice, unique_cage = paste(`Group Name`, `Cage #`))
  b4_cages <- aggregate(unique_cage ~ `Group Name`, x = b4_mutate, FUN = function(x) length(unique(x)))
  b6_cages <- aggregate(unique_cage ~ `Group Name`, x = b6_mutate, FUN = function(x) length(unique(x)))
  
  #What are the lines with the most cages
  #c(b4_cages$`Group Name`[which(b4_cages$unique_cage == max(b4_cages$unique_cage))], max(b4_cages$unique_cage))
  #c(b6_cages$`Group Name`[which(b6_cages$unique_cage == max(b6_cages$unique_cage))], max(b6_cages$unique_cage))
  
  #Combine cage data frames from b4 and b6 into one data frame
  all_cages <- rbind(b4_cages, b6_cages)
  all_cages <- all_cages[order(all_cages$unique_cage),]
  all_cages$`Group Name` <- factor(all_cages$`Group Name`, levels = all_cages$`Group Name`)
  all_cages$AUF_Group <- gsub("^.*?([[:digit:]]{2}-[[:digit:]]{3}).*?$", "\\1" , all_cages$`Group Name`)
  all_cages$group_name_short <- gsub("^(.{10}).*$", "\\1",all_cages$`Group Name`)

}

# Prints plots of cage counts by group name and by AUF
# Run individually or only the last one will show
{
  
  ggplot(all_cages, aes(x = `Group Name`, y = unique_cage)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggplot(all_cages, aes(x = AUF_Group, y = unique_cage)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

# Combine all living mice into one data frame
{
  All_Mice <- rbind(b4_mice, b6_mice)
  All_Mice$Age_Weeks <- All_Mice$Age/days_in_a_week                             #Add column for weeks
  All_Mice$Age_Months <- All_Mice$Age/days_in_a_month                           #Add column for months
  All_Mice$Age_Years <- All_Mice$Age/days_in_a_year                             #Add column for years
  All_Mice$Comments <- gsub("\n", "", All_Mice$Comments)                        #remove new lines in comments (they will affect graphing)
  All_Mice$Comments[which(is.na(All_Mice$Comments))] <- "no comment"            #Add text to unfilled comments for sorting
  All_Mice$Comments[which(All_Mice$Comments == "cull1")] <- "cull"              #remove duplicate/redundant cull notation
  All_Mice$Group <- All_Mice$`Group Name`                                       #make new group column name for easier access
  All_Mice <- mutate(.data = All_Mice, unique_cage = paste(Group,`Cage #`))     #make unique cage label for easier sorting
}

# Makes Graphs for individual lines and saves to local directory
# Cas9, PTEN, TFAM, WT-MNV-free, STAT1
# local directory is ~/Documents/Lab Projects/MiceInR if unmodified
{
  #CAS9 mice
  CAS9_Mice <- All_Mice[which(All_Mice$`Group Name` == "CAS9 (MNV-Free Room 6107)"),]
  Line_Name <- CAS9_Mice$`Group Name`[1]
  ggplot(CAS9_Mice, aes(x = Age_Months, fill = Genotype)) +
    geom_histogram(binwidth = 0.3333333) +
    labs(title = "CAS9 (MNV-Free Room 6107)", subtitle = import_date, y = "Mice")
  if(save_to_file){
    ggsave(filename = paste0(getwd(),"/Images and Graphs/",Line_Name,".png"), width = widescreen[1], height = widescreen[2])
  }
  
  #PTEN 
  PTEN_Mice <- All_Mice[which(All_Mice$`Group Name` == "Pten (MNV-Free Room 6107)"),]
  PTEN_Mice <- PTEN_Mice[which(!(is.na(PTEN_Mice$Genotype)&PTEN_Mice$Age_Months>3)),]
  PTEN_Mice <- PTEN_Mice[which(!grepl(pattern = "Pten wt/wt", x = PTEN_Mice$Genotype)),]
  Line_Name <- PTEN_Mice$`Group Name`[1]
  ggplot(PTEN_Mice, aes(x = Age_Months, fill = Genotype)) +
    geom_histogram(binwidth = 0.3333333) +
    labs(title = Line_Name, subtitle = import_date, y = "Mice")
  if(save_to_file){
    ggsave(filename = paste0(getwd(),"/Images and Graphs/",Line_Name,".png"), width = widescreen[1], height = widescreen[2])
  }
  
  #TFAM experimental mice
  TFAM_Line <- unique(All_Mice$`Group Name`[grep("TFAM", All_Mice$`Group Name`)])[1]
  TFAM_Mice <- All_Mice[which(All_Mice$`Group Name` == TFAM_Line),]
  Line_Name <- TFAM_Mice$`Group Name`[1]
  ggplot(TFAM_Mice, aes(x = Age_Months, fill = Genotype)) +
    geom_histogram(binwidth = 0.3333333) +
    labs(title = Line_Name, subtitle = paste(import_date, "Aging TFAM Line"))
  if(save_to_file){
    ggsave(filename = paste0(getwd(),"/Images and Graphs/",Line_Name,".png"), width = widescreen[1], height = widescreen[2])
  }
  
  #WT MNV-free mice
  WT_Mice <- All_Mice[which(All_Mice$`Group Name` == "WT (MNV-Free Room 6107)"),]
  Line_Name <- WT_Mice$`Group Name`[1]
  ggplot(WT_Mice, aes(x = Age_Months, fill = Sex)) +
    geom_histogram(binwidth = 0.3333333) +
    labs(title = Line_Name, subtitle = import_date)
  if(save_to_file){
    ggsave(filename = paste0(getwd(),"/Images and Graphs/",Line_Name,".png"), width = widescreen[1], height = widescreen[2])
  }
  
  #STAT1 mice
  STAT1_Mice <- All_Mice[which(All_Mice$`Group Name` == "STAT1"),]
  Line_Name <- STAT1_Mice$`Group Name`[1]
  ggplot(STAT1_Mice, aes(x = Age_Months, fill = Sex)) +
    geom_histogram(binwidth = 0.3333333) +
    labs(title = "STAT1", subtitle = import_date)
  if(save_to_file){
    ggsave(filename = paste0(getwd(),"/Images and Graphs/",Line_Name,".png"), width = widescreen[1], height = widescreen[2])
  }
  
}

# Makes a separate data frame for P01 mice based on custom list of line names
# This list is likely outdated as of 4/24/2023.  Several lines have been added since the last update
{
  P01_liver_lines <- c("Adams:(B4) C57BL/6JN-Crl", "Adams: C57BL/6JN-CR(Gout) AUF 21-008 (S-423) ( Young Ctrl)", "Adams: C57BL/6-Charles River- 20-044", "Adams/Feng: C57BL/6JN-NIA-CR- Lee-22-002", "Adams: (B-4) C57BL/6NCrl inbred Andrew", "Adams: C57BL/6JN-COHORT-CR", "Adams: C57BL/6-CR - Armin-22-034", "Adams: C57BL/6JN - CR - 19-100  (S-178)", "Adams: (B-4) 8/23/22 CR Aaron C57BL/6JN NIA(free)", "Adams: C57BL/6 - NIA", "Adams 8/23/22 CR C57BL/6 Michael/Adarsh", "Adams:(B-4)  C57BL/6 Aaron-CR", "Adams:Andrew-C57BL/6-CR-NIA COHORT-22Mo", "Adams: STAT1 22-002)", "Adams(B-4)-TFAM", "Adams :( B-4) 4133- WT:C57BL/6 (21-011)", "Adams:WT (22-034)", "WT (MNV-Free Room 6107)", "Pten (MNV-Free Room 6107)", "P53 flox ( MNV-Free) (S-34)", "Cyclin D1 – fl/fl (S-46) (MNV Free Room 6107)", "CAS9 (MNV-Free Room 6107)", "STAT1", "eGFP (S-43)", "WT", "rCAS", "FAH KO (S-44)", "TFAM - Breeding - Quar", "FAH KO - Quar")
  P01_Mice <- All_Mice[All_Mice$`Group Name` %in% P01_liver_lines,]
  P01_Mice_without_cull <- P01_Mice[which(P01_Mice$Comments!="cull"),]
  P01_Cages <- aggregate(unique_cage ~ Group, P01_Mice, FUN=function(x) length(unique(x)))
  P01_Cages <- P01_Cages[order(P01_Cages$unique_cage, decreasing = F),]
  P01_Cages_without_cull <- aggregate(unique_cage ~ Group, P01_Mice_without_cull, FUN=function(x) length(unique(x)))
  P01_Cages_without_cull <- P01_Cages_without_cull[order(P01_Cages_without_cull$unique_cage, decreasing = F),]
  P01_Cages_without_cull$Group <- factor(P01_Cages_without_cull$Group, levels = P01_Cages_without_cull$Group)
  P01_Cages_without_cull$AUF_Group <- gsub("^.*?([[:digit:]]{2}-[[:digit:]]{3}).*?$", "\\1" , P01_Cages_without_cull$Group)
  P01_Cages_without_cull$group_name_short <- gsub("^(.{10}).*$", "\\1",P01_Cages_without_cull$Group)

  #Count of P01 cages
  sum(P01_Cages$unique_cage)
  sum(P01_Cages_without_cull$unique_cage)

  ggplot(P01_Cages_without_cull, aes(x = Group, y = unique_cage)) +
    geom_bar(stat = "identity") +
    labs(y = "Cage Count") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggplot(P01_Cages_without_cull, aes(x = AUF_Group, y = unique_cage)) +
    geom_bar(stat = "identity") +
    labs(title = "Cage Counts", subtitle = import_date, y = "Cage Count") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  if(save_to_file){
    ggsave(filename = paste0(getwd(),"/Images and Graphs/P01 Cage Counts.png"), width = widescreen[1], height = widescreen[2])
  }
  
}

# A script to aggregate mice to assess ages and amount of mice leftover from previous experiments
# Prints to a file named "Mice by protocol.csv" in the local directory
# local directory is ~/Documents/Lab Projects/MiceInR if unmodified
{
  b4_only = TRUE
  AUF_list <- unique(All_Mice$`Protocol #`)
  AUF_dictionary <- data.frame(protocol = c("20-028", "20-044", "20-045", "21-001", "21-008", "21-025", "21-027", "21-036", "21-062", "22-001", "22-005", "22-007", "22-008", "22-009", "22-010", "22-020", "22-032", "22-033", "22-034", "22-039", "22-040", "22-051", "22-053", "22-059", "22-069", "22-084", "22-086", "22-089", "23-010", "23-011"),
                               primary = c("Karl Miller", "Aaron Havas", "Aaron Havas", "Andrew Davis", "Aaron Havas", "Aaron Havas", "Aaron Havas", "Aaron Havas", "Andrew Davis", "Aaron Havas", "Sha Li", "Rouven Arnold", "Rouven Arnold", "Rouven Arnold", "Rouven Arnold", "Aaron Havas", "Shanshan Yin", "Armin Gandhi", "Armin Gandhi", "Aaron Havas", "Andrew Davis", "Adarsh Rajesh", "Adarsh Rajesh", "Andrew Davis", "Clara Guida", "Aaron Havas", "Nirmalya Dasgupta", "Marcos Garcia Teneche", "Adarsh Rajesh", "Sha Li"),
                               title = c("CCF and anti-SASP Interventions In vivo", "Oncogenesis in Old HFD Mouse Model", "Tumor Development in HFD Mouse Model", "HDAC Inibitors of CCF in vivo", "Drugs to Modulate the Effects of Age on Liver", "Identify role of Type I and II IFN in Age-associated Increase in Stat1 and IFN Stimulated Gene Expression", "Investigate the Ability of MIEL in vivo in Age", "Question: Identify Role of Sting in Promoting type I IFN stimulated Gene Expression and PDL1", "Rapamycin and HIRA deficiency in Mouse Melanocytes", "AAV-saCas9-sg(gene) Injection Model in Liver Aging", "Epigenetic reactivation of the p53 Tumor Suppressor Pathway in AML", "Does Inactivation of HIRA in the Young Adult Liver cause Age-associated Liver Degeneration during Aging", "Does Embryonic Inactivation [Tyrosinase CRE] of HIRA in Melanoblasts cause Premature Hair Greying during Aging?", "Inactivation of HIRA in Aging Melanoblasts (Tyrosinase CRE activation)", "Inactivation of HIRA in Aging Neurons", "Question: Identify role of PD1 upregulation in age-associated increase in Stat1 and IFN stimulated gene expression.", "Identify the Role of Aging in Breast Cancer", "To test if old Hepatocytes are Resistant to Antigen-directed Immune Clearance", "To Establish a CRISPR Activation Approach for Screening Oncogenic and Regenerative Potential of Genes in the Mouse Liver.", "To study interferon inhibition as an intervention against age-related inflammation and cancer initiation in the liver.", "Descriptive Data Collection of Single-cell Multi-omic and Spatial Transcriptomic Profiling of Cellular Senescence in Five Mouse Tissues across Lifespan in Two Different Mouse Strains", "Exploring the Differences in Synthesis of Oncogenes and Tumor Suppressors with Age In-vivo using Protease Inhibitor", "To Explore the Role of Ccnd1 in Cell Proliferation in the Aging Liver", "Epigenetics of Aging and Cancer (Breeding)", "Investigating the effects of Ezh2 inhibitor (EPZ-6438) on Cardiac Aging", "High Fat Diet and Liver Carcinogenesis in Young and Old Mice", "The Role of Hira during Stressed Induced Senescence in the Liver", "Effects of Aging in Leukemia progression and Initiation", "To study CDK4/6 Inhibition as an Intervention against Age-related Inflammation Cancer Initiation in the Liver.", "Breeding and Maintenance of NSG Mice")
                               )
  All_Mice$Age_Months_rounded <- round(All_Mice$Age_Months)
  
  for(i in 1:length(AUF_list)){
    if(i == 1){
      if(is.na(AUF_list[i])){i=i+1}
      mice_protocol_ages <- aggregate(`Group Name` ~ Comments + Age_Months_rounded + Sex + `Group Name`, x = All_Mice[which(All_Mice$`Protocol #` == AUF_list[i]),], FUN = function(x) length(x))
      mice_protocol_ages$protocol <- AUF_list[i]
      names(mice_protocol_ages) <- gsub("Group Name", "count", names(mice_protocol_ages))
    }
    if(is.na(AUF_list[i])){i=i+1}
    mice_protocol_ages_temp <- aggregate(`Group Name` ~ Comments + Age_Months_rounded + Sex + `Group Name`, x = All_Mice[which(All_Mice$`Protocol #` == AUF_list[i]),], FUN = function(x) length(x))
    mice_protocol_ages_temp$protocol <- AUF_list[i]
    names(mice_protocol_ages_temp) <- gsub("Group Name", "count", names(mice_protocol_ages_temp))
    mice_protocol_ages <- rbind(mice_protocol_ages, mice_protocol_ages_temp)
  };rm(mice_protocol_ages_temp)
  mice_protocol_ages <- mice_protocol_ages[,c("protocol", "Age_Months_rounded", "Sex", "count", "Comments")]
  head(mice_protocol_ages)
  for(i in 1:length(mice_protocol_ages$protocol)){
    if(i == 1){
      protocol_description <- c()
      primary <- c()
      match_found <- FALSE
    }
    for(j in 1:length(AUF_dictionary$protocol)){
      if(mice_protocol_ages$protocol[i] == AUF_dictionary$protocol[j]){
        protocol_description <- c(protocol_description, AUF_dictionary$title[j])
        primary <- c(primary, AUF_dictionary$primary[j])
        match_found <- TRUE
      }
    }
    if(match_found == FALSE){
      protocol_description <- c(protocol_description, "no description found")
      primary <- c(primary, "no primary found")
    }
    match_found <- FALSE
  }
  
  tryCatch(length(protocol_description) == length(mice_protocol_ages$protocol))
  
  mice_protocol_ages$description <- protocol_description
  rm(protocol_description)
  mice_protocol_ages$primary <- primary
  rm(primary)
  
  write_csv(mice_protocol_ages, "Mice by protocol.csv")
}
