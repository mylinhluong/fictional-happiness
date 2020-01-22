#This script sources several scripts

#make sure to launch the R process from the project's top-level directory, where the Rproj is housed  (fictional happiness)
#if you launch R from the shell, cd to the correct folder first

##01_import data
#this script includes all libraries needed for scripts, imports data, ?should functions be included here or in a separate R script?
source('./02_scripts/01_import.R') 

##02_clean_names
#this script selects specific variables for self-report (baseline & follow-up) and IAT dataframes(DF) and renames variables
source('./02_scripts/02_clean_names.R') 

##03_tidy_selfreport
# this script creates new DF that includes tidy data for scoring self-report data for baseline and follow-up: 
# the BREQ-3, 
# physical exercise self-efficacy (PESES), 
# attitudes about physical activity, 
# the self-report behavioural automaticity index (SRBAI)
# decisional intention & intention strength
# pain numeric rating scale (NRS)
# sociodemographics
source('./02_scripts/03_tidy_selfreport.R') 

##03_tidy_IAT
#This script computes d-scores for automatic evaluations (i.e. IAT_eval) and automatic self-schema (i.e. IAT_id) from the implicit association test (IAT).
source('./02_scripts/03_tidy_IAT.R') 


##04 join tidy_selfeport and tidy_IAT
#in main script?
complete_data_processed<-IAT_processed_dscore%>%
  left_join(self_report_processed,by="group")

write.csv(complete_data_processed,"01_data/02_processed/complete_data_processed.csv", row.names=FALSE)

####
##still working-determine structure for names##
##Rmd files for 
#descriptive
#analysis1 (main)
#analysis2 (moderator)
#analysis3 (exploratory)

#include the following at the header of the Rmd files

#{r load_scripts, include = FALSE}
#source('./scripts/01-import.R')
#source('./scripts/02-clean-names.R')
#source('./scripts/03-tidy.R')

#{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = FALSE)

#primarily visualizations and results of analyses where all code chunks are hidden using global chunk options at the top of the Rmd file