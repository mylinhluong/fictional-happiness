#this script includes all libraries needed for scripts, and imports the self-report and IAT data 
#todo
#should libraries be loaded here or by script?
##make sure to decide whether or not the package DATA.TABLE will be used or not.

#load libraries here
library(here)
library(readr)
library(gdata)
library(dplyr)
library(data.table)
library(psych)
library(mice)
library(tidyr)


#import IAT and self-report raw datasets into environment
IAT <- read.csv(here("01_data","01_raw", "NCP_OA_PA_IAT_raw.csv"))
self_report <-read.csv(here("01_data","01_raw", "NCP_OA_PA_SR_raw.csv"))
