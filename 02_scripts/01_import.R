#this script imports the self-report and IAT data 
library(readr)

#Create a .txt file within the errors folder
import_01 <- file(here("02_scripts","Errors", "01_import.txt"), open = "wt")
sink(import_01, type = "message")

#import IAT and self-report raw datasets into environment
IAT <- read.csv(here("01_data","01_raw", "NCP_OA_PA_IAT_raw.csv"))
self_report <-read.csv(here("01_data","01_raw", "NCP_OA_PA_SR_raw.csv"))

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(import_01)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "01_import.txt"))