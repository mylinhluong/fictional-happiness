#This script sources several scripts

#make sure to set the directory to where the Rproj is housed (fictional happiness)


#todo
#have to figure out why sourcing scripts isn't working correctly
##https://stackoverflow.com/questions/25273166/combine-and-run-multiple-r-scripts-from-another-script/25273217
##https://stackoverflow.com/questions/41729469/r-script-as-a-function
###This sounds like each script-file should really be a function where the "artifacts" that are needed should be passed as arguments to the function
#think about what will print, what's in global environment vs not
#join self-report and IAT tidy data in main script or in separate, sourced here?

('./scripts/01-import.R')
here()

source("C:\Users\My-Linh\Dropbox\University of Melbourne\Dissertation\CH 3 Non conscious processes\fictional-happiness\02_scripts\01_import.R")
#?why doesn't this work?

source("C:\\My-Linh\\Dropbox\\University of Melbourne\\Dissertation\\CH 3 Non conscious processes\\fictional-happiness\\02_scripts\\01_import.R")
#?why doesn't this work?
#Error in file(filename, "r", encoding = encoding) : 
#  cannot open the connection
#In addition: Warning message:
#  In file(filename, "r", encoding = encoding) :
#  cannot open file 'C:\My-Linh\Dropbox\University of Melbourne\Dissertation\CH 3 Non conscious processes\fictional-happiness\02_scripts\01_import.R': No such file or directory

source("C:/Users/My-Linh/Dropbox/University of Melbourne/Dissertation/CH 3 Non conscious processes/fictional-happiness/02_scripts/01_import.R")
#?why doesn't this work?
#Error: '\U' used without hex digits in character string starting ""console message
#package â€˜miceâ€™ successfully unpacked and MD5 sums checked
#Error in install.packages : ERROR: failed to lock directory â€˜C:\U

##01_import data
#this script includes all libraries needed for scripts, imports data, ?should functions be included here or in a separate R script?
source(here::here("02_scripts", "01_import.R"))

source(here::here('02_scripts/01_import.R'))

source(here("02_scripts/01_import.R"))

source(here("./02_scripts/01_import.R"))

source(here("02_scripts","01_import.R"))

source("../02_scripts/01_import.R")

source(here::here("02_scripts/01_import.R"))

source(here('02_scripts','01_import.R'))

#source(here::here("code/functions.R"))

IAT <- read.csv(here("01_data","01_raw", "NCP_OA_PA_IAT_raw.csv"))

"Error: '\U' used without hex digits in character string starting ""console message
package â€˜miceâ€™ successfully unpacked and MD5 sums checked
Error in install.packages : ERROR: failed to lock directory â€˜C:\U
"
##02_clean_names
#this script selects specific variables for self-report (baseline & follow-up) and IAT dataframes(DF) and renames variables
source()

##03_tidy_selfreport
# this script creates new DF that includes tidy data for scoring self-report data for baseline and follow-up: 
# the BREQ-3, 
# physical exercise self-efficacy (PESES), 
# attitudes about physical activity, 
# the self-report behavioural automaticity index (SRBAI)
# decisional intention & intention strength
# pain numeric rating scale (NRS)
# sociodemographics
source()

##03_tidy_selfreport_IPAQ
# this script scores IPAQ data 
source()

##03_tidy_IAT
#This script computes d-scores for automatic evaluations (i.e. IAT_eval) and automatic self-schema (i.e. IAT_id) from the implicit association test (IAT).
source()

##04 join tidy_selfeport, tidy_selfreport_IPAQ and tidy_IAT
#in main script?

##still working-determine structure for names##
##Rmd files for 
#descriptive
#analysis1 (main)
#analysis2 (moderator)
#analysis3 (exploratory)

#include the following at the header of the Rmd files

{r load_scripts, include = FALSE}
source('./scripts/01-import.R')
source('./scripts/02-clean-names.R')
source('./scripts/03-tidy.R')

{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

#primarily visualizations and results of analyses where all code chunks are hidden using global chunk options at the top of the Rmd file
