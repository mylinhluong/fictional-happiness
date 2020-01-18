#this script includes all libraries needed for scripts, imports data, ?should functions be included here or in a separate R script?

#todo
#should libraries be loaded here or by script?
#should functions be loaded here or by script?
#check on packrat MICE package to see if it working appropriately
#make sure to decide whether or not the package DATA.TABLE will be used or not.

#load libraries here
library(here)
library(readr)
library(gdata)
library(dplyr)
library(data.table)
library(psych)
library(mice)
library(tidyr)

#function for converting variable types#
convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, function(x) FUN1(as.character(x)))
  as.data.frame(out)
}



"console message
package ‘mice’ successfully unpacked and MD5 sums checked
Error in install.packages : ERROR: failed to lock directory ‘C:\Users\My-Linh\Dropbox\University of Melbourne\Dissertation\CH 3 Non conscious processes\fictional-happiness\packrat\lib\x86_64-w64-mingw32\3.6.1’ for modifying
Try removing ‘C:\Users\My-Linh\Dropbox\University of Melbourne\Dissertation\CH 3 Non conscious processes\fictional-happiness\packrat\lib\x86_64-w64-mingw32\3.6.1/00LOCK’


#import IAT and self-report raw datasets into environment
IAT <- read.csv(here("01_data","01_raw", "NCP_OA_PA_IAT_raw.csv"))
self_report <-read.csv(here("01_data","01_raw", "NCP_OA_PA_SR_raw.csv"))
