library(here)
library(readr)

#import IAT and self-report datasets
IAT <- read.csv(here("01_data","01_raw", "NCP_OA_PA_IAT_raw.csv"))
self_report <-read.csv(here("01_data","01_raw", "NCP_OA_PA_IAT_raw.csv"))

View(IAT)
View(self-report)


