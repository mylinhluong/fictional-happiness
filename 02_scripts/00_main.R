#Next steps
#look at hierarchy
#where does reliability fit in
#where does sensitivity analysis fit it
#write and run reliability code
#all scripts should be able to be run independently
#push to git?
#BONUS/STRETCH goals
#look at how to clean up environment after sourcing programs
#convert assumption testing and analyses to Rmd/dynamic report
#save assumption testing plots as a grid using gridExtra 
#(https://bookdown.org/lyzhang10/lzhang_r_tips_book/how-to-plot-data.html#putting-plots-in-one-panel)
#check code efficiency using profvis
#https://bookdown.org/lyzhang10/lzhang_r_tips_book/how-to-check-code-efficiency.html

#This script sources several scripts
#make sure to launch the R process from the project's top-level directory, where the Rproj is housed  (fictional happiness)
#if you launch R from the shell, cd to the correct folder first
#Please run the R programs in order

#load library here
library(here)

############SCRIPTS############
##01_import data
#this script imports data
source(here("02_scripts","01_import.R")) 

##02_clean_names
#this script selects specific variables for self-report (baseline & follow-up) and IAT dataframes(DF) and renames variables
source(here("02_scripts","02_clean_names.R")) 

##03_tidy_selfreport
# this script creates new dataframe that includes tidy data for scoring self-report data for baseline and follow-up: 
# the BREQ-3, 
# physical exercise self-efficacy (PESES), 
# attitudes about physical activity, 
# the self-report behavioural automaticity index (SRBAI)
# decisional intention & intention strength
# pain numeric rating scale (NRS)
# sociodemographics
source(here("02_scripts","03_tidy_selfreport.R"))

##03_tidy_IAT
#This script computes d-scores for automatic evaluations (i.e. IAT_eval) and automatic self-schema (i.e. IAT_id) from the implicit association test (IAT).
source(here("02_scripts","03_tidy_IAT.R"))

##03 joins tidy_selfreport and tidy_IAT datasets
#This joins two data sets together to create a complete data set
#complete_data_processed<-IAT_processed_dscore%>%
#  full_join(self_report_processed,by="group")

#View(complete_data_processed)

#These data exist already in the Rproj as an rds and a csv
#Save object to an rds file to preserve column data types
#saveRDS(complete_data_processed,"01_data/02_processed/complete_data_processed.rds")

#Write to CSV file
#write.csv(complete_data_processed,"01_data/02_processed/complete_data_processed.csv", row.names=FALSE)

##04 inspection of data (currently as a script, eventually as Rmd file)
#https://psyteachr.github.io/msc-conv-f2f/rm2-r-markdown-correlations.html
#https://stackoverflow.com/questions/39368928/using-data-from-environment-in-r-markdown
#https://gist.github.com/sebkopf/7caffdd8b299ed73914a

#This script visually tests key assumptions of linear relationships to determine if any transformations are needed
#using standard diagnostic plots
#Key assumptions (affiliated plots) are:
#1)Linearity of the relationship between y and its explanatory variables (Residuals vs Fitted)
#2)Independence of variables where explanatory variables are not highly correlated w/ each other
#3)Normal distribution of residuals (Normal Q-Q)
#4)Homoscedasticity of equal variance of residuals (Scale-Location)
#bottomline: There isn't a clear rationale for a transformation. It *is* surprising that the d_id (self-schema)
#data appear in a somewhat negative relationship. Also surprising that habit does not appear to be correlated 
#with T2 physical activity.
#If outlier was more than 3 SD above or below the mean, we winsorised (i.e. replaced extreme values with less extreme values) 
#the extreme values to the next higher or lowest value found in the sample that was not an outlier
#source(here("02_scripts","04_inspection.R"))

##05 imputation (currently as a script, eventually as Rmd file)
#This script creates an imputed dataset that pools 155 imputations to predict values using predictive mean matching
#This script also does graphical and descriptive diagnostic checking to explore imputed vaues generated 
#by the imputation model & comparisons b/w observed and imputed data
#source(here("02_scripts","05_imputation.R"))

############OUTPUT############
##01_analyses (currently as a script, eventually as Rmd file)
#This script runs estimates of associations between physical activity (IPAQ) and 
#each non-conscious processes using linear regression models
#w/ statistical significance set at p<0.05
#We conduct three-step multiple regression to identify the independent contributions of each non-conscious process at baseline on
#physical activity at one-week follow-up
#Step 1: IPAQ at baseline and each non-conscious process (automatic evaluations, automatic self-schema, habit automaticity)
#Step 2: + demographic covariates (age, gender, body mass index, income, education, location)
#Step 3: + reflective processes (decisional intention, intention strength, self-effacy for physical activity when experiencing pain
#while walking)
#Step 4: Moderation, including a mean-centered interaction term between pain and each non-conscious process

##02_posthoc (currently as a script, eventually as Rmd file)
#This script runs estimates of association between physical activity (total minutes of physical activity, 
#total minutes of moderate-vigorous physical activity) and non-conscious processes using linear regression. 
#This script also runs estimates of association between physical activity (IPAQ) and 
#non-conscious processses without accounting for physical activity at baseline. 
#We also conduct a sensitivity analysis that compares main analyses performed 
#without winsorisation of outliers 3 SD above or below the mean
