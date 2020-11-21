#This script creates an imputed dataset that pools 154 imputations to predict values using predictive mean matching
##The csv for these data already exist in the fictional-happiness folders, run without # if needed
#write.csv(imp.int,"01_data/02_processed/01_imputed_data.csv", row.names=FALSE)
#to load csv into environment
#imp.int<- read_csv(here("01_data","02_processed", "01_imputed_data.csv"))

#things to resolve:
##dealing with squeeze for BMI
##sensitivity analysis

###############LOAD LIBRARIES###############
library(here)
library(dplyr)
library(mice)
library(lattice)
library(naniar)#to explore missingness
set.seed(734)
options(max.print=999999)

#Create a .txt file within the errors folder
imputation_05 <- file(here("02_scripts","Errors", "05_imputation.txt"), open = "wt")
sink(imputation_05, type = "message")

###############LOAD FUNCTIONS###############
#function to calculate how many imputations to compute
how_many_imputations <- function(model,
cv = .05,
alpha = .05) {
  if (is(model, 'mira')) {
    model <- mice::pool(model)
  }
  if (!is(model, "mipo")) {
    stop("Model must be multiply imputed.")
  }
  fmi <- max(model$pooled$fmi)
  z <- qnorm(1 - alpha/2)
  
  fmiu <- plogis(qlogis(fmi) + z*sqrt(2/model$m))
  
  ceiling(1 + 1/2*(fmiu/cv)^2)
}

###############IMPORT DATA###############
#the following imported dataset is the processed dataset containing all variables
data<- readRDS(here("01_data","02_processed","complete_data_processed.rds"))

#colnames(data)

#the following dataset is a processed dataset with incomplete/missing data and includes 33 variables:
#automatic processes, Physical activity at T1 and T2, Pain at T1 and T2, 
#self-efficacy, intention, sociodemographic chars, BREQ-3 motivation

data_miss<-data%>%
  select(group, d_eval,d_id, habit,
         IPAQ_MET_MIN_WEEK_T1_trunc,IPAQ_TOTAL_MIN_WMV_T1, IPAQ_TOTAL_MVPA_T1,
         IPAQ_MET_MIN_WEEK_T2_trunc,IPAQ_TOTAL_MIN_WMV_T2,IPAQ_TOTAL_MVPA_T2,
         pain_T1,pain_T2,
         selfefficacy,selfefficacy_walk,
         intention,intention_min,intention_strength,
         age,gender,height, weight,income,educ_years,state,study_knee,BMI_calc,
         instrumental,affective,
         identified, amotivation,intrinsic,introjected,integrated,extrinsic)

#View(data_miss)
str(data_miss)

###############INSPECT THE INCOMPLETE DATA###############
#Data of first and last 10 participants in the dataset
#head(data_miss,10) 
#tail(data_miss, 10)

#summary(data_miss)

#     d_eval             d_id              habit       IPAQ_MET_MIN_WEEK_T1_trunc IPAQ_TOTAL_MIN_WMV_T1 IPAQ_TOTAL_MVPA_T1
#Min.   :-1.2172   Min.   :-1.28287   Min.   :1.000   Min.   :    0              Min.   :  0.0         Min.   :  0       
#1st Qu.: 0.6698   1st Qu.:-0.35339   1st Qu.:3.000   1st Qu.: 2202              1st Qu.: 85.0         1st Qu.: 10       
#Median : 0.9287   Median : 0.03326   Median :4.750   Median : 4373              Median :180.0         Median : 60       
#Mean   : 0.8352   Mean   : 0.02792   Mean   :4.361   Mean   : 5036              Mean   :199.6         Mean   :104       
#3rd Qu.: 1.1076   3rd Qu.: 0.40393   3rd Qu.:5.625   3rd Qu.: 6971              3rd Qu.:300.0         3rd Qu.:180       
#Max.   : 1.6318   Max.   : 1.18383   Max.   :7.000   Max.   :21798              Max.   :540.0         Max.   :360       
#NA's   :22        NA's   :22         NA's   :6       NA's   :51                 NA's   :47            NA's   :42        

#IPAQ_MET_MIN_WEEK_T2_trunc IPAQ_TOTAL_MIN_WMV_T2 IPAQ_TOTAL_MVPA_T2    pain_T1      pain_T2    selfefficacy   selfefficacy_walk
#Min.   :   33              Min.   :  0.0         Min.   :  0.00     4      :45   4      :37   Min.   :1.000   Min.   :1.000    
#1st Qu.: 1894              1st Qu.: 70.0         1st Qu.:  0.00     6      :42   3      :36   1st Qu.:2.625   1st Qu.:2.000    
#Median : 3744              Median :167.5         Median : 60.00     5      :39   6      :31   Median :3.000   Median :3.000    
#Mean   : 4491              Mean   :185.1         Mean   : 92.71     3      :36   5      :27   Mean   :3.008   Mean   :2.704    
#3rd Qu.: 6328              3rd Qu.:272.5         3rd Qu.:150.00     7      :35   7      :23   3rd Qu.:3.750   3rd Qu.:3.000    
#Max.   :16924              Max.   :540.0         Max.   :360.00     (Other):35   (Other):46   Max.   :4.000   Max.   :4.000    
#NA's   :69                 NA's   :65            NA's   :62         NA's   :21   NA's   :53   NA's   :6       NA's   :6        

#intention  intention_min    intention_strength      age          gender        height          weight        income     educ_years   
#0   :  9   Min.   :   0.0   1   :  1           Min.   : 46.00   1   :159   Min.   :135.0   Min.   : 49.00   1   :11   Min.   : 0.00  
#1   :223   1st Qu.: 120.0   2   :  7           1st Qu.: 56.00   2   : 61   1st Qu.:160.0   1st Qu.: 72.00   2   :26   1st Qu.:12.00  
#NA's: 21   Median : 240.0   3   : 33           Median : 62.00   NA's: 33   Median :165.0   Median : 83.50   3   :55   Median :15.00  
#Mean   : 320.4   4   : 59           Mean   : 62.38              Mean   :167.1   Mean   : 84.44   4   :65   Mean   :14.84  
#3rd Qu.: 420.0   5   :123           3rd Qu.: 68.00              3rd Qu.:175.0   3rd Qu.: 95.00   5   :13   3rd Qu.:17.12  
#Max.   :6360.0   NA's: 30           Max.   :114.00              Max.   :193.0   Max.   :150.00   6   :48   Max.   :35.00  
#NA's   :26                          NA's   :34                  NA's   :40      NA's   :37       NA's:35   NA's   :37     

#state        study_knee    BMI_calc      instrumental     affective      identified     amotivation       intrinsic    
#6      :80   1   :106   Min.   :18.79   Min.   :0.000   Min.   :0.00   Min.   :0.250   Min.   :0.0000   Min.   :0.000  
#5      :52   2   :120   1st Qu.:25.46   1st Qu.:6.000   1st Qu.:5.00   1st Qu.:2.750   1st Qu.:0.0000   1st Qu.:2.250  
#4      :41   NA's: 27   Median :29.38   Median :7.000   Median :6.00   Median :3.500   Median :0.0000   Median :3.000  
#1      :18              Mean   :30.17   Mean   :6.306   Mean   :5.53   Mean   :3.226   Mean   :0.1933   Mean   :2.876  
#3      :14              3rd Qu.:34.23   3rd Qu.:7.000   3rd Qu.:7.00   3rd Qu.:3.750   3rd Qu.:0.0000   3rd Qu.:3.750  
#(Other):15              Max.   :49.13   Max.   :7.000   Max.   :7.00   Max.   :4.000   Max.   :2.7500   Max.   :4.000  
#NA's   :33              NA's   :45      NA's   :6       NA's   :6      NA's   :6       NA's   :6        NA's   :6      

#introjected      integrated      extrinsic     
#Min.   :0.000   Min.   :0.000   Min.   :0.0000  
#1st Qu.:1.500   1st Qu.:2.000   1st Qu.:0.0000  
#Median :2.000   Median :3.000   Median :0.5000  
#Mean   :2.088   Mean   :2.787   Mean   :0.7348  
#3rd Qu.:2.750   3rd Qu.:3.750   3rd Qu.:1.2500  
#Max.   :4.000   Max.   :4.000   Max.   :3.2500  
#NA's   :6       NA's   :6       NA's   :6  


###############INSPECT THE MISSING DATA PATTERN###############
#Missing data patterns
md.pattern(data_miss, rotate.names=TRUE)
p<-md.pairs(data_miss)
p

vis_miss(data_miss)

##############################################
#CREATE INTERACTION TERMS & DERIVED VARIABLES#
##############################################
###the following dataset creates new interaction variables of automatic proceses x pain 
#a total of 38 variables in the dataset
data_ixn<-cbind(data_miss,pain.eval=NA, pain.id=NA, pain.habit=NA,BMI_derived=NA)

###obtain predictor matrix and imputation method w/ a dry run of mice (i.e. imputation with 0 iterations).
ini<-mice(data_ixn,maxit=0, print=F)

###create variables thru passive imputation
#mean-centered: pain_T2, d_eval, d_id, habit, then created interaction terms 
meth<-ini$meth
meth["pain.eval"]<- "~I((pain_T2-4.685)*(d_eval-0.8352))"
meth["pain.id"]<- "~I((pain_T2-4.685)*(d_id-0.02792))"
meth["pain.habit"] <- "~I((pain_T2-4.685)*(habit-4.361))"
#a BMI calculation to account for imputed missing height/weight data
meth["BMI_derived"]<-"~I((weight/height/height)*10000)"


###repair circularity in predictor matrix
pred<-ini$predictorMatrix
pred["pain_T2", "pain.eval"]<-0
pred["d_eval", "pain.eval"]<-0
pred["pain_T2", "pain.id"]<-0
pred["d_id", "pain.id"]<-0
pred["pain_T2", "pain.habit"]<-0
pred["habit", "pain.habit"]<-0
pred["weight","BMI_derived"]<-0
pred["height","BMI_derived"]<-0
pred["weight","BMI_calc"]<-0

###Note: "pain.[AUTOMATIC-PROCESS]" should be used as a predictor when 
#imputing IPAQ_MET_MIN_WEEK_T2_trunc. Without this step, the imputation model would neglect the product term, 
#thus being incongruent with the analysis model
pred["pain.eval","IPAQ_MET_MIN_WEEK_T2_trunc"]<-1
pred["pain.id","IPAQ_MET_MIN_WEEK_T2_trunc"]<-1
pred["pain.habit","IPAQ_MET_MIN_WEEK_T2_trunc"]<-1
pred["pain.eval","IPAQ_TOTAL_MIN_WMV_T2"]<-1
pred["pain.id","IPAQ_TOTAL_MIN_WMV_T2"]<-1
pred["pain.habit","IPAQ_TOTAL_MIN_WMV_T2"]<-1
pred["pain.eval","IPAQ_TOTAL_MVPA_T2"]<-1
pred["pain.id","IPAQ_TOTAL_MVPA_T2"]<-1
pred["pain.habit","IPAQ_TOTAL_MVPA_T2"]<-1

post <- init$post

#group ID does not predict any other variable
pred[,"group"]<-0

###in future scripts, this is something to play around with more, 
#I ended up doing the post-processing manually
#post <-ini$post
#post["BMI_derived"] <- squeeze(BMI_derived, bounds = c(15,50))


###############CONSIDER HOW MANY IMPUTATIONS NEED TO BE CREATED###############
#See von Hippel, 2019
#First, carry out a pilot analysis. Impute the data using a convenient number of imputations. 
#(20 imputations is a reasonable default, if it doesn’t take too long.) Estimate the FMI by analyzing the imputed data.
#create temporary data set

#Next, plug the estimated FMI into the formula above to figure out how many imputations 
#you need to achieve a certain value of CV(SE). If you need more imputations than you had in the pilot, 
#then add those imputations and analyze the data again.

#library(howManyImputations)
tempData<-mice(data_ixn, m=5,maxit=20, meth = meth, pred = pred, seed=734)

#test to see if data have been imputed for new variables
tempData_long<-mice::complete(tempData, action="long", include = TRUE)
#View(tempData_long)

modelFit1_eval<-with(tempData,lm(IPAQ_MET_MIN_WEEK_T2_trunc~ IPAQ_MET_MIN_WEEK_T1_trunc+d_eval+age+educ_years
        +intention_strength + selfefficacy + selfefficacy_walk+ pain_T2 + pain.eval))

how_many_imputations(modelFit1_eval)
#[1] 155

modelFit1_id<-with(tempData,lm(IPAQ_MET_MIN_WEEK_T2_trunc~ IPAQ_MET_MIN_WEEK_T1_trunc+d_id+age+educ_years
                             +intention_strength + selfefficacy + selfefficacy_walk+ pain_T2 + pain.id))

how_many_imputations(modelFit1_id)
#[1] 152

modelFit1_habit<-with(tempData,lm(IPAQ_MET_MIN_WEEK_T2_trunc~ IPAQ_MET_MIN_WEEK_T1_trunc+habit+age+educ_years
                              +intention_strength + selfefficacy + selfefficacy_walk+ pain_T2 + pain.habit))

how_many_imputations(modelFit1_habit)
#[1] 155


###############CREATING THE IMPUTED DATA SET W/ 136 IMPUTATIONS###############
data_ixn_imp<-mice(data_ixn, m=155, maxit=20, meth =meth, pred = pred, seed = 734) #has interaction terms

        
###############SAVE THE IMPUTED DATA###############
#save as multiply imputed datasets
saveRDS(data_ixn_imp,"01_data/02_processed/01_imputed_data_asmids.rds")

#turn datasets into long format
data_imputed_long<-mice::complete(data_ixn_imp, action="long", include = TRUE)

#Save imputed dataset as csv
write.csv(data_imputed_long,"01_data/02_processed/01_imputed_data.csv", row.names=FALSE)

#Post-processing of BMI to be within bounds
data_imputed_long_post<- data_imputed_long %>% 
  mutate(BMI_calc=squeeze(BMI_calc, bounds = c(15, 50)))%>%
  mutate(BMI_derived = squeeze(BMI_derived, bounds = c(15, 50)))

#Save imputed dataset with post-processing of BMI (calculated and derived) as csv
write.csv(data_imputed_long_post,"01_data/02_processed/01_imputed_data_post.csv", row.names=FALSE)

#saved "asmids" with BMI post-processing in order to run models on multiple imputed datasets
data_imputed_long_post<-as.mids(data_imputed_long_post, where=NULL, .imp = ".imp", .id= ".id")
saveRDS(data_imputed_long_post,"01_data/02_processed/01_imputed_data_long_post_asmids.rds")


#https://uvastatlab.github.io/2019/05/01/getting-started-with-multiple-imputation-in-r/

###CHECK THIS CODE####
#plot(data_ixn_imp)
plot(data_ixn_imp)
data_ixn_imp$meth

table(complete(imp_mids)$BMI_derived)



###############DIAGNOSTIC CHECKING###############
##Diagnostic graphs to study the discrepancy between observed and imputed data
#good imputations have a distribution similar to observed data

imp_mids<- readRDS(here("01_data","02_processed","01_imputed_data_long_post_asmids.rds"))

#Check BMI_calc and BMI_derived
#make a missing data indicator (miss) and check the relation of BMI_derived, 
#weight and height in the imputed dat. To do so, plot the imptued values against their respective
#calculated values
miss <- is.na(imp_mids$data$BMI_calc)

xyplot(imp_mids, BMI_derived ~ I((weight/height/height)*10000), na.groups = miss,
       cex = c(1, 1), pch = c(1, 20),
       ylab = "BMI (kg/m2) Imputed", xlab = "BMI (kg/m2) Calculated")

plot(imp_mids, c("BMI_derived"))

#We can inspect the distributions of the original and imputed data using the stripplot 
#function that is part of the lattice package


plot(imp_mids)
stripplot(imp_mids)

## labels observed data in blue and imputed data in red for y1
col <- rep(c("blue", "red")[1 + as.numeric(is.na(imp1$data$y1))], 6)
## plots data for y1 by imputation
stripplot(y1 ~ .imp, data = imp_tot2, jit = TRUE, col = col, xlab = "imputation Number")


imp_mids_sans<-readRDS(here("01_data","02_processed","01_imputed_data_asmids.rds"))
plot(imp_mids_sans)
stripplot(imp_mids_sans)

imp_mids_BW<-bwplot(
  imp_mids,
  data,
  na.groups = NULL,
  groups = NULL,
  as.table = TRUE,
  theme = mice.theme(),
  mayreplicate = TRUE,
  allow.multiple = TRUE,
  outer = TRUE,
  drop.unused.levels = lattice::lattice.getOption("drop.unused.levels"),
  ...,
  subscripts = TRUE,
  subset = TRUE
)

bwplot
  x,
  data,
  na.groups = NULL,
  groups = NULL,
  as.table = TRUE,
  theme = mice.theme(),
  mayreplicate = TRUE,
  allow.multiple = TRUE,
  outer = TRUE,
  drop.unused.levels = lattice::lattice.getOption("drop.unused.levels"),
  ...,
  subscripts = TRUE,
  subset = TRUE
)


#DV
imp_mids$imp$IPAQ_MET_MIN_WEEK_T2_trunc

#imp$imp$IPAQ_TOTAL_MIN_WMV_T2
#imp$imp$IPAQ_TOTAL_MVPA_T2

#main IV
imp$imp$d_eval
imp$imp$d_id
imp$imp$habit

#other predictor IV
imp$imp$IPAQ_MET_MIN_WEEK_T1_trunc
imp$imp$intention_strength
#imp$imp$IPAQ_TOTAL_MIN_WMV_T1
#imp$imp$IPAQ_TOTAL_MVPA_T1

#sociodem covariates
imp$imp$gender
imp$imp$age
imp$imp$BMI
imp$imp$location
imp$imp$income
imp$imp$educ

bwplot(imp, pch=20,cex=1.2)





###############EXPLORE THE IMPUTED DATA###############
#generate descriptive and summary statistics of the imputed data



#graphical displays such as histograms or boxplots





#compare observed and imputed data (an internal check)
##this includes boxplots, density plots, cumulative distribution plots
##strip plots and quantile-quantile plots




#end of script
#close the error message catching script and save the file
sink(type = "message")
close(imputation_05)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "05_imputation.txt"))


#Sources
#to determine how many imputations are needed von Hippel, 2018: https://statisticalhorizons.com/how-many-imputations
#https://rdrr.io/github/josherrickson/howManyImputations/src/R/how_many_imputations.R
#https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul/219049
#Hippel 2018, How many imputations do you need? https://arxiv.org/abs/1608.05406
#Nguyen et al.2017, Model checking in MI https://ete-online.biomedcentral.com/articles/10.1186/s12982-017-0062-6
#https://stats.stackexchange.com/questions/200477/compute-95-confidence-interval-for-predictions-using-a-pooled-model-after-multi
#https://github.com/stefvanbuuren/mice/issues/92 for CI
#https://rdrr.io/cran/mice/man/pool.r.squared.html for r-square
#https://www.tandfonline.com/doi/full/10.1080/00273171.2018.1540967 for significance tests and esitmates (F)
#derived variables: https://stefvanbuuren.name/fimd/sec-knowledge.html

#Notes
#analysis model vs. models used for imputation
#include all the variables from the analysis model in the imputation model 
#to ensure imputation model preserved the relationships between the vars of interest
#include auxiliary variables that are good candidates (i.e. highly correlated with incomplete vars, 
#which could improve prediction of missing values)
#include predictors of missingess

#Theoretically it is always better to use higher m, but this involves more computation and storage. Setting  
#m very high (say  m   =200) may be useful for low-level estimands that are very uncertain, and for which we want to approximate the full distribution, or for parameters that are notoriously different to estimates, like variance components. On the other hand, setting  
#m high may not be worth the extra wait if the primary interest is on the point estimates (and not on standard errors,  
# p -values, and so on). In that case using  m=5−20 will be enough under moderate missingness.

#Imputing a dataset in practice often involves trial and error to adapt and refine the imputation model. Such initial explorations do not require large  
#m. It is convenient to set  m= 5 during model building, and increase  m only after being satisfied with the model for the “final” round of imputation. 
#So if calculation is not prohibitive, we may set  
#m to the average percentage of missing data. The substantive conclusions are unlikely to change as a result of raising  
#m beyond m=  5

#explore the imputed value
#generate descriptive statistics
#graphical displays such as histograms or boxplots
#compare observed and imputed data (an internal check)
##this includes boxplots, density plots, cumulative distribution plots
##strip plots and quantile-quantile plots
##generate summary statistics of of the observed and imputed data

#passive imputation: since transformed variable is available for imputation, the hope is that passive imputation
#removes the bias of the imputation, then transform methods, while restoring consistency among the imputations that was broken

#imputation models
#1. assume missing at random (MAR) or missing not at random (MNAR)
#2. form of the imputation model (structural and assumed error distribution, refer to 3.2)
#3. set of variables to include as predictors into the imputation model
#include as many relevant variables as possible, 3.3 for possibilities
#4 should we impute variables that are functions of other incomplete varrs? 3.4 on passive imputation
#5 order in which variables should be imputed, 3.6
#6 setup of the starting imputations and number of iterations. 
#convergence can be monitored Section 4.3
#7 m, the number of multiply imputed data sets m too low = large simulation error, 
#especially if missing information is high

#https://stackoverflow.com/questions/58095295/imputation-in-mice-post-processing-r
complete(imp.int)
attributes(imp.int)

####4. in earlier iterations of mice Post-process the values to constrain them between 1 and 25, use norm as the imputation method 
#for tv.
#In this way the imputed values of tv are constrained (squeezed by function squeeze()) between 1 and 25.
ini <- mice(boys, maxit = 0)
meth <- ini$meth
meth["tv"] <- "norm"
post <- ini$post
post["tv"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 25))"
imp <- mice(boys, meth=meth, post=post, print=FALSE)
  ###############WHAT IS THIS?###############

#datlist<-miceadds::mids2datlist(imp.int)
#fit_m1_eval.list<-with(datlist, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_eval))
#betas<-lapply(fit_m1_eval.list,coef)
#summary(pool_mi(betas))

#vars<-lapply(fit_m1_eval.list,FUN = function(x){vcovCL(x, cluster = datlist[[1]]$idschool)}))



colnames(complete_data_processed)
#Residual standard error: 2575 on 151 degrees of freedom
#(99 observations deleted due to missingness)
#Multiple R-squared:  0.447,	Adjusted R-squared:  0.4397 
#F-statistic: 61.03 on 2 and 151 DF,  p-value: < 2.2e-16
visreg(lm1_eval)
par(mfrow=c(2,2))
plot(lm1_eval)

#Proportion of usable cases
p<-md.pairs(data_mis)
p

p$mr/(p$mr+p$mm)
flux(data_mis)[,1:3]
#influx of variable quantifies how well its missing data connect to the observed data in other vars
#outflux of variable quantifies how well its observed data connect to the missing data on other vars
#in genderal, higher influx and outflux vars are preferred

                            pobs      influx   outflux
d_eval                     0.9130435 0.070934534 0.7508143
d_id                       0.9130435 0.070934534 0.7508143
identified                 0.9762846 0.001930191 0.7557003
amotivation                0.9762846 0.001930191 0.7557003
intrinsic                  0.9762846 0.001930191 0.7557003
introjected                0.9762846 0.001930191 0.7557003
integrated                 0.9762846 0.001930191 0.7557003
extrinsic                  0.9762846 0.001930191 0.7557003
selfefficacy               0.9762846 0.001930191 0.7557003
selfefficacy_walk          0.9762846 0.001930191 0.7557003
instrumental               0.9762846 0.001930191 0.7557003
affective                  0.9762846 0.001930191 0.7557003
habit                      0.9762846 0.001930191 0.7557003
intention                  0.9169960 0.033456651 0.4153094
intention_min              0.8972332 0.052115168 0.3843648
intention_strength         0.8814229 0.070291137 0.3925081
study_knee                 0.8932806 0.055653852 0.3762215
pain_T1                    0.9169960 0.033456651 0.4153094
IPAQ_MET_MIN_WEEK_T1_trunc 0.7984190 0.142995014 0.2052117
age                        0.8656126 0.072703876 0.2410423
gender                     0.8695652 0.068521795 0.2426710
BMI                        0.8656126 0.072221329 0.2361564
income                     0.8616601 0.076564259 0.2361564
educ                       0.8537549 0.084928422 0.2328990
location                   0.8695652 0.068521795 0.2426710
pain_T2                    0.7905138 0.161653531 0.3061889
IPAQ_MET_MIN_WEEK_T2_trunc 0.7272727 0.224867299 0.2426710

quickpred(data_mis)

imp$loggedEvents




IPAQ_MET_MIN_WEEK_T2_trunc
d_eval                                      0.5454545
d_id                                        0.5454545
identified                                  0.0000000
amotivation                                 0.0000000
intrinsic                                   0.0000000
introjected                                 0.0000000
integrated                                  0.0000000
extrinsic                                   0.0000000
selfefficacy                                0.0000000
selfefficacy_walk                           0.0000000
instrumental                                0.0000000
affective                                   0.0000000
habit                                       0.0000000
intention                                   0.2857143
intention_min                               0.3076923
intention_strength                          0.4666667
study_knee                                  0.3703704
pain_T1                                     0.2857143
IPAQ_MET_MIN_WEEK_T1_trunc                  0.4313725
age                                         0.2941176
gender                                      0.2727273
BMI                                         0.2647059
income                                      0.2857143
educ                                        0.3243243
location                                    0.2727273
pain_T2                                     0.0000000
IPAQ_MET_MIN_WEEK_T2_trunc                  0.0000000

p$rm/(p$rm+p$rr)
IPAQ_MET_MIN_WEEK_T2_trunc
d_eval                                      0.2554113
d_id                                        0.2554113
identified                                  0.2550607
amotivation                                 0.2550607
intrinsic                                   0.2550607
introjected                                 0.2550607
integrated                                  0.2550607
extrinsic                                   0.2550607
selfefficacy                                0.2550607
selfefficacy_walk                           0.2550607
instrumental                                0.2550607
affective                                   0.2550607
habit                                       0.2550607
intention                                   0.2327586
intention_min                               0.2246696
intention_strength                          0.2376682
study_knee                                  0.2300885
pain_T1                                     0.2327586
IPAQ_MET_MIN_WEEK_T1_trunc                  0.1980198
age                                         0.2054795
gender                                      0.2045455
BMI                                         0.2009132
income                                      0.2018349
educ                                        0.2037037
location                                    0.2045455
pain_T2                                     0.0800000
IPAQ_MET_MIN_WEEK_T2_trunc                  0.0000000

                            extrinsic selfefficacy selfefficacy_walk instrumental  affective      habit  intention
d_eval                     0.02597403   0.02597403        0.02597403   0.02597403 0.02597403 0.02597403 0.06493506
d_id                       0.02597403   0.02597403        0.02597403   0.02597403 0.02597403 0.02597403 0.06493506
identified                 0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
amotivation                0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
intrinsic                  0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
introjected                0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
integrated                 0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
extrinsic                  0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
selfefficacy               0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
selfefficacy_walk          0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
instrumental               0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
affective                  0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
habit                      0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.06072874
intention                  0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
intention_min              0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
intention_strength         0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
study_knee                 0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
pain_T1                    0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
IPAQ_MET_MIN_WEEK_T1_trunc 0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
age                        0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
gender                     0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
BMI                        0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
income                     0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
educ                       0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
location                   0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.00000000
pain_T2                    0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.03500000
IPAQ_MET_MIN_WEEK_T2_trunc 0.00000000   0.00000000        0.00000000   0.00000000 0.00000000 0.00000000 0.03260870


intention_min intention_strength study_knee    pain_T1 IPAQ_MET_MIN_WEEK_T1_trunc
d_eval                        0.08225108         0.09956710 0.07792208 0.06493506                 0.18181818
d_id                          0.08225108         0.09956710 0.07792208 0.06493506                 0.18181818
identified                    0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
amotivation                   0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
intrinsic                     0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
introjected                   0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
integrated                    0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
extrinsic                     0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
selfefficacy                  0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
selfefficacy_walk             0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
instrumental                  0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
affective                     0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
habit                         0.08097166         0.09716599 0.08502024 0.06072874                 0.18218623
intention                     0.02155172         0.03879310 0.02586207 0.00000000                 0.12931034
intention_min                 0.00000000         0.03964758 0.02643172 0.00000000                 0.12334802
intention_strength            0.02242152         0.00000000 0.02690583 0.00000000                 0.13004484
study_knee                    0.02212389         0.03982301 0.00000000 0.00000000                 0.12389381
pain_T1                       0.02155172         0.03879310 0.02586207 0.00000000                 0.12931034
IPAQ_MET_MIN_WEEK_T1_trunc    0.01485149         0.03960396 0.01980198 0.00000000                 0.00000000
age                           0.01826484         0.04109589 0.02283105 0.00000000                 0.09132420
gender                        0.01818182         0.04090909 0.02272727 0.00000000                 0.09090909
BMI                           0.01826484         0.04109589 0.02283105 0.00000000                 0.08675799
income                        0.01834862         0.04128440 0.02293578 0.00000000                 0.09174312
educ                          0.01851852         0.04166667 0.02314815 0.00000000                 0.09259259
location                      0.01818182         0.04090909 0.02272727 0.00000000                 0.09090909
pain_T2                       0.05500000         0.07500000 0.05500000 0.03500000                 0.12000000
IPAQ_MET_MIN_WEEK_T2_trunc    0.04347826         0.07608696 0.05434783 0.03260870                 0.11956522
                             age     gender         BMI      income       educ   location   pain_T2
d_eval                     0.112554113 0.10822511 0.112554113 0.116883117 0.12554113 0.10822511 0.1991342
d_id                       0.112554113 0.10822511 0.112554113 0.116883117 0.12554113 0.10822511 0.1991342
identified                 0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
amotivation                0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
intrinsic                  0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
introjected                0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
integrated                 0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
extrinsic                  0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
selfefficacy               0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
selfefficacy_walk          0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
instrumental               0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
affective                  0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
habit                      0.113360324 0.10931174 0.113360324 0.117408907 0.12550607 0.10931174 0.1902834
intention                  0.056034483 0.05172414 0.056034483 0.060344828 0.06896552 0.05172414 0.1681034
intention_min              0.052863436 0.04845815 0.052863436 0.057268722 0.06607930 0.04845815 0.1674009
intention_strength         0.058295964 0.05381166 0.058295964 0.062780269 0.07174888 0.05381166 0.1704036
study_knee                 0.053097345 0.04867257 0.053097345 0.057522124 0.06637168 0.04867257 0.1637168
pain_T1                    0.056034483 0.05172414 0.056034483 0.060344828 0.06896552 0.05172414 0.1681034
IPAQ_MET_MIN_WEEK_T1_trunc 0.014851485 0.00990099 0.009900990 0.019801980 0.02970297 0.00990099 0.1287129
age                        0.000000000 0.00000000 0.004566210 0.009132420 0.01826484 0.00000000 0.1369863
gender                     0.004545455 0.00000000 0.004545455 0.009090909 0.01818182 0.00000000 0.1363636
BMI                        0.004566210 0.00000000 0.000000000 0.009132420 0.01826484 0.00000000 0.1324201
income                     0.004587156 0.00000000 0.004587156 0.000000000 0.01376147 0.00000000 0.1376147
educ                       0.004629630 0.00000000 0.004629630 0.004629630 0.00000000 0.00000000 0.1388889
location                   0.004545455 0.00000000 0.004545455 0.009090909 0.01818182 0.00000000 0.1363636
pain_T2                    0.055000000 0.05000000 0.050000000 0.060000000 0.07000000 0.05000000 0.0000000
IPAQ_MET_MIN_WEEK_T2_trunc 0.054347826 0.04891304 0.048913043 0.054347826 0.06521739 0.04891304 0.0000000

                                 IPAQ_MET_MIN_WEEK_T2_trunc
d_eval                                      0.2554113
d_id                                        0.2554113
identified                                  0.2550607
amotivation                                 0.2550607
intrinsic                                   0.2550607
introjected                                 0.2550607
integrated                                  0.2550607
extrinsic                                   0.2550607
selfefficacy                                0.2550607
selfefficacy_walk                           0.2550607
instrumental                                0.2550607
affective                                   0.2550607
habit                                       0.2550607
intention                                   0.2327586
intention_min                               0.2246696
intention_strength                          0.2376682
study_knee                                  0.2300885
pain_T1                                     0.2327586
IPAQ_MET_MIN_WEEK_T1_trunc                  0.1980198
age                                         0.2054795
gender                                      0.2045455
BMI                                         0.2009132
income                                      0.2018349
educ                                        0.2037037
location                                    0.2045455
pain_T2                                     0.0800000
IPAQ_MET_MIN_WEEK_T2_trunc                  0.0000000

####????WHAT IS THIStempData$imp$pain.id


meth["BMI_derived"]<-"~I((data_miss$weight/data_miss$height/data_miss$height)*10000)"