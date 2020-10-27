#This script visually tests key assumptions of linear relationships to determine if any transformations are needed
#using standard diagnostic plots
#Key assumptions (affiliated plots) are:
#1)Linearity of the relationship between y and its explanatory variables (Residuals vs Fitted)
#2)Independence of variables where explanatory variables are not highly correlated w/ each other
#3)Normal distribution of residuals (Normal Q-Q)
#4)Homoscedasticity of equal variance of residuals (Scale-Location)

#bottomline: There isn't a clear rationale for a transformation. It *is* surprising that the d_id (self-schema)
#data appear in a somewhat negative relationship. Also surprising that habit does not appear to be correlated 
#with T2 physical activity...
#https://lesslikely.com/statistics/statistics-quality-control/
#in next iteration, might save assumption plots to file so we don't have to keep running them


###LOAD LIBRARIES
library(here)
library(dplyr)
library(psych)
library(knitr) #kable function
library(gvlma) #global validation of linear model assumptions
library(Hmisc) #correlations/covariances

options(scipen=999)

#Create a .txt file within the errors folder
inspection_04 <- file(here("02_scripts","Errors", "04_inspection.txt"), open = "wt")
sink(inspection_04, type = "message")

###LOAD FUNCTIONS
#function to display corr and significance
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


###import data
data<- readRDS(here("01_data","02_processed","complete_data_processed.rds"))

#The regression model is linear in parameters
#T2 Physical activity MET-minutes/week = 
          #β0 + 
          #β1 (T1 Physical activity MET-Minutes/week i) + 
          #β2(Automatic evaluations of physical activity i) + 
          #e

#Key assumptions are
#1)Linearity of the relationship between y and its explanatory variables (Residuals vs Fitted)
#2)Independence of variables where explanatory variables are not highly correlated w/ each other
#3)Normal distribution of residuals (Normal Q-Q)
#4)Homoscedasticity of equal variance of residuals (Scale-Location)


##### Assumption 1: linearity of relationships (Residuals vs Fitted)
#determine whether the predicted variable y is correlated with x explanatory variables.
#test to assess if the residuals appear to form an equal spread around 
#the horizontal line without distinct patterns

##### Assumption 2: Independence of Variables (Correlation)
#are explanatory variables used independent of one another
#is there collinearity between variables by testing these against one another

##### Assumption 3: Normal Distribution of Residuals (Normal Q-Q)
#do the residual error terms meet the assumptions of normal distribution? 
#make sure there are no other significant relationship that could be explaining the variance
#that have not been taken into acct in the linear regression

##### Assumption 4: Homoscedasticity or Equal Variance of Variables (Scale-Location)
#are error terms the same across all values of the independent variable.
#look for a constant spread of the residuals


#-----------------------------
###AUTOMATIC EVALUATIONS###

#examine the general relationship between IV and DV
par(mfrow=c(1,2))
plot(IPAQ_MET_MIN_WEEK_T2_trunc~+ IPAQ_MET_MIN_WEEK_T1_trunc + d_eval,data)
#looks like there is a linear relationship between T1 IPAQ and T2 IPAQ (makes sense)
#unclear what relationship there is between T2 & automatic evals, but it doesn't look like it is taking
#some other form

par(mfrow=c(1,1))
lm1_eval<-lm(IPAQ_MET_MIN_WEEK_T2_trunc~ + IPAQ_MET_MIN_WEEK_T1_trunc +d_eval,data)
plot(lm1_eval)

#Linearity of relationship: Residuals to Fitted Line, for automatic evaluations
#There appears to be a linear relationship between the fitted line and the residual value 
#creating a mostly horizontal line in the representation of their relationship, however,
#there appears to be one residual that affects this

##Normal distibution of residuals: Normal Q-Q plot for automatic evaluations
#Normal Q-Q appears to also be along this line however some substantial deviation
#near the tails suggests that the residuals might not be as close to a normal distribution.

##Homoscedasticity of equal variance of residuals: Scale-Location automatic evaluations
#Residuals appear unequal in variance with as the spread of residuals is more dense on the left
#and less dense on the right, red trend line is not straight

##Residuals vs Leverage
#Leverage is how much each data point influences the regression
#Cook's distance reflects how much fitted values would change if a point was deleted
#It appears that there is one point that might be affecting the regression model
influence.measures(lm1_eval)

##The mean of residuals
mean(lm1_eval$residuals)
#[1] -0.00000000000005338226
kable(resid(lm1_eval)[1:5]) #print first 5 residual values
#|   |          x|
#  |:--|----------:|
#  |2  |  4811.6119|
#  |3  |  -251.9883|
#  |4  | -1033.1146|
#  |5  |  -448.2514|
#  |6  | -2463.1953|

x=residuals(lm1_eval)
describe(x, type=2)
#   vars   n mean      sd  median trimmed     mad      min     max    range skew kurtosis     se
#X1    1 154    0 2557.93 -398.38  -69.75 1732.02 -8223.45 7668.08 15891.53  0.3     1.24 206.12

#x 
#n              missing             distinct                 Info                 Mean                  Gmd 
#154                    0                  154                    1 -0.00000000000005334                 2766 
#.05                  .10                  .25                  .50                  .75                  .90 
#-3874.2              -2541.2              -1307.2               -398.4               1451.9               3170.2 
#.95 
#4763.2 

#lowest : -8223.453 -5975.179 -5821.653 -5568.046 -5542.052, highest:  5793.692  5953.191  6889.305  7390.800  7668.080

summary(lm1_eval)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8223.5 -1307.2  -398.4  1451.9  7668.1 

#Coefficients:
#                           Estimate Std. Error t value             Pr(>|t|)    
#(Intercept)                1724.4312   534.0116   3.229              0.00152 ** 
#  IPAQ_MET_MIN_WEEK_T1_trunc    0.5937     0.0538  11.037 < 0.0000000000000002 ***
#  d_eval                     -451.3322   481.0076  -0.938              0.34959    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2575 on 151 degrees of freedom
#(99 observations deleted due to missingness)
#Multiple R-squared:  0.447,	Adjusted R-squared:  0.4397 
#F-statistic: 61.03 on 2 and 151 DF,  p-value: < 0.00000000000000022


gvlma::gvlma(lm1_eval)
#Call:
#lm(formula = IPAQ_MET_MIN_WEEK_T2_trunc ~ +IPAQ_MET_MIN_WEEK_T1_trunc + 
#     d_eval, data = data)

#Coefficients:
#  (Intercept)  IPAQ_MET_MIN_WEEK_T1_trunc                      d_eval  
#1724.4312                      0.5937                   -451.3322  


#ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
#USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
#  Level of Significance =  0.05 

#Call:
#  gvlma::gvlma(x = lm1_eval) 

#Value  p-value                   Decision
#Global Stat        12.1727 0.016112 Assumptions NOT satisfied!
#Skewness            2.2070 0.137382    Assumptions acceptable.
#Kurtosis            8.6916 0.003197 Assumptions NOT satisfied!
#Link Function       0.2822 0.595278    Assumptions acceptable.
#Heteroscedasticity  0.9920 0.319265    Assumptions acceptable.

###AUTOMATIC SELF-SCHEMA###
par(mfrow=c(1,2))
plot(IPAQ_MET_MIN_WEEK_T2_trunc~+ IPAQ_MET_MIN_WEEK_T1_trunc + d_id,data)
#looks like there is a linear relationship between T1 IPAQ and T2 IPAQ (makes sense)
#unclear what relationship with id

par(mfrow=c(1,1))
lm1_id<-lm(IPAQ_MET_MIN_WEEK_T2_trunc~ + IPAQ_MET_MIN_WEEK_T1_trunc +d_id,data)
plot(lm1_id)

#Residuals to Fitted Line, for automatic self-schema
#There appears to be a linear relationship between the fitted line and the residual value 
#creating a mostly horizontal line in the representation of their relationship,
#However, there appears to be one residual that affects this

mean(lm1_id$residuals)
#[1] 0.0000000000001929164

#Normal distribution of residuals:Normal Q-Q plot for automatic self-schema
# plot appears to also be along line, some substantial deviation, especially at the ends
# suggests that the residuals are not as close to a normal distribution.

##Homoscedasticity of equal variance of residuals: Scale-Location automatic eval
#Residuals appear unequal in variance with as the spread of residuals is more dense on the left
#and less dense on the right


##Residuals vs Leverage
#Leverage is how much each data point influences the regression
#Cook's distance reflects how much fitted values would change if a point was deleted
#It appears that there is one point that might be affecting the regression model
influence.measures(lm1_id)


##The mean of residuals
x=residuals(lm1_id)
describe(x, type=2)
#   vars   n mean      sd  median trimmed     mad      min     max    range skew kurtosis     se
#X1    1 154    0 2565.34 -459.24  -82.46 1725.57 -7886.67 7707.16 15593.83 0.36     1.28 206.72

#x 
#  n            missing           distinct               Info               Mean                Gmd 
#154                  0                154                  1 0.0000000000001929               2766 
#.05                .10                .25                .50                .75                .90 
#-3844.6            -2663.0            -1263.1             -459.2             1399.6             3169.7 
#.95 
#4803.9 

#lowest : -7886.670 -6258.903 -5704.666 -5692.532 -5649.196, highest:  5987.827  6410.086  6936.496  7609.022  7707.157

summary(lm1_id)
#Call:
#  lm(formula = IPAQ_MET_MIN_WEEK_T2_trunc ~ +IPAQ_MET_MIN_WEEK_T1_trunc + 
#       d_id, data = data)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-7886.7 -1263.1  -459.2  1399.6  7707.2 

#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)    
#(Intercept)                1343.69763  349.08640   3.849             0.000175 ***
#  IPAQ_MET_MIN_WEEK_T1_trunc    0.59171    0.05391  10.976 < 0.0000000000000002 ***
#  d_id                         25.65578  411.27961   0.062             0.950342    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2582 on 151 degrees of freedom
#(99 observations deleted due to missingness)
#Multiple R-squared:  0.4438,	Adjusted R-squared:  0.4364 
#F-statistic: 60.24 on 2 and 151 DF,  p-value: < 0.00000000000000022


#Check assumptions automatically
gvlma::gvlma(lm1_id)
#Call:
#lm(formula = IPAQ_MET_MIN_WEEK_T2_trunc ~ +IPAQ_MET_MIN_WEEK_T1_trunc + 
#     d_id, data = data)

#Coefficients:
#  (Intercept)  IPAQ_MET_MIN_WEEK_T1_trunc                        d_id  
#1343.6976                      0.5917                     25.6558  


#ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
#USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
#  Level of Significance =  0.05 

#Call:
#  gvlma::gvlma(x = lm1_id) 

#                     Value   p-value         Decision
#Global Stat        13.7522 0.008129 Assumptions NOT satisfied!
#Skewness            3.2863 0.069859    Assumptions acceptable.
#Kurtosis            9.2400 0.002368 Assumptions NOT satisfied!
#Link Function       0.2570 0.612213    Assumptions acceptable.
#Heteroscedasticity  0.9689 0.324943    Assumptions acceptable.


###HABIT AUTOMATICITY
#Linearity: Residuals to Fitted Line, for habit automaticity
par(mfrow=c(1,2))
plot(IPAQ_MET_MIN_WEEK_T2_trunc~+ IPAQ_MET_MIN_WEEK_T1_trunc + habit,data)
##looks like there is a linear relationship between T1 IPAQ and T2 IPAQ (makes sense)
#it is not clear to me what the relationship is between habit and T2 IPAQ, could be somewhat positive?

par(mfrow=c(1,1))
lm1_habit<-lm(IPAQ_MET_MIN_WEEK_T2_trunc~ + IPAQ_MET_MIN_WEEK_T1_trunc +habit,data)
plot(lm1_habit)

#Linearity of relationship: Residuals to Fitted Line, for automatic evaluations
#There appears to be a mostly linear relationship between the fitted line and the residual value 
#creating a horizontal line in the representation of their relationship

#Normal distribution of residuals:Normal Q-Q plot for habit
# Some substantial deviation, especially at tails suggests that the residuals are not as close to a normal distribution.

#Homoscedasticity of equal variance of residuals: Scale-Location habit
#Residuals appear unequal in variance with as the spread of residuals is more dense on the left
#and less dense on the right

##Residuals vs Leverage
#Leverage is how much each data point influences the regression
#Cook's distance reflects how much fitted values would change if a point was deleted
#It appears that there is one point that might be affecting the regression model
#?124
influence.measures(lm1_habit)

##The mean of residuals
mean(lm1_habit$residuals)
#[1] -0.000000000000238554

x=residuals(lm1_habit)
describe(x, type=2)
#   vars   n mean      sd  median trimmed     mad      min     max    range skew kurtosis     se
#X1    1 162    0 2639.12 -485.78 -114.98 1762.41 -7792.69 7490.72 15283.41  0.4     0.92 207.35

#x 
#n             missing            distinct                Info                Mean                 Gmd 
#162                   0                 162                   1 -0.0000000000002386                2861 
#.05                 .10                 .25                 .50                 .75                 .90 
#-3857.8             -2642.5             -1391.5              -485.8              1354.8              3426.8 
#.95 
#5445.4 

#lowest : -7792.695 -6437.608 -5928.821 -5875.335 -5598.925, highest:  5877.407  6520.019  6706.046  7385.279  7490.715

summary(lm1_habit)
#Call:
#lm(formula = IPAQ_MET_MIN_WEEK_T2_trunc ~ +IPAQ_MET_MIN_WEEK_T1_trunc + 
#     habit, data = data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7792.7 -1391.5  -485.8  1354.8  7490.7 

#Coefficients:
#Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)                1090.2444   594.9486   1.833              0.0687 .  
#IPAQ_MET_MIN_WEEK_T1_trunc    0.5851     0.0570  10.263 <0.0000000000000002 ***
#  habit                        84.6938   127.9338   0.662              0.5089    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2656 on 159 degrees of freedom
#(91 observations deleted due to missingness)
#Multiple R-squared:  0.4331,	Adjusted R-squared:  0.4259 
#F-statistic: 60.73 on 2 and 159 DF,  p-value: < 0.00000000000000022

#Check assumptions automatically
gvlma::gvlma(lm1_habit)

#Call:
#lm(formula = IPAQ_MET_MIN_WEEK_T2_trunc ~ +IPAQ_MET_MIN_WEEK_T1_trunc + 
#     habit, data = data)

#Coefficients:
#(Intercept)  IPAQ_MET_MIN_WEEK_T1_trunc                       habit  
#1090.2444                      0.5851                     84.6938  


#ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
#USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
#  Level of Significance =  0.05 

#Call:
#  gvlma::gvlma(x = lm1_habit) 

#                       Value p-value                   Decision
#Global Stat        9.2333010 0.05553    Assumptions acceptable.
#Skewness           4.1944041 0.04056 Assumptions NOT satisfied!
# Kurtosis           4.9761305 0.02570 Assumptions NOT satisfied!
# Link Function      0.0625811 0.80246    Assumptions acceptable.
# Heteroscedasticity 0.0001854 0.98914    Assumptions acceptable.

##################
#CORRELATIONS    #
##################
data_corr<-select(data, eval=d_eval, 
                  id=d_id,
                  habit=habit,
                  intention=intention_strength,
                  painT1=pain_T1,
                  painT2=pain_T2,
                  IPAQT1=IPAQ_MET_MIN_WEEK_T1_trunc,
                  IPAQT2=IPAQ_MET_MIN_WEEK_T2_trunc,
                  age=age,
                  BMI=BMI_calc,
                  edu=educ_years,
                  se=selfefficacy,
                  se_walk=selfefficacy_walk)

summary(data_corr)
#      eval               id               habit         intention         painT1          painT2           IPAQT1     
#Min.   :-1.2172   Min.   :-1.28287   Min.   :1.000   Min.   :1.000   Min.   :0.000   Min.   : 0.000   Min.   :    0  
#1st Qu.: 0.6698   1st Qu.:-0.35339   1st Qu.:3.000   1st Qu.:4.000   1st Qu.:4.000   1st Qu.: 3.000   1st Qu.: 2202  
#Median : 0.9287   Median : 0.03326   Median :4.750   Median :5.000   Median :5.000   Median : 5.000   Median : 4373  
#Mean   : 0.8352   Mean   : 0.02792   Mean   :4.361   Mean   :4.327   Mean   :5.073   Mean   : 4.685   Mean   : 5036  
#3rd Qu.: 1.1076   3rd Qu.: 0.40393   3rd Qu.:5.625   3rd Qu.:5.000   3rd Qu.:6.000   3rd Qu.: 6.000   3rd Qu.: 6971  
#Max.   : 1.6318   Max.   : 1.18383   Max.   :7.000   Max.   :5.000   Max.   :9.000   Max.   :10.000   Max.   :21798  
#NA's   :22        NA's   :22         NA's   :6       NA's   :30      NA's   :21      NA's   :53       NA's   :51     
 
#    IPAQT2           age              BMI             edu              se           se_walk     
#Min.   :   33   Min.   : 46.00   Min.   :18.79   Min.   : 0.00   Min.   :1.000   Min.   :1.000  
#1st Qu.: 1894   1st Qu.: 56.00   1st Qu.:25.46   1st Qu.:12.00   1st Qu.:2.625   1st Qu.:2.000  
#Median : 3744   Median : 62.00   Median :29.38   Median :15.00   Median :3.000   Median :3.000  
#Mean   : 4491   Mean   : 62.38   Mean   :30.17   Mean   :14.84   Mean   :3.008   Mean   :2.704  
#3rd Qu.: 6328   3rd Qu.: 68.00   3rd Qu.:34.23   3rd Qu.:17.12   3rd Qu.:3.750   3rd Qu.:3.000  
#Max.   :16924   Max.   :114.00   Max.   :49.13   Max.   :35.00   Max.   :4.000   Max.   :4.000  
#NA's   :69      NA's   :34       NA's   :45      NA's   :37      NA's   :6       NA's   :6  

corr_matrix<-rcorr(as.matrix(data_corr),
      type="pearson")

corr_matrix
#corr_matrix
#          eval    id habit intention painT1 painT2 IPAQT1 IPAQT2   age   BMI   edu    se se_walk
#eval       1.00  0.29  0.11      0.10  -0.15  -0.12   0.00  -0.05  0.02 -0.07  0.14  0.08   -0.01
#id         0.29  1.00  0.19      0.13  -0.08  -0.06  -0.04  -0.01  0.00 -0.23  0.11  0.18    0.14
#habit      0.11  0.19  1.00      0.47  -0.07  -0.07   0.31   0.25  0.15 -0.40 -0.04  0.60    0.54
#intention  0.10  0.13  0.47      1.00  -0.05  -0.10   0.35   0.24  0.10 -0.25 -0.01  0.55    0.45
#painT1    -0.15 -0.08 -0.07     -0.05   1.00   0.64  -0.01  -0.06 -0.03  0.17 -0.14 -0.14   -0.10
#painT2    -0.12 -0.06 -0.07     -0.10   0.64   1.00  -0.03  -0.05 -0.09  0.22 -0.14 -0.11   -0.12
#IPAQT1     0.00 -0.04  0.31      0.35  -0.01  -0.03   1.00   0.66  0.04 -0.19 -0.19  0.28    0.26
#IPAQT2    -0.05 -0.01  0.25      0.24  -0.06  -0.05   0.66   1.00 -0.10 -0.20 -0.07  0.16    0.22
#age        0.02  0.00  0.15      0.10  -0.03  -0.09   0.04  -0.10  1.00 -0.14 -0.03  0.15    0.17
#BMI       -0.07 -0.23 -0.40     -0.25   0.17   0.22  -0.19  -0.20 -0.14  1.00 -0.08 -0.29   -0.27
#edu        0.14  0.11 -0.04     -0.01  -0.14  -0.14  -0.19  -0.07 -0.03 -0.08  1.00  0.08    0.00
#se         0.08  0.18  0.60      0.55  -0.14  -0.11   0.28   0.16  0.15 -0.29  0.08  1.00    0.71
#se_walk   -0.01  0.14  0.54      0.45  -0.10  -0.12   0.26   0.22  0.17 -0.27  0.00  0.71    1.00

#n
#          eval  id habit intention painT1 painT2 IPAQT1 IPAQT2 age BMI edu  se se_walk
#eval       231 231   225       208    216    185    189    172 205 195 202 225     225
#id         231 231   225       208    216    185    189    172 205 195 202 225     225
#habit      225 225   247       223    232    200    202    184 219 208 216 247     247
#intention  208 208   223       223    223    185    194    170 210 199 207 223     223
#painT1     216 216   232       223    232    193    202    178 219 208 216 232     232
#painT2     185 185   200       185    193    200    176    184 189 182 186 200     200
#IPAQT1     189 189   202       194    202    176    202    162 199 191 196 202     202
#IPAQT2     172 172   184       170    178    184    162    184 174 167 172 184     184
#age        205 205   219       210    219    189    199    174 219 207 215 219     219
#BMI        195 195   208       199    208    182    191    167 207 208 204 208     208
#edu        202 202   216       207    216    186    196    172 215 204 216 216     216
#se         225 225   247       223    232    200    202    184 219 208 216 247     247
#se_walk    225 225   247       223    232    200    202    184 219 208 216 247     247

#P
#        eval   id     habit  intention painT1 painT2 IPAQT1 IPAQT2 age    BMI    edu    se     se_walk
#eval             0.0000 0.0914 0.1585    0.0324 0.1142 0.9744 0.5397 0.8286 0.3587 0.0481 0.2545 0.8844 
#id        0.0000        0.0042 0.0541    0.2532 0.4014 0.6193 0.9471 0.9865 0.0014 0.1095 0.0058 0.0385 
#habit     0.0914 0.0042        0.0000    0.2892 0.3460 0.0000 0.0006 0.0308 0.0000 0.5457 0.0000 0.0000 
#intention 0.1585 0.0541 0.0000           0.4219 0.1617 0.0000 0.0015 0.1408 0.0004 0.9188 0.0000 0.0000 
#painT1    0.0324 0.2532 0.2892 0.4219           0.0000 0.8656 0.4164 0.6417 0.0120 0.0422 0.0389 0.1164 
#painT2    0.1142 0.4014 0.3460 0.1617    0.0000        0.6654 0.5079 0.2216 0.0034 0.0607 0.1072 0.0959 
#IPAQT1    0.9744 0.6193 0.0000 0.0000    0.8656 0.6654        0.0000 0.5341 0.0093 0.0062 0.0000 0.0002 
#IPAQT2    0.5397 0.9471 0.0006 0.0015    0.4164 0.5079 0.0000        0.2015 0.0104 0.3286 0.0259 0.0031 
#age       0.8286 0.9865 0.0308 0.1408    0.6417 0.2216 0.5341 0.2015        0.0392 0.6779 0.0293 0.0122 
#BMI       0.3587 0.0014 0.0000 0.0004    0.0120 0.0034 0.0093 0.0104 0.0392        0.2752 0.0000 0.0000 
#edu       0.0481 0.1095 0.5457 0.9188    0.0422 0.0607 0.0062 0.3286 0.6779 0.2752        0.2557 0.9701 
#se        0.2545 0.0058 0.0000 0.0000    0.0389 0.1072 0.0000 0.0259 0.0293 0.0000 0.2557        0.0000 
#se_walk   0.8844 0.0385 0.0000 0.0000    0.1164 0.0959 0.0002 0.0031 0.0122 0.0000 0.9701 0.0000        

flattenCorrMatrix(corr_matrix$r, corr_matrix$P)
#          row    column          cor                      p
#1       eval        id  0.293020620 0.00000592135763488599
#2       eval     habit  0.112822286 0.09135308909927375431
#3         id     habit  0.190022970 0.00422868552480393234
#4       eval intention  0.098135714 0.15847755954354703078
#5         id intention  0.133764755 0.05407662635119181438
#6      habit intention  0.472340204 0.00000000000008570922
#7       eval    painT1 -0.145663437 0.03237007985543627342
#8         id    painT1 -0.078085780 0.25315964447737338006
#9      habit    painT1 -0.069883346 0.28915437115633269016
#10 intention    painT1 -0.054048959 0.42187392995883321589
#11      eval    painT2 -0.116533175 0.11418331287399841045
#12        id    painT2 -0.062048733 0.40144536933935182077
#13     habit    painT2 -0.066980131 0.34600656702414456234
#14 intention    painT2 -0.103308881 0.16170028113066892139
#15    painT1    painT2  0.644027862 0.00000000000000000000
#16      eval    IPAQT1 -0.002350447 0.97439311553131391364
#17        id    IPAQT1 -0.036373959 0.61925508018220210005
#18     habit    IPAQT1  0.310777833 0.00000674620662488579
#19 intention    IPAQT1  0.350541878 0.00000054191712717255
#20    painT1    IPAQT1 -0.011981050 0.86561412753055777536
#21    painT2    IPAQT1 -0.032827855 0.66536057351442456920
#22      eval    IPAQT2 -0.047072910 0.53974819251365224737
#23        id    IPAQT2 -0.005091445 0.94714916985798591753
#24     habit    IPAQT2  0.249270068 0.00064430152928007800
#25 intention    IPAQT2  0.242163355 0.00146453348524699933
#26    painT1    IPAQT2 -0.061290717 0.41637658242768571526
#27    painT2    IPAQT2 -0.049114665 0.50791785347025752984
#28    IPAQT1    IPAQT2  0.656893602 0.00000000000000000000
#29      eval       age  0.015211658 0.82861474314096850158
#30        id       age  0.001192224 0.98646399730031375519
#31     habit       age  0.146014587 0.03076936989640621789
#32 intention       age  0.101983852 0.14077575757015470259
#33    painT1       age -0.031613109 0.64173790165226285431
#34    painT2       age -0.089321149 0.22160888062496253426
#35    IPAQT1       age  0.044330626 0.53412163067661566629
#36    IPAQT2       age -0.097308592 0.20147152600060902472
#37      eval       BMI -0.066086918 0.35865850625447026623
#38        id       BMI -0.226811422 0.00142991096705458176
#39     habit       BMI -0.403409765 0.00000000152484469496
#40 intention       BMI -0.249964719 0.00036991647994888410
#41    painT1       BMI  0.173972391 0.01196620322037533768
#42    painT2       BMI  0.216191331 0.00337621701093659965
#43    IPAQT1       BMI -0.187678744 0.00932537419933021638
#44    IPAQT2       BMI -0.197823821 0.01038849225716642621
#45       age       BMI -0.143474288 0.03916795907690318046
#46      eval       edu  0.139269263 0.04807007320321887711
#47        id       edu  0.112962920 0.10945026519429967848
#48     habit       edu -0.041333248 0.54570694018478871357
#49 intention       edu -0.007130627 0.91877897294814636631
#50    painT1       edu -0.138337645 0.04224295735986771660
#51    painT2       edu -0.137817538 0.06067298042527591306
#52    IPAQT1       edu -0.194818323 0.00621383697015320813
#53    IPAQT2       edu -0.074927541 0.32863152874617429511
#54       age       edu -0.028481988 0.67793869224094116532
#55       BMI       edu -0.076754064 0.27521065692257762336
#56      eval        se  0.076278401 0.25450964743821424818
#57        id        se  0.183301803 0.00582245027304018059
#58     habit        se  0.597320862 0.00000000000000000000
#59 intention        se  0.546588567 0.00000000000000000000
#60    painT1        se -0.135721296 0.03886184034754402994
#61    painT2        se -0.114244551 0.10722043118479551715
#62    IPAQT1        se  0.276719914 0.00006701252023666449
#63    IPAQT2        se  0.164253398 0.02587970209391432519
#64       age        se  0.147274055 0.02934063447961765547
#65       BMI        se -0.290246704 0.00002112281554422779
#66       edu        se  0.077669650 0.25570541239352317930
#67      eval   se_walk -0.009743921 0.88443597411549568221
#68        id   se_walk  0.138101415 0.03846060898113456972
#69     habit   se_walk  0.540134794 0.00000000000000000000
#70 intention   se_walk  0.446948735 0.00000000000238320474
#71    painT1   se_walk -0.103351532 0.11643657754074254740
#72    painT2   se_walk -0.118073062 0.09587910841416613650
#73    IPAQT1   se_walk  0.259804201 0.00018852330401575301
#74    IPAQT2   se_walk  0.216719037 0.00312784869789295428
#75       age   se_walk  0.169161061 0.01217181894614283166
#76       BMI   se_walk -0.268477252 0.00008820791376829007
#77       edu   se_walk  0.002565887 0.97009277416462524890
#78        se   se_walk  0.711394420 0.00000000000000000000



#Sources
#Visual Tests for the Key Assumptions of Multiple Linear Regression
#https://rpubs.com/ajdowny_student/300663
#https://rcompanion.org/handbook/I_01.html
#http://r-statistics.co/Assumptions-of-Linear-Regression.html
#https://www.statmethods.net/stats/rdiagnostics.html

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(inspection_04)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "04_inspection.txt"))