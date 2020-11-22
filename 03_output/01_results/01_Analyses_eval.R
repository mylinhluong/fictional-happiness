#This script conducts analysis of 
#PREDICTOR: automatic evaluations (d_eval) on OUTCOME: physical activity (IPAQ_MET_MIN_WEEK_T2_trunc)
#w/ statistical significance set at p<0.05
#We conduct three-step multiple regression to identify the independent contributions of each non-conscious process at baseline on
#physical activity at one-week follow-up
#Step 1: IPAQ at baseline and each non-conscious process (automatic evaluations, automatic self-schema, habit automaticity)
#Step 2: + demographic covariates (age, gender, body mass index, income, education, location)
#Step 3: + reflective processes (decisional intention, intention strength, self-effacy for physical activity when experiencing pain
#while walking)
#Step 4: Moderation, including a mean-centered interaction term between pain and each non-conscious process

## linear regression for each imputed data set
#R will estimate our regression model separately for each imputed dataset, 1 though 155. 
#We then need to summarize or pool those estimates to get one overall set of parameter estimates.
# Then we will pool coefficients and standard errors across all 155 regression models
#and produce output parameter estimates

###############LOAD LIBRARIES###############
library(here)
library(mice)

###############IMPORT MULTIPLY IMPUTED DATA SET###############
imp<- readRDS(here("01_data","02_processed","01_imputed_data_long_post_asmids.rds"))

###############ANALYSIS OF IMPUTED DATA###############
############MAIN ANALYSES############
#Table 2 #
#Effect of automatic evaluations on physical activity at follow-up
#Step 1
##########MODEL-1-main##########
fit_m1_eval<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_eval))
m1_eval<-(summary(pool(fit_m1_eval),conf.int=TRUE))
m1_eval[-1] <- round(m1_eval[-1],2)
m1_eval
#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                (Intercept)  1828.01    554.90      3.29 126.14    0.00   729.90 2926.12
#2 IPAQ_MET_MIN_WEEK_T1_trunc     0.56      0.06      9.76 132.62    0.00     0.44    0.67
#3                     d_eval  -396.61    514.71     -0.77 108.43    0.44 -1416.81  623.59


round(pool.r.squared(fit_m1_eval,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN

round(pool.r.squared(fit_m1_eval),2)
#     est lo 95 hi 95 fmi
#R^2 0.39  0.27   0.5 NaN

#Step 2
##########MODEL-2-main##########
fit_m2_eval<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                                d_eval+age+gender+BMI_derived+income+educ_years+state))
m2_eval<-(summary(pool(fit_m2_eval),conf.int=TRUE))
m2_eval[-1] <- round(m2_eval[-1],2)
m2_eval

#                           term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  4345.81   2408.23      1.80 142.09    0.07  -414.77 9106.39
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.57      0.06      9.24 124.92    0.00     0.45    0.70
#3                      d_eval  -504.77    540.08     -0.93 104.93    0.35 -1575.64  566.11
#4                         age   -35.20     25.19     -1.40 128.05    0.16   -85.03   14.64
#5                     gender2  -144.69    459.70     -0.31 165.38    0.75 -1052.33  762.94
#6                 BMI_derived   -18.94     31.60     -0.60 164.79    0.55   -81.33   43.46
#7                      income    89.89    139.46      0.64 153.55    0.52  -185.63  365.40
#8                  educ_years    20.27     44.47      0.46 146.41    0.65   -67.62  108.16
#9                      state3 -1150.29   1025.81     -1.12 167.44    0.26 -3175.48  874.89
#10                     state4   -36.02    804.34     -0.04 173.99    0.96 -1623.54 1551.50
#11                     state5  -439.47    769.43     -0.57 173.38    0.57 -1958.12 1079.19
#12                     state6  -320.57    748.70     -0.43 170.06    0.67 -1798.51 1157.36
#13                     state7  -732.15   1244.11     -0.59 146.04    0.56 -3190.94 1726.63
#14                     state8  -185.27   1323.51     -0.14 158.02    0.89 -2799.31 2428.77
#15                    state10  -804.48   2450.63     -0.33 169.73    0.74 -5642.12 4033.17

round(pool.r.squared(fit_m2_eval,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.39  0.27  0.51 NaN

round(pool.r.squared(fit_m2_eval),2)
#     est lo 95 hi 95 fmi
#R^2 0.42   0.3  0.54 NaN

#STEP 3
##########MODEL-3-main##########
fit_m3_eval<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_eval+
                                age+gender+BMI_derived+income+educ_years+state+
                                intention_min+intention_strength+selfefficacy+selfefficacy_walk))

m3_eval<-(summary(pool(fit_m3_eval),conf.int=TRUE))
m3_eval[-1] <- round(m3_eval[-1],2)
m3_eval

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  3956.62   2621.75      1.51 145.09    0.13 -1225.14 9138.38
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.27 141.63    0.00     0.45    0.70
#3                      d_eval  -431.53    536.46     -0.80 105.65    0.42 -1495.16  632.11
#4                         age   -37.63     25.01     -1.50 130.03    0.13   -87.10   11.84
#5                     gender2  -119.91    464.72     -0.26 158.52    0.80 -1037.75  797.93
#6                 BMI_derived   -23.25     32.21     -0.72 166.06    0.47   -86.86   40.35
#7                      income    87.04    140.29      0.62 148.03    0.54  -190.18  364.27
#8                  educ_years    24.09     44.40      0.54 145.71    0.59   -63.67  111.85
#9                      state3 -1191.04   1036.09     -1.15 160.93    0.25 -3237.12  855.04
#10                     state4     3.78    804.66      0.00 169.79    1.00 -1584.65 1592.21
#11                     state5  -510.00    774.06     -0.66 168.10    0.51 -2038.13 1018.13
#12                     state6  -318.10    743.87     -0.43 167.34    0.67 -1786.68 1150.48
#13                     state7   -28.36   1241.15     -0.02 161.24    0.98 -2479.36 2422.63
#14                     state8  -270.25   1319.65     -0.20 154.10    0.84 -2877.19 2336.68
#15                    state10 -1007.51   2434.90     -0.41 167.78    0.68 -5814.51 3799.49
#16              intention_min    -0.74      0.59     -1.25  91.69    0.22    -1.91    0.44
#17         intention_strength   206.30    270.33      0.76 148.46    0.45  -327.89  740.49
#18               selfefficacy  -419.13    413.13     -1.01 147.19    0.31 -1235.57  397.31
#19          selfefficacy_walk   422.40    300.32      1.41 173.82    0.16  -170.35 1015.14

round(pool.r.squared(fit_m3_eval,adjusted=TRUE),2)
#       est lo 95 hi 95 fmi
#adj R^2 0.4  0.28  0.52 NaN

round(pool.r.squared(fit_m3_eval),2)
#     est lo 95 hi 95 fmi
#R^2 0.44  0.32  0.55 NaN

############MODERATION ANALYSES############
##########MODEL-1-moderation##########
fit_mod1_eval<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                              d_eval+
                              pain_T2+pain.eval))
mod1_eval<-(summary(pool(fit_mod1_eval),conf.int=TRUE))
mod1_eval[-1] <- round(mod1_eval[-1],2)
mod1_eval
#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                (Intercept)  2239.61    741.13      3.02 152.26    0.00   775.38 3703.84
#2 IPAQ_MET_MIN_WEEK_T1_trunc     0.56      0.06      9.76 132.08    0.00     0.44    0.67
#3                     d_eval  -444.65    513.87     -0.87 113.44    0.39 -1462.67  573.37
#4                    pain_T2   -79.98     98.10     -0.82 162.20    0.42  -273.70  113.73
#5                  pain.eval   -32.38    219.84     -0.15 153.43    0.88  -466.69  401.93

round(pool.r.squared(fit_mod1_eval,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN

round(pool.r.squared(fit_mod1_eval),2)
#     est lo 95 hi 95 fmi
#R^2 0.39  0.27  0.51 NaN

##########MODEL-2-moderation##########
fit_mod2_eval<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                            d_eval+pain_T2+pain.eval+age+gender+BMI_derived+income+educ_years+state))
mod2_eval<-(summary(pool(fit_mod2_eval),conf.int=TRUE))
mod2_eval[-1] <- round(mod2_eval[-1],2)
mod2_eval

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  4665.43   2425.06      1.92 146.19    0.06  -127.27 9458.13
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.25 124.73    0.00     0.45    0.70
#3                      d_eval  -539.13    539.84     -1.00 108.38    0.32 -1609.14  530.89
#4                     pain_T2   -75.98    103.88     -0.73 152.72    0.47  -281.21  129.24
#5                   pain.eval    -8.71    224.23     -0.04 146.77    0.97  -451.85  434.44
#6                         age   -36.08     25.09     -1.44 129.51    0.15   -85.72   13.56
#7                     gender2  -139.66    463.43     -0.30 162.55    0.76 -1054.77  775.46
#8                 BMI_derived   -14.18     32.61     -0.43 159.52    0.66   -78.58   50.22
#9                      income    92.00    139.58      0.66 153.75    0.51  -183.73  367.74
#10                 educ_years    16.73     44.98      0.37 144.08    0.71   -72.19  105.64
#11                     state3 -1070.18   1031.47     -1.04 166.85    0.30 -3106.60  966.25
#12                     state4   -47.99    804.79     -0.06 173.12    0.95 -1636.46 1540.48
#13                     state5  -440.34    771.11     -0.57 171.78    0.57 -1962.41 1081.74
#14                     state6  -286.85    748.23     -0.38 170.49    0.70 -1763.84 1190.15
#15                     state7  -796.28   1256.78     -0.63 144.96    0.53 -3280.26 1687.69
#16                     state8  -120.68   1325.91     -0.09 157.85    0.93 -2739.49 2498.13
#17                    state10  -726.19   2464.04     -0.29 167.35    0.77 -5590.80 4138.41

round(pool.r.squared(fit_mod2_eval,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.39  0.26  0.51 NaN

round(pool.r.squared(fit_mod2_eval),2)
#     est lo 95 hi 95 fmi
#R^2 0.43  0.31  0.54 NaN

##########MODEL-3-moderation##########
fit_mod3_eval<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_eval+
                              pain_T2+pain.eval+
                            age+gender+BMI_derived+income+educ_years+state+
                            intention_min+intention_strength+selfefficacy+selfefficacy_walk))

mod3_eval<-(summary(pool(fit_mod3_eval),conf.int=TRUE))
mod3_eval[-1] <- round(mod3_eval[-1],2)
mod3_eval

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  4316.62   2649.80      1.63 147.58    0.11  -919.83 9553.07
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.28 140.90    0.00     0.46    0.70
#3                      d_eval  -460.90    535.56     -0.86 109.41    0.39 -1522.32  600.53
#4                     pain_T2   -78.12    104.24     -0.75 148.34    0.45  -284.10  127.87
#5                   pain.eval   -25.24    222.21     -0.11 146.51    0.91  -464.39  413.92
#6                         age   -38.35     24.88     -1.54 131.76    0.13   -87.55   10.86
#7                     gender2  -110.99    469.02     -0.24 155.28    0.81 -1037.47  815.50
#8                 BMI_derived   -18.95     33.07     -0.57 160.97    0.57   -84.26   46.35
#9                      income    89.21    140.24      0.64 148.61    0.53  -187.92  366.33
#10                 educ_years    20.44     44.89      0.46 143.60    0.65   -68.29  109.17
#11                     state3 -1110.06   1041.69     -1.07 160.22    0.29 -3167.28  947.16
#12                     state4   -12.86    805.20     -0.02 168.86    0.99 -1602.41 1576.69
#13                     state5  -515.05    776.10     -0.66 166.31    0.51 -2047.33 1017.24
#14                     state6  -282.22    743.61     -0.38 167.62    0.70 -1750.27 1185.83
#15                     state7   -71.84   1255.41     -0.06 159.21    0.95 -2551.25 2407.57
#16                     state8  -198.56   1322.15     -0.15 153.96    0.88 -2810.46 2413.34
#17                    state10  -933.10   2447.65     -0.38 165.39    0.70 -5765.78 3899.57
#18              intention_min    -0.77      0.59     -1.29  91.41    0.20    -1.94    0.41
#19         intention_strength   198.08    270.37      0.73 148.53    0.46  -336.18  732.35
#20               selfefficacy  -407.89    413.69     -0.99 146.40    0.33 -1225.46  409.69
#21          selfefficacy_walk   411.85    301.08      1.37 172.72    0.17  -182.42 1006.12

round(pool.r.squared(fit_mod3_eval,adjusted=TRUE),2)
#        est lo 95 hi 95 fmi
#adj R^2 0.4  0.28  0.52 NaN

round(pool.r.squared(fit_mod3_eval),2)
#     est lo 95 hi 95 fmi
#R^2 0.45  0.33  0.56 NaN

#Rounding in mice
#https://stackoverflow.com/questions/61840259/round-function-no-longer-works-with-mice-output

