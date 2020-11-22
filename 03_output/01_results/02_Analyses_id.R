#This script conducts analysis of  
#PREDICTOR: automatic self-schema (d_id) on OUTCOME:physical activity (IPAQ_MET_MIN_WEEK_T2_trunc)
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

############MAIN ANALYSES############
#Table 3 #
#Effect of automatic self-schema on physical activity at follow-up
#Step 1
##########MODEL-1-main##########
fit_m1_id<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_id))
m1_id<-(summary(pool(fit_m1_id),conf.int=TRUE))
m1_id[-1] <- round(m1_id[-1],2)
m1_id
#                        term estimate std.error statistic     df p.value   2.5 %  97.5 %
#1                (Intercept)  1483.46    347.06      4.27 151.78    0.00  797.77 2169.14
#2 IPAQ_MET_MIN_WEEK_T1_trunc     0.56      0.06      9.73 131.42    0.00    0.45    0.67
#3                       d_id    90.28    407.15      0.22 137.30    0.82 -714.82  895.38


round(pool.r.squared(fit_m1_id,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN


round(pool.r.squared(fit_m1_id),2)
#     est lo 95 hi 95 fmi
#R^2 0.39  0.27   0.5 NaN

#Step 2
##########MODEL-2-main##########
fit_m2_id<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                            d_id+age+gender+BMI_derived+income+educ_years+state))
m2_id<-(summary(pool(fit_m2_id),conf.int=TRUE))
m2_id[-1] <- round(m2_id[-1],2)
m2_id

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  3853.00   2447.14      1.57 137.65    0.12  -985.84 8691.84
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.17 124.51    0.00     0.45    0.70
#3                        d_id    62.14    428.79      0.14 135.00    0.88  -785.86  910.15
#4                         age   -36.48     25.36     -1.44 126.71    0.15   -86.67   13.71
#5                     gender2  -115.62    459.31     -0.25 166.95    0.80 -1022.42  791.18
#6                 BMI_derived   -16.82     32.59     -0.52 162.66    0.61   -81.17   47.52
#7                      income    94.69    140.57      0.67 153.02    0.50  -183.02  372.40
#8                  educ_years    13.76     44.59      0.31 145.14    0.76   -74.37  101.89
#9                      state3  -931.21   1029.22     -0.90 163.40    0.37 -2963.49 1101.08
#10                     state4    94.87    810.66      0.12 173.07    0.91 -1505.19 1694.92
#11                     state5  -331.31    778.69     -0.43 173.41    0.67 -1868.24 1205.61
#12                     state6  -121.06    749.66     -0.16 171.95    0.87 -1600.77 1358.65
#13                     state7  -507.70   1229.25     -0.41 152.39    0.68 -2936.27 1920.87
#14                     state8  -140.71   1336.13     -0.11 156.40    0.92 -2779.90 2498.48
#15                    state10  -728.68   2456.76     -0.30 170.28    0.77 -5578.30 4120.94

round(pool.r.squared(fit_m2_id,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN

round(pool.r.squared(fit_m2_id),2)
#      est lo 95 hi 95 fmi
#R^2 0.42   0.3  0.53 NaN

#STEP 3
##########MODEL-3-main##########
fit_m3_id<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_id+
                            age+gender+BMI_derived+income+educ_years+state+
                            intention_min+intention_strength+selfefficacy+selfefficacy_walk))

m3_id<-(summary(pool(fit_m3_id),conf.int=TRUE))
m3_id[-1] <- round(m3_id[-1],2)
m3_id

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  3690.37   2644.47      1.40 142.28    0.17 -1537.16 8917.91
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.21 141.63    0.00     0.46    0.70
#3                        d_id    11.98    427.20      0.03 136.46    0.98  -832.81  856.77
#4                         age   -38.72     25.21     -1.54 128.24    0.13   -88.59   11.16
#5                     gender2   -88.01    463.39     -0.19 159.96    0.85 -1003.16  827.14
#6                 BMI_derived   -22.99     32.98     -0.70 164.07    0.49   -88.11   42.13
#7                      income    88.80    141.20      0.63 147.53    0.53  -190.22  367.83
#8                  educ_years    19.37     44.49      0.44 144.58    0.66   -68.55  107.30
#9                      state3 -1033.70   1040.62     -0.99 157.04    0.32 -3089.12 1021.72
#10                     state4    96.86    810.00      0.12 168.66    0.90 -1502.17 1695.90
#11                     state5  -444.41    782.97     -0.57 167.77    0.57 -1990.14 1101.33
#12                     state6  -169.70    746.08     -0.23 168.27    0.82 -1642.58 1303.17
#13                     state7   158.12   1221.65      0.13 167.60    0.90 -2253.68 2569.92
#14                     state8  -239.56   1331.16     -0.18 152.56    0.86 -2869.43 2390.32
#15                    state10  -956.07   2443.36     -0.39 167.56    0.70 -5779.80 3867.66
#16              intention_min    -0.77      0.59     -1.30  91.31    0.20    -1.95    0.41
#17         intention_strength   191.45    271.19      0.71 148.43    0.48  -344.45  727.35
#18               selfefficacy  -447.79    414.76     -1.08 146.85    0.28 -1267.47  371.88
#19          selfefficacy_walk   444.34    303.95      1.46 168.83    0.15  -155.69 1044.36

round(pool.r.squared(fit_m3_id,adjusted=TRUE),2)
#        est lo 95 hi 95 fmi
#adj R^2 0.4  0.27  0.51 NaN

round(pool.r.squared(fit_m3_id),2)
#     est lo 95 hi 95 fmi
#R^2 0.44  0.32  0.55 NaN

############MODERATION ANALYSES############
##########MODEL-1-moderation##########
fit_mod1_id<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                              d_id+
                              pain_T2+pain.id))
mod1_id<-(summary(pool(fit_mod1_id),conf.int=TRUE))
mod1_id[-1] <- round(mod1_id[-1],2)
mod1_id
#                        term estimate std.error statistic     df p.value   2.5 %  97.5 %
#1                (Intercept)  1767.07    560.41      3.15 168.97    0.00  660.76 2873.38
#2 IPAQ_MET_MIN_WEEK_T1_trunc     0.56      0.06      9.75 130.77    0.00    0.45    0.67
#3                       d_id    58.08    409.54      0.14 136.76    0.89 -751.78  867.93
#4                    pain_T2   -60.01     97.93     -0.61 158.99    0.54 -253.42  133.40
#5                    pain.id   105.48    185.41      0.57 161.97    0.57 -260.65  471.61

round(pool.r.squared(fit_mod1_id,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN

round(pool.r.squared(fit_mod1_id),2)
#     est lo 95 hi 95 fmi
#R^2 0.39  0.27  0.51 NaN

##########MODEL-2-moderation##########
fit_mod2_id<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                              d_id+pain_T2+pain.id+
                            age+gender+BMI_derived+income+educ_years+state))
mod2_id<-(summary(pool(fit_mod2_id),conf.int=TRUE))
mod2_id[-1] <- round(mod2_id[-1],2)
mod2_id

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  4118.62   2455.64      1.68 140.91    0.10  -736.04 8973.28
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.22 124.21    0.00     0.45    0.70
#3                        d_id    31.95    432.08      0.07 133.49    0.94  -822.67  886.56
#4                     pain_T2   -57.53    104.02     -0.55 150.67    0.58  -263.06  148.00
#5                     pain.id   143.92    189.32      0.76 153.24    0.45  -230.10  517.93
#6                         age   -37.79     25.28     -1.50 127.86    0.14   -87.80   12.22
#7                     gender2  -136.22    460.95     -0.30 165.57    0.77 -1046.32  773.87
#8                 BMI_derived   -13.25     33.60     -0.39 156.94    0.69   -79.63   53.12
#9                      income   100.98    140.66      0.72 152.22    0.47  -176.93  378.88
#10                 educ_years    12.25     45.20      0.27 142.81    0.79   -77.10  101.59
#11                     state3  -883.46   1034.94     -0.85 163.36    0.39 -2927.04 1160.12
#12                     state4    75.54    809.19      0.09 172.88    0.93 -1521.63 1672.71
#13                     state5  -366.34    777.75     -0.47 173.86    0.64 -1901.38 1168.70
#14                     state6  -142.90    753.40     -0.19 172.40    0.85 -1629.97 1344.17
#15                     state7  -610.45   1232.23     -0.50 152.05    0.62 -3044.95 1824.04
#16                     state8  -168.51   1344.31     -0.13 155.18    0.90 -2824.02 2487.00
#17                    state10  -704.52   2463.68     -0.29 168.72    0.78 -5568.13 4159.08

round(pool.r.squared(fit_mod2_id,adjusted=TRUE),2)
#          est lo 95 hi 95 fmi
#adj R^2 0.39  0.26   0.5 NaN

round(pool.r.squared(fit_mod2_id),2)
#      est lo 95 hi 95 fmi
#R^2 0.42   0.3  0.54 NaN

##########MODEL-3-moderation##########
fit_mod3_id<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+d_id+
                              pain_T2+pain.id+
                              age+gender+BMI_derived+income+educ_years+state+
                              intention_min+intention_strength+selfefficacy+selfefficacy_walk))

mod3_id<-(summary(pool(fit_mod3_id),conf.int=TRUE))
mod3_id[-1] <- round(mod3_id[-1],2)
mod3_id

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  4051.16   2660.14      1.52 144.91    0.13 -1206.53 9308.86
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.30 141.41    0.00     0.46    0.71
#3                        d_id   -20.72    429.83     -0.05 134.97    0.96  -870.79  829.35
#4                     pain_T2   -61.16    104.40     -0.59 145.85    0.56  -267.48  145.17
#5                     pain.id   169.42    186.37      0.91 154.80    0.36  -198.74  537.58
#6                         age   -39.93     25.05     -1.59 129.77    0.11   -89.50    9.64
#7                     gender2  -108.72    464.42     -0.23 158.71    0.82 -1025.96  808.52
#8                 BMI_derived   -20.12     33.81     -0.60 158.64    0.55   -86.89   46.65
#9                      income    94.80    141.11      0.67 146.80    0.50  -184.08  373.67
#10                 educ_years    18.32     45.09      0.41 142.10    0.69   -70.81  107.46
#11                     state3  -994.65   1045.37     -0.95 156.91    0.34 -3059.46 1070.15
#12                     state4    67.80    808.06      0.08 168.39    0.93 -1527.44 1663.04
#13                     state5  -494.92    781.40     -0.63 168.37    0.53 -2037.53 1047.68
#14                     state6  -201.52    749.61     -0.27 168.56    0.79 -1681.36 1278.32
#15                     state7    65.96   1223.71      0.05 166.40    0.96 -2350.04 2481.96
#16                     state8  -279.19   1338.38     -0.21 151.47    0.84 -2923.48 2365.11
#17                    state10  -952.11   2448.33     -0.39 165.95    0.70 -5786.00 3881.78
#18              intention_min    -0.81      0.60     -1.36  90.68    0.18    -1.99    0.37
#19         intention_strength   189.79    270.97      0.70 147.86    0.48  -345.68  725.26
#20               selfefficacy  -464.47    414.54     -1.12 146.66    0.26 -1283.71  354.76
#21          selfefficacy_walk   445.18    303.60      1.47 168.37    0.14  -154.18 1044.54

round(pool.r.squared(fit_mod3_id,adjusted=TRUE),2)
#              est     lo 95     hi 95 fmi
#adj R^2 0.3976794 0.2762913 0.5136339 NaN

round(pool.r.squared(fit_mod3_id),2)
#        est lo 95 hi 95 fmi
#adj R^2 0.4  0.28  0.51 NaN

#Rounding in mice
#https://stackoverflow.com/questions/61840259/round-function-no-longer-works-with-mice-output

