#This script conducts analysis of 
#PREDICTOR: habit automaticity (habit) on OUTCOME: physical activity (IPAQ_MET_MIN_WEEK_T2_trunc)
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
#Table 4 #
#Effect of habit automaticity on physical activity at follow-up
#Step 1
##########MODEL-1-main##########
fit_m1_habit<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+habit))
m1_habit<-(summary(pool(fit_m1_habit),conf.int=TRUE))
m1_habit[-1] <- round(m1_habit[-1],2)
m1_habit
#                        term estimate std.error statistic     df p.value   2.5 %  97.5 %
#1                (Intercept)  1066.74    543.14      1.96 177.69    0.05   -5.09 2138.57
#2 IPAQ_MET_MIN_WEEK_T1_trunc     0.54      0.06      8.97 126.23    0.00    0.42    0.66
#3                      habit   114.16    121.95      0.94 160.26    0.35 -126.68  355.00

round(pool.r.squared(fit_m1_habit,adjusted=TRUE),2)
#        est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN

round(pool.r.squared(fit_m1_habit),2)
#     est lo 95 hi 95 fmi
#R^2 0.39  0.27   0.5 NaN

#Step 2
##########MODEL-2-main##########
fit_m2_habit<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                            habit+age+gender+BMI_derived+income+educ_years+state))
m2_habit<-(summary(pool(fit_m2_habit),conf.int=TRUE))
m2_habit[-1] <- round(m2_habit[-1],2)
m2_habit

#                          term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  2980.20   2550.77      1.17 137.27    0.24 -2063.69 8024.08
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.56      0.06      8.74 121.39    0.00     0.43    0.69
#3                       habit   152.29    133.32      1.14 149.63    0.26  -111.15  415.72
#4                         age   -38.33     25.19     -1.52 128.27    0.13   -88.17   11.51
#5                     gender2  -176.71    461.24     -0.38 166.13    0.70 -1087.35  733.93
#6                 BMI_derived    -5.28     33.66     -0.16 161.06    0.88   -71.75   61.19
#7                      income   105.14    140.94      0.75 150.41    0.46  -173.33  383.62
#8                  educ_years    15.13     44.27      0.34 145.04    0.73   -72.38  102.63
#9                      state3  -911.68   1017.89     -0.90 164.48    0.37 -2921.50 1098.14
#10                     state4   151.96    803.87      0.19 172.53    0.85 -1434.72 1738.64
#11                     state5  -310.68    768.25     -0.40 172.45    0.69 -1827.07 1205.71
#12                     state6  -134.24    729.68     -0.18 172.44    0.85 -1574.51 1306.02
#13                     state7  -471.78   1209.11     -0.39 153.62    0.70 -2860.41 1916.85
#14                     state8  -267.29   1332.78     -0.20 156.31    0.84 -2899.88 2365.30
#15                    state10  -697.04   2451.63     -0.28 169.43    0.78 -5536.73 4142.64

round(pool.r.squared(fit_m2_habit,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.39  0.27  0.51 NaN

round(pool.r.squared(fit_m2_habit),2)
#     est lo 95 hi 95 fmi
#R^2 0.42   0.3  0.53 NaN

#STEP 3
##########MODEL-3-main##########
fit_m3_habit<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+habit+
                            age+gender+BMI_derived+income+educ_years+state+
                            intention_min+intention_strength+selfefficacy+selfefficacy_walk))

m3_habit<-(summary(pool(fit_m3_habit),conf.int=TRUE))
m3_habit[-1] <- round(m3_habit[-1],2)
m3_habit

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  3367.77   2637.84      1.28 142.09    0.20 -1846.71 8582.26
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.29 141.42    0.00     0.45    0.70
#3                       habit   198.17    156.21      1.27 158.73    0.21  -110.36  506.70
#4                         age   -38.88     25.01     -1.55 129.00    0.12   -88.37   10.61
#5                     gender2  -133.20    462.61     -0.29 159.92    0.77 -1046.81  780.40
#6                 BMI_derived   -13.32     33.32     -0.40 162.88    0.69   -79.11   52.48
#7                      income    94.55    140.95      0.67 145.86    0.50  -184.02  373.12
#8                  educ_years    23.94     44.38      0.54 144.01    0.59   -63.77  111.66
#9                      state3 -1072.15   1028.63     -1.04 158.47    0.30 -3103.74  959.45
#10                     state4   111.34    801.05      0.14 168.59    0.89 -1470.04 1692.73
#11                     state5  -465.01    771.48     -0.60 167.60    0.55 -1988.08 1058.06
#12                     state6  -188.12    725.77     -0.26 169.12    0.80 -1620.87 1244.62
#13                     state7   175.32   1207.38      0.15 167.78    0.88 -2208.30 2558.93
#14                     state8  -385.60   1327.28     -0.29 152.73    0.77 -3007.79 2236.59
#15                    state10  -939.98   2433.71     -0.39 167.27    0.70 -5744.72 3864.76
#16              intention_min    -0.81      0.60     -1.36  89.54    0.18    -2.00    0.37
#17         intention_strength   122.92    277.08      0.44 146.09    0.66  -424.69  670.52
#18               selfefficacy  -581.95    419.60     -1.39 152.99    0.17 -1410.89  247.00
#19          selfefficacy_walk   384.29    304.47      1.26 170.97    0.21  -216.71  985.29

round(pool.r.squared(fit_m3_habit,adjusted=TRUE),2)
#        est lo 95 hi 95 fmi
#adj R^2 0.4  0.28  0.52 NaN

round(pool.r.squared(fit_m3_habit),2)
#     est lo 95 hi 95 fmi
#R^2 0.44  0.33  0.55 NaN

############MODERATION ANALYSES############
##########MODEL-1-moderation##########
fit_mod1_habit<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                              habit+
                              pain_T2+pain.habit))
mod1_habit<-(summary(pool(fit_mod1_habit),conf.int=TRUE))
mod1_habit[-1] <- round(mod1_habit[-1],2)
mod1_habit
#                        term estimate std.error statistic     df p.value   2.5 %  97.5 %
#1                (Intercept)  1385.25    719.17      1.93 175.41    0.06  -34.08 2804.58
#2 IPAQ_MET_MIN_WEEK_T1_trunc     0.54      0.06      8.92 125.36    0.00    0.42    0.66
#3                      habit   109.18    122.08      0.89 160.20    0.37 -131.90  350.27
#4                    pain_T2   -61.35     97.66     -0.63 159.22    0.53 -254.22  131.53
#5                 pain.habit    25.11     56.79      0.44 176.62    0.66  -86.97  137.20

round(pool.r.squared(fit_mod1_habit,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.38  0.26   0.5 NaN

round(pool.r.squared(fit_mod1_habit),2)
#     est lo 95 hi 95 fmi
#R^2 0.39  0.27   0.5 NaN

##########MODEL-2-moderation##########
fit_mod2_habit<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                              habit+pain_T2+pain.habit
                             +age+gender+BMI_derived+income+educ_years+state))
mod2_habit<-(summary(pool(fit_mod2_habit),conf.int=TRUE))
mod2_habit[-1] <- round(mod2_habit[-1],2)
mod2_habit

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  3191.05   2572.42      1.24 138.76    0.22 -1895.16 8277.27
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.56      0.06      8.70 120.59    0.00     0.43    0.69
#3                       habit   154.42    133.54      1.16 148.85    0.25  -109.47  418.30
#4                     pain_T2   -65.81    103.69     -0.63 151.04    0.53  -270.67  139.05
#5                  pain.habit    20.27     58.04      0.35 166.47    0.73   -94.31  134.85
#6                         age   -38.91     25.15     -1.55 129.32    0.12   -88.67   10.84
#7                     gender2  -183.50    464.47     -0.40 163.90    0.69 -1100.63  733.62
#8                 BMI_derived    -0.05     34.81      0.00 156.34    1.00   -68.81   68.70
#9                      income   109.86    141.61      0.78 148.89    0.44  -169.97  389.68
#10                 educ_years    10.84     44.69      0.24 144.89    0.81   -77.49   99.17
#11                     state3  -826.85   1026.37     -0.81 163.63    0.42 -2853.48 1199.79
#12                     state4   145.76    803.37      0.18 172.32    0.86 -1439.95 1731.47
#13                     state5  -295.87    769.51     -0.38 171.35    0.70 -1814.81 1223.06
#14                     state6   -96.22    730.80     -0.13 172.70    0.90 -1538.66 1346.23
#15                     state7  -503.49   1213.68     -0.41 152.54    0.68 -2901.28 1894.30
#16                     state8  -247.91   1344.80     -0.18 154.54    0.85 -2904.47 2408.66
#17                    state10  -623.48   2457.89     -0.25 168.24    0.80 -5475.75 4228.79

round(pool.r.squared(fit_mod2_habit,adjusted=TRUE),2)
#         est lo 95 hi 95 fmi
#adj R^2 0.39  0.27   0.5 NaN

round(pool.r.squared(fit_mod2_habit),2)
#     est lo 95 hi 95 fmi
#R^2 0.43  0.31  0.54 NaN

##########MODEL-3-moderation##########
fit_mod3_habit<-with(imp, lm(IPAQ_MET_MIN_WEEK_T2_trunc~IPAQ_MET_MIN_WEEK_T1_trunc+
                              habit+pain_T2+pain.habit+
                              age+gender+BMI_derived+income+educ_years+state+
                              intention_min+intention_strength+selfefficacy+selfefficacy_walk))

mod3_habit<-(summary(pool(fit_mod3_habit),conf.int=TRUE))
mod3_habit[-1] <- round(mod3_habit[-1],2)
mod3_habit

#                         term estimate std.error statistic     df p.value    2.5 %  97.5 %
#1                 (Intercept)  3646.18   2663.62      1.37 143.70    0.17 -1618.75 8911.12
#2  IPAQ_MET_MIN_WEEK_T1_trunc     0.58      0.06      9.25 140.12    0.00     0.45    0.70
#3                       habit   204.95    156.56      1.31 157.82    0.19  -104.28  514.18
#4                     pain_T2   -76.59    104.05     -0.74 146.54    0.46  -282.23  129.05
#5                  pain.habit    24.48     57.80      0.42 163.24    0.67   -89.65  138.60
#6                         age   -39.42     24.91     -1.58 130.46    0.12   -88.69    9.85
#7                     gender2  -140.67    465.77     -0.30 157.54    0.76 -1060.62  779.29
#8                 BMI_derived    -7.44     34.42     -0.22 157.38    0.83   -75.43   60.54
#9                      income    99.68    141.65      0.70 144.09    0.48  -180.30  379.66
#10                 educ_years    19.10     44.71      0.43 144.03    0.67   -69.28  107.47
#11                     state3  -979.86   1036.20     -0.95 157.42    0.35 -3026.50 1066.79
#12                     state4   100.24    800.12      0.13 168.31    0.90 -1479.32 1679.79
#13                     state5  -452.89    772.71     -0.59 166.18    0.56 -1978.49 1072.70
#14                     state6  -145.78    726.38     -0.20 169.35    0.84 -1579.71 1288.15
#15                     state7   151.87   1212.31      0.13 165.87    0.90 -2241.68 2545.42
#16                     state8  -367.70   1337.60     -0.27 151.24    0.78 -3010.50 2275.11
#17                    state10  -864.36   2439.07     -0.35 165.89    0.72 -5679.98 3951.25
#18              intention_min    -0.84      0.60     -1.41  89.88    0.16    -2.02    0.34
#19         intention_strength   116.18    277.62      0.42 145.31    0.68  -432.52  664.88
#20               selfefficacy  -588.74    418.89     -1.41 153.45    0.16 -1416.27  238.78
#21          selfefficacy_walk   384.70    305.73      1.26 170.35    0.21  -218.81  988.22

round(pool.r.squared(fit_mod3_habit,adjusted=TRUE),2)
#       est lo 95 hi 95 fmi
#adj R^2 0.4  0.28  0.51 NaN

round(pool.r.squared(fit_mod3_habit),2)
#     est lo 95 hi 95 fmi
#R^2 0.45  0.33  0.56 NaN

#Rounding in mice
#https://stackoverflow.com/questions/61840259/round-function-no-longer-works-with-mice-output

