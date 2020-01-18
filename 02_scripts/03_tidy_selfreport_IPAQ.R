 #####IN PROGRESS###### this script scores IPAQ data

#*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*#
#######STILL WORKING ON THIS SECTION#####
#todo
#???hours and minutes responses were cleaned in excel;should they instead be done in R, tricky because not numeric data. 
#would need to figure out how to coerce data

######IPAQ Scoring Protocol##### #include link to the Scoring Protocol here
###MAXIMUM VALUES FOR EXCLUDING OUTLIERS###
#considered outliers and thus are excluded from analysis. All cases in which the sum
#total of all Walking, Moderate and Vigorous time variables is greater than 960
#minutes (16 hours) should be excluded from the analysis. This assumes that on
#average an individual of 8 hours per day is spent sleeping.
#The ‘days’ variables can take the range 0-7 days, or 8, 9 (don’t know or refused);
#values greater than 9 should not be allowed and those cases excluded from analysis.

###MINIMUM VALUES FOR DURATION OF ACTIVITY###
#Only values of 10 or more minutes of activity should be included in the calculation of
#summary scores. The rationale being that the scientific evidence indicates that
#episodes or bouts of at least 10 minutes are required to achieve health benefits.
#Responses of less than 10 minutes [and their associated days] should be re-coded to
#‘zero’.

#need to create a function whereby 'Walking', 'Moderate' and 'Vigorous' time variables are calculated
#-if <10 = 0, otherwise should be between 10-960
#in our dataset, 'days' 8 and 9 don't exist, so do not need to deal with this

data$excMin <- ifelse(sumpa > 960, 1,0)
data$excDay <- ifelse(sumday > 9, 1,0)

vminday <- ifelse(vminday < 10,0,vminday)
mminday <- ifelse(mminday < 10,0,mminday)
wminday <- ifelse(wminday < 10,0,wminday)

init.ipaq$ipaq_readj_freq_time <- function(dt_,time,freq) 
{
  freq_v <- dt_[,eval(as.name(freq))];  time_v <- dt_[,eval(as.name(time))]
  set(dt_, which(freq_v==0L),j=time,value = 0L)
  set(dt_, which(time_v>960L),j=time,value = NA_integer_)
  set(dt_, which(time_v<10L),j=time,value = 0L)
  set(dt_, which(time_v==0L),j=freq,value = 0L)
}

init.ipaq$ipaq_mult <- function(dt_,output,vars,num=1L)
{
  dt_[,(output):=Reduce('*',.SD)*eval(num),.SDcols=vars]
}


init.ipaq$ipaq_sum <- function(dt_,output,vars){ dt_[,(output):=rowSums(.SD),.SDcols=vars] }


init.ipaq$ipaq_mark_gt960_7 <- function(dt_,total_time)
{
  dt_[eval(as.name(total_time))>960*7,is_gt960time7:=1L]
}

init.ipaq$ipaq_clean_gt960_7 <- function (dt_,vars){dt_[is_gt960time7==1,c(vars):=NA]}

# Compute the time/day and clean: time in [10, 960]; freq ==0 -> time = 0 
ipaq_computetime(dt_,freq = 'PA_WRK_WALK_FREQ_CLEAN',min = 'PA_WRK_WALK_TIME_MIN',hr = 'PA_WRK_WALK_TIME_HR',time_output = 'PA_WRK_WALK_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_WRK_VIG_FREQ_CLEAN',min = 'PA_WRK_VIG_TIME_MIN',hr = 'PA_WRK_VIG_TIME_HR',time_output = 'PA_WRK_VIG_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_WRK_MOD_FREQ_CLEAN',min = 'PA_WRK_MOD_TIME_MIN',hr = 'PA_WRK_MOD_TIME_HR',time_output = 'PA_WRK_MOD_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_CYCLING_FREQ_CLEAN',min = 'PA_CYCLING_TIME_MIN',hr = 'PA_CYCLING_TIME_HR',time_output = 'PA_CYCLING_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_TRANS_WALK_FREQ_CLEAN',min = 'PA_TRANS_WALK_TIME_MIN',hr = 'PA_TRANS_WALK_TIME_HR',time_output = 'PA_TRANS_WALK_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_LEISURE_MOD_FREQ_CLEAN',min = 'PA_LEISURE_MOD_TIME_MIN',hr = 'PA_LEISURE_MOD_TIME_HR',time_output = 'PA_LEISURE_MOD_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_GARDEN_MOD_FREQ_CLEAN',min = 'PA_GARDEN_MOD_TIME_MIN',hr = 'PA_GARDEN_MOD_TIME_HR',time_output = 'PA_GARDEN_MOD_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_LEISURE_VIG_FREQ_CLEAN',min = 'PA_LEISURE_VIG_TIME_MIN',hr = 'PA_LEISURE_VIG_TIME_HR',time_output = 'PA_LEISURE_VIG_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_INSIDE_MOD_FREQ_CLEAN',min = 'PA_INSIDE_MOD_TIME_MIN',hr = 'PA_INSIDE_MOD_TIME_HR',time_output = 'PA_INSIDE_MOD_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_LEISURE_WALK_FREQ_CLEAN',min = 'PA_LEISURE_WALK_TIME_MIN',hr = 'PA_LEISURE_WALK_TIME_HR',time_output = 'PA_LEISURE_WALK_TIME_DAY')
ipaq_computetime(dt_,freq = 'PA_GARDEN_VIG_FREQ_CLEAN',min = 'PA_GARDEN_VIG_TIME_MIN',hr = 'PA_GARDEN_VIG_TIME_HR',time_output = 'PA_GARDEN_VIG_TIME_DAY')

# Readjust activity time_day AND freq: time in [10, 960]; freq ==0 -> time = 0 
ipaq_readj_freq_time(dt_, freq = 'PA_WRK_WALK_FREQ_CLEAN',time = 'PA_WRK_WALK_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_WRK_VIG_FREQ_CLEAN', time = 'PA_WRK_VIG_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_WRK_MOD_FREQ_CLEAN',time = 'PA_WRK_MOD_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_CYCLING_FREQ_CLEAN',time = 'PA_CYCLING_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_TRANS_WALK_FREQ_CLEAN',time = 'PA_TRANS_WALK_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_LEISURE_MOD_FREQ_CLEAN',time = 'PA_LEISURE_MOD_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_GARDEN_MOD_FREQ_CLEAN',time = 'PA_GARDEN_MOD_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_LEISURE_VIG_FREQ_CLEAN',time = 'PA_LEISURE_VIG_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_INSIDE_MOD_FREQ_CLEAN',time = 'PA_INSIDE_MOD_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_LEISURE_WALK_FREQ_CLEAN',time = 'PA_LEISURE_WALK_TIME_DAY')
ipaq_readj_freq_time(dt_, freq = 'PA_GARDEN_VIG_FREQ_CLEAN',time = 'PA_GARDEN_VIG_TIME_DAY')

#Mark where total weekly activity time > 960*7
ipaq_mark_gt960_7(dt_,'PA_TOTAL_TIME_WK');ipaq_mark_gt960_7(dt_,'PA_VIG_TIME_WK'); ipaq_mark_gt960_7(dt_,'PA_MOD_TIME_WK'); ipaq_mark_gt960_7(dt_,'PA_WALK_TIME_WK')

###TRUNCATION OF DATA RULES###
#In IPAQ long – variables total Walking, total Moderate intensity
#and total Vigorous-intensity activity are calculated and then, for each of
#these summed behaviours, the total value should be truncated to 3 hours (180minutes).

#TOTAL WALKING IF >180 = 180 MIN
#TOTAL MODERATE IF >180 = 180 MIN
#TOTAL VIGOROUS IF >180 = 180 MIN

#TOTAL PA MAX = 540 MIN/DAY

##IPAQ scoring
#truncating times over 3 hours
sr$vig.time.1.t<-ifelse(sr$vig.time.1>180,180,sr$vig.time.1)
sr$mod.time.1.t<-ifelse(sr$mod.time.1>180,180,sr$mod.time.1)
sr$walk.time.1.t<-ifelse(sr$walk.time.1>180,180,sr$walk.time.1)


sr$vig.1<-sr$vig.day.1*sr$vig.time.1.t
sr$mod.1<-sr$mod.day.1*sr$mod.time.1.t
sr$walk.1<-sr$walk.day.1*sr$walk.time.1.t

sr$pa.min.1<-sr$vig.1+sr$mod.1+sr$walk.1
sr$pa.met.1<-(3.3*sr$walk.1)+(4*sr$mod.1)+(8*sr$vig.1)


sr$vig.time.2.t<-ifelse(sr$vig.time.2>180,180,sr$vig.time.2)
sr$mod.time.2.t<-ifelse(sr$mod.time.2>180,180,sr$mod.time.2)
sr$walk.time.2.t<-ifelse(sr$walk.time.2>180,180,sr$walk.time.2)


sr$vig.2<-sr$vig.day.2*sr$vig.time.2.t
sr$mod.2<-sr$mod.day.2*sr$mod.time.2.t
sr$walk.2<-sr$walk.day.2*sr$walk.time.2.t

sr$pa.min.2<-sr$vig.2+sr$mod.2+sr$walk.2
sr$pa.met.2<-(3.3*sr$walk.2)+(4*sr$mod.2)+(8*sr$vig.2)


init.ipaq$ipaq_mark_trunc180 <- function (dt_,time_day ) { dt_[eval(as.name(time_day))>180 ,is_trunc180:=1L] }

init.ipaq$ipaq_trunc180 <- function(dt_,vars)
{
  for(x in vars) {set(dt_,j=k<-paste0(x,'_CLEAN'),value=dt_[,eval(as.name(x))]); dt_[is_trunc180==1 & eval(as.name(x))>180L,(k):=180L]}
}

init.ipaq$ipaq_estimate_time <- function (dt_, output, freq_est,time_wk)
{
  freq_est_v <- as.name(freq_est); time_wk_v <- as.name(time_wk)
  ix <- dt_[,eval(freq_est_v)]==0
  set(dt_,i= which(ix),j=output,value = 0.0)
  set(dt_,i=which(!ix), j=output,value = dt_[!ix, eval(time_wk_v)/eval(freq_est_v)])
}



init.ipaq$ipaq_div <- function(dt_,var,by,output) 
{
  by_v <- as.name(by)
  dt_[eval(by_v) == 0 ,(output):=0.0 ]
  dt_[!(eval(by_v) == 0),(output):= eval(as.name(var))/eval(by_v)]
}


#When analysing the data as categorical variable or presenting median and
#interquartile ranges of the MET-minute scores, the application of the truncation rule
#will not affect the results. This rule does have the important effect of preventing
#misclassification in the ‘high’ category. For example, an individual who reports
#walking for 10 minutes on 6 days and 12 hours of moderate activity on one day could
#be coded as ‘high’ because this pattern meets the ‘7 day” and “3000 MET-min”
#criteria for ‘high’. However, this uncommon pattern of activity is unlikely to yield the
#health benefits that the ‘high’ category is intended to represent.
#Although using median is recommended due to the skewed distribution of scores, if
#IPAQ data are analysed and presented as a continuous variable using mean values,
#the application of the truncation rule will produce slightly lower mean values than
#would otherwise be obtained.

###CALCULATING MET-MIN/WEEK CONTINUOUS SCORES###
#Data collected with the IPAQ long form can be reported as a continuous measure
#and reported as median MET-minutes. Median values and interquartile ranges can
#be computed for walking (W), moderate-intensity activities (M), and vigorous-intensity
#activities (V) within each domain using the formulas below. Total scores may also be
#calculated for walking (W), moderate-intensity activities (M), and vigorous-intensity
#activities (V); for each domain (work, transport, domestic and garden, and leisure)
#and for an overall grand total.

#WORK WORK WORK WORK WORK#
#WALKING AT WORK = 3.3 METS
#MODERATE AT WORK = 4.0 METS
#VIGOROUS AT WORK = 8.0 METS

# Update work related freq: work == 0 -> freq =0   
ipaq_update_work_freq(dt_,job='PA_JOB_UNPAID_WRK',vars = c('PA_WRK_WALK_FREQ_CLEAN','PA_WRK_VIG_FREQ_CLEAN','PA_WRK_MOD_FREQ_CLEAN'))
#did this in excel, need to do in R instead?

View(self_report_baseline_IPAQ)
#ACTIVE TRANSPORT
#WALKING FOR TRANSPORT = 3.3 METS
#CYCLING FOR TRANSPORT = 6.0 METS [CONSIDERED MODERATE]

#DOMESTIC/GARDEN WORK
#VIGOROUS AT DOMESTIC=5.5 METS [CONSIDERED MODERATE]
#MODERATE AT YARD = 4.0 METS
#MODERATE INSIDE = 3.0 METS

#LEISURE
#WALKING FOR LEISURE= 3.3 METS
#MODERATE LEISURE = 4.0 METS
#VIGOROUS LEISURE = 8.0 METS
#VIGOROUS INTENSITY IN LEISURE = 8.0 METS


##CALCULATE DOMAIN SUB SCORES (#WORK, #TRANSPORT #DOMESTIC/GARDEN #LEISURE)
##CALCULATE INTENSITY SUB SCORES (#WALKING, #MODERATE #VIGOROUS)
#-- VIG AND WALK
ipaq_mult(dt_,output = 'PA_TOTAL_VIG_MET_L',vars = c('PA_VIG_FREQ_EST','PA_VIG_TIME_DAY_EST_CLEAN'),num = 8.0)
ipaq_mult(dt_,output = 'PA_TOTAL_WALK_MET_L',vars = c('PA_WALK_FREQ_EST','PA_WALK_TIME_DAY_EST_CLEAN'),num = 3.3)

#--MOD
ipaq_mult(dt_,output = 'PA_CYCLING_PARTIAL_MET',vars = c('PA_MOD_FREQ_EST','PA_MOD_TIME_DAY_EST_CLEAN','PA_CYCLING_PROP'),num = 6)
ipaq_mult(dt_,output = 'PA_GARDEN_VIG_PARTIAL_MET',vars = c('PA_MOD_FREQ_EST','PA_MOD_TIME_DAY_EST_CLEAN','PA_GARDEN_VIG_PROP'),num = 5.5)
ipaq_mult(dt_,output = 'PA_INSIDE_MOD_PARTIAL_MET',vars = c('PA_MOD_FREQ_EST','PA_MOD_TIME_DAY_EST_CLEAN','PA_INSIDE_MOD_PROP'),num = 3)
ipaq_mult(dt_,output = 'PA_GARDEN_MOD_PARTIAL_MET',vars = c('PA_MOD_FREQ_EST','PA_MOD_TIME_DAY_EST_CLEAN','PA_GARDEN_MOD_PROP'),num = 4)
ipaq_mult(dt_,output = 'PA_LEISURE_MOD_PARTIAL_MET',vars = c('PA_MOD_FREQ_EST','PA_MOD_TIME_DAY_EST_CLEAN','PA_LEISURE_MOD_PROP'),num = 4)
ipaq_mult(dt_,output = 'PA_WRK_MOD_PARTIAL_MET',vars = c('PA_MOD_FREQ_EST','PA_MOD_TIME_DAY_EST_CLEAN','PA_WRK_MOD_PROP'),num = 4)

ipaq_sum(dt_,output = 'PA_TOTAL_MOD_MET_L',vars = c('PA_CYCLING_PARTIAL_MET','PA_GARDEN_VIG_PARTIAL_MET','PA_INSIDE_MOD_PARTIAL_MET','PA_GARDEN_MOD_PARTIAL_MET','PA_LEISURE_MOD_PARTIAL_MET','PA_WRK_MOD_PARTIAL_MET'))

#adjust MOD intensity with 0.9
ipaq_mult(dt_,output = 'PA_TOTAL_MOD_MET_L', vars = 'PA_TOTAL_MOD_MET_L', num = 0.9)



##TOTAL PA SCORE
#Total Physical Activity MET-minutes/week = Walking MET-minutes/week + Moderate METminutes/
#  week + Total Vigorous MET-minutes/week

#/NO MORE THAN 540 MINS/#

#Also
#Total Physical Activity MET-minutes/week = Total MET-minutes/week (at Work + for
#                                                                   Transport + in Chores + in Leisure)

# --------------TOTAL SCORE MET--------------------------------
ipaq_sum(dt_,output = 'PA_TOTAL_MET_L',vars = c('PA_TOTAL_VIG_MET_L','PA_TOTAL_MOD_MET_L','PA_TOTAL_WALK_MET_L'))


# Compute activity time per/week
ipaq_mult(dt_,vars = c('PA_WRK_WALK_FREQ_CLEAN', 'PA_WRK_WALK_TIME_DAY'),output = 'PA_WRK_WALK_TIME_WK')
ipaq_mult(dt_,vars = c('PA_WRK_VIG_FREQ_CLEAN', 'PA_WRK_VIG_TIME_DAY'),output = 'PA_WRK_VIG_TIME_WK')
ipaq_mult(dt_,vars = c('PA_WRK_MOD_FREQ_CLEAN', 'PA_WRK_MOD_TIME_DAY'),output = 'PA_WRK_MOD_TIME_WK')
ipaq_mult(dt_,vars = c('PA_CYCLING_FREQ_CLEAN', 'PA_CYCLING_TIME_DAY'),output = 'PA_CYCLING_TIME_WK')
ipaq_mult(dt_,vars = c('PA_TRANS_WALK_FREQ_CLEAN', 'PA_TRANS_WALK_TIME_DAY'),output = 'PA_TRANS_WALK_TIME_WK')
ipaq_mult(dt_,vars = c('PA_LEISURE_MOD_FREQ_CLEAN', 'PA_LEISURE_MOD_TIME_DAY'),output = 'PA_LEISURE_MOD_TIME_WK')
ipaq_mult(dt_,vars = c('PA_GARDEN_MOD_FREQ_CLEAN', 'PA_GARDEN_MOD_TIME_DAY'),output = 'PA_GARDEN_MOD_TIME_WK')
ipaq_mult(dt_,vars = c('PA_LEISURE_VIG_FREQ_CLEAN', 'PA_LEISURE_VIG_TIME_DAY'),output = 'PA_LEISURE_VIG_TIME_WK')
ipaq_mult(dt_,vars = c('PA_INSIDE_MOD_FREQ_CLEAN', 'PA_INSIDE_MOD_TIME_DAY'),output = 'PA_INSIDE_MOD_TIME_WK')
ipaq_mult(dt_,vars = c('PA_LEISURE_WALK_FREQ_CLEAN', 'PA_LEISURE_WALK_TIME_DAY'),output = 'PA_LEISURE_WALK_TIME_WK')
ipaq_mult(dt_,vars = c('PA_GARDEN_VIG_FREQ_CLEAN', 'PA_GARDEN_VIG_TIME_DAY'),output = 'PA_GARDEN_VIG_TIME_WK')


#Compute weekly activity for each intensity group [walk,vig,mod]
ipaq_sum(dt_,output = 'PA_VIG_TIME_WK', vars = c('PA_WRK_VIG_TIME_WK','PA_LEISURE_VIG_TIME_WK'))
ipaq_sum(dt_,output = 'PA_MOD_TIME_WK',vars = c('PA_WRK_MOD_TIME_WK','PA_CYCLING_TIME_WK','PA_GARDEN_MOD_TIME_WK','PA_GARDEN_VIG_TIME_WK','PA_INSIDE_MOD_TIME_WK','PA_LEISURE_MOD_TIME_WK'))
ipaq_sum(dt_,output = 'PA_WALK_TIME_WK',vars = c('PA_WRK_WALK_TIME_WK','PA_LEISURE_WALK_TIME_WK','PA_TRANS_WALK_TIME_WK'))

#Sum all weekly activity intensity times
ipaq_sum(dt_,output = 'PA_TOTAL_TIME_WK',vars = c('PA_VIG_TIME_WK','PA_MOD_TIME_WK','PA_WALK_TIME_WK'))