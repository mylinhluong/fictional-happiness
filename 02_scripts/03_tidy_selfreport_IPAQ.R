#this script scores IPAQ data

#todo
#how to deal with the >960 values, since they should be excluded and NOT imputed as NAs? 
#thresholds for total min v total MET-minutes (see page 8 of scoring protocol) vs. page 11 (7.4 truncation of data)
#calculate median and IQR

######IPAQ Scoring Protocol##### #link to Scoring Protocol here: https://sites.google.com/site/theipaq/scoring-protocol
###MINIMUM VALUES FOR DURATION OF ACTIVITY###
#Only values of 10 or more minutes of activity should be included in the calculation of
#summary scores. The rationale being that the scientific evidence indicates that
#episodes or bouts of at least 10 minutes are required to achieve health benefits.
#Responses of less than 10 minutes [and their associated days] should be re-coded to
#‘zero’. 

#in our data, we corrected this in excel, may need to go back and correct this in R?
##check if any mins <10, if so replace with 0
#minutes<-select(self_report_baseline_IPAQ, ends_with("_MIN"))
#View(minutes)

#The ‘days’ variables can take the range 0-7 days, or 8, 9 (don’t know or refused);
#values greater than 9 should not be allowed and those cases excluded from analysis.
#in our survey they only took the range of 0-7, so 8/9 are not in our data
#check if any frequencies >7, if so, replace with 0
#frequency<-select(self_report_baseline_IPAQ, ends_with("FREQ"))

###MAXIMUM VALUES FOR EXCLUDING OUTLIERS###
#All cases in which the sum total of all Walking, Moderate and Vigorous time variables is greater than 960
#minutes (16 hours) should be excluded from the analysis. This assumes that on
#average an individual of 8 hours per day is spent sleeping.

###TRUNCATION OF DATA RULES###
#In IPAQ long – variables total Walking, total Moderate intensity
#and total Vigorous-intensity activity are calculated and then, for each of
#these summed behaviours, the total value should be truncated to 3 hours (180minutes).
##??how to deal with this in IPAQ long, whereby there are different activity domains and you're not referring back to total time variables??

#colnames(self_report_baseline_IPAQ)
#[1] "group"                    "PA_JOB_UNPAID_WRK"        "PA_WRK_VIG_FREQ"          "PA_WRK_VIG_TIME_HR"       "PA_WRK_VIG_TIME_MIN"      "PA_WRK_MOD_FREQ"          "PA_WRK_MOD_TIME_HR"       "PA_WRK_MOD_TIME_MIN"     
#[9] "PA_WRK_WALK_FREQ"         "PA_WRK_WALK_TIME_HR"      "PA_WRK_WALK_TIME_MIN"     "PA_TRANS_FREQ"            "PA_TRANS_TIME_HOUR"       "PA_TRANS_TIME_MIN"        "PA_CYCLING_FREQ"          "PA_CYCLING_TIME_HR"      
#[17] "PA_CYCLING_TIME_MIN"      "PA_TRANS_WALK_FREQ"       "PA_TRANS_WALK_TIME_HR"    "PA_TRANS_WALK_TIME_MIN"   "PA_GARDEN_VIG_FREQ"       "PA_GARDEN_VIG_TIME_HR"    "PA_GARDEN_VIG_TIME_MIN"   "PA_GARDEN_MOD_FREQ"      
#[25] "PA_GARDEN_MOD_TIME_HR"    "PA_GARDEN_MOD_TIME_MIN"   "PA_INSIDE_MOD_FREQ"       "PA_INSIDE_MOD_HR"         "PA_INSIDE_MOD_MIN"        "PA_LEISURE_WALK_FREQ"     "PA_LEISURE_WALK_TIME_HR"  "PA_LEISURE_WALK_TIME_MIN"
#[33] "PA_LEISURE_VIG_FREQ"      "PA_LEISURE_VIG_TIME_HR"   "PA_LEISURE_VIG_TIME_MIN"  "PA_LEISURE_MOD_FREQ"      "PA_LEISURE_MOD_TIME_HR"   "PA_LEISURE_MOD_TIME_MIN" 


#Walking MET-minutes/week = 3.3 * walking minutes * walking days (work/active transport/leisure)

#Moderate MET-minutes/week = 4.0 * moderate-intensity (work/yard/leisure) minutes * moderate days 
#+ 5.5*vigorous-intensity YARD work + 3.0 moderate-intensity INSIDE chores + 6.0 * CYCLING mins* cycle days

#Vigorous MET-minutes/week = 8.0 * vigorous-intensity activity minutes * vigorous-intensity days (work,leisure)

#Total physical activity MET-minutes/week = sum of Walking + Moderate + Vigorous METminutes/
#  week scores.

#DOMAINS
#work
#active transport
#domestic and garden
#leisure-time

#creating a tidy IPAQ data frame at baseline
IPAQ_T1<-self_report_baseline_IPAQ%>%
  mutate(total_walk_min=(PA_WRK_WALK_TIME_MIN)+(PA_TRANS_WALK_TIME_MIN) + (PA_LEISURE_WALK_TIME_MIN),
         total_mod_min=(PA_WRK_MOD_TIME_MIN) + (PA_CYCLING_TIME_MIN) + (PA_GARDEN_VIG_TIME_MIN),
         total_vig_min=(PA_WRK_VIG_TIME_MIN)+ PA_LEISURE_VIG_TIME_MIN)%>%
  mutate(total_PA_min=total_walk_min+total_mod_min+total_vig_min)%>%
  filter(is.na(total_PA_min)|total_PA_min<960)%>% #check if total walking, mod, vig >960, if so then NA_integer_, or remove from dataset?
  mutate(total_walk_min_trunc=ifelse(total_walk_min>180,180, total_walk_min),#is it necessary to create this variable for long IPAQ?
        total_mod_min_trunc=ifelse(total_mod_min>180,180,total_mod_min),
        total_vig_min_trunc=ifelse(total_vig_min>180,180, total_vig_min))%>%
  mutate(PA_WRK_VIG_TIME_DAY=PA_WRK_VIG_FREQ*PA_WRK_VIG_TIME_MIN,
         PA_WRK_MOD_TIME_DAY=PA_WRK_MOD_FREQ*PA_WRK_MOD_TIME_MIN,
         PA_WRK_WALK_TIME_DAY=PA_WRK_WALK_FREQ*PA_WRK_WALK_TIME_MIN,
         PA_CYCLING_TIME_DAY=PA_CYCLING_FREQ*PA_CYCLING_TIME_MIN,
         PA_TRANS_WALK_TIME_DAY=PA_TRANS_WALK_FREQ*PA_TRANS_WALK_TIME_MIN,
         PA_GARDEN_VIG_TIME_DAY=PA_GARDEN_VIG_FREQ*PA_GARDEN_VIG_TIME_MIN,
         PA_GARDEN_MOD_TIME_DAY=PA_GARDEN_MOD_FREQ*PA_GARDEN_MOD_TIME_MIN,
         PA_INSIDE_MOD_TIME_DAY=PA_INSIDE_MOD_FREQ*PA_INSIDE_MOD_MIN,
         PA_LEISURE_WALK_TIME_DAY=PA_LEISURE_WALK_FREQ*PA_LEISURE_WALK_TIME_MIN,
         PA_LEISURE_VIG_TIME_DAY=PA_LEISURE_VIG_FREQ*PA_LEISURE_VIG_TIME_MIN,
         PA_LEISURE_MOD_TIME_DAY=PA_LEISURE_MOD_FREQ*PA_LEISURE_MOD_TIME_MIN)%>%
  mutate(PA_WRK_MET=PA_WRK_VIG_TIME_DAY*8+PA_WRK_MOD_TIME_DAY*4+PA_WRK_WALK_TIME_DAY*3.3,#WORK domain
         PA_TRANS_MET=PA_CYCLING_TIME_DAY*6+PA_TRANS_WALK_TIME_DAY*3.3, #ACTIVE TRANSPORT domain
         PA_GARDEN_INSIDE_MET=PA_GARDEN_VIG_TIME_DAY*5.5+PA_GARDEN_MOD_TIME_DAY*4+PA_INSIDE_MOD_TIME_DAY*3,#YARD/HOUSEWORK domain
         PA_LEISURE_MET=PA_LEISURE_WALK_TIME_DAY*3.3+PA_LEISURE_MOD_TIME_DAY*4+PA_LEISURE_VIG_TIME_DAY*8, #LEISURE domain
         PA_TOTAL_WALK_MET=PA_WRK_WALK_TIME_DAY*3.3+PA_TRANS_WALK_TIME_DAY*3.3+PA_LEISURE_WALK_TIME_DAY*3.3,#WALKING activity intensity
         PA_TOTAL_MOD_MET=PA_CYCLING_TIME_DAY*6+PA_GARDEN_VIG_TIME_DAY*5.5+PA_GARDEN_MOD_TIME_DAY*4+PA_INSIDE_MOD_TIME_DAY*3+PA_LEISURE_MOD_TIME_DAY*4,#MODERATE activity intensity
         PA_TOTAL_VIG_MET=PA_WRK_VIG_TIME_DAY*8+PA_LEISURE_VIG_TIME_DAY*8)%>% #VIGOROUS activity intensity
  mutate(TOTAL_PA_SCORE_WEEK=PA_TOTAL_WALK_MET+PA_TOTAL_MOD_MET+PA_TOTAL_VIG_MET)%>%
  mutate(PA_TOTAL_WALK_MET_trunc=ifelse(PA_TOTAL_WALK_MET>4158, 4158, PA_TOTAL_WALK_MET), #I ended up truncating based on max METs possible
         PA_TOTAL_MOD_MET_trunc=ifelse(PA_TOTAL_MOD_MET>7560,7560, PA_TOTAL_MOD_MET),
         PA_TOTAL_VIG_MET_trunc=ifelse(PA_TOTAL_VIG_MET>10080, 10080, PA_TOTAL_VIG_MET))%>%
  mutate(TOTAL_PA_SCORE_WEEK_trunc=PA_TOTAL_WALK_MET_trunc+PA_TOTAL_MOD_MET_trunc+ PA_TOTAL_VIG_MET_trunc)%>%
  select(group,IPAQ_MET_MIN_WEEK_T1=TOTAL_PA_SCORE_WEEK, IPAQ_MET_MIN_WEEK_T1_trunc=TOTAL_PA_SCORE_WEEK_trunc)

#maybe >4158, 7560, 10080 aren't necessary
#As there are no established thresholds for presenting MET-minutes, the IPAQ
#Research Committee proposes that these data are reported as comparisons of
#median values and interquartile ranges for different populations.

#HOWEVER, in 7.4 Truncation of data rules
#In IPAQ long – the truncation process is more complicated, but to be consistent with
#the approach for IPAQ short requires that the variables total Walking, total Moderateintensity
#and total Vigorous-intensity activity are calculated and then, for each of
#these summed behaviours, the total value should be truncated to 3 hours (180 minutes).

#MAX METs for walking=(180*7)*3.3=4158
#MAX METs for moderate= (180*7)*6= 7560
#MAX METs for vigorous= (180*7)*8=10080

#IPAQ excluded participants because gt>960, write code for NA_integer_ for exclusion? different than NA b/c don't want to impute data?
#IPAQ_T1_gt960<-self_report_baseline_IPAQ%>%
#  mutate(total_walk_min=(PA_WRK_WALK_TIME_MIN)+(PA_TRANS_WALK_TIME_MIN) + (PA_LEISURE_WALK_TIME_MIN),
#         total_mod_min=(PA_WRK_MOD_TIME_MIN) + (PA_CYCLING_TIME_MIN) + (PA_GARDEN_VIG_TIME_MIN),
#         total_vig_min=(PA_WRK_VIG_TIME_MIN)+ PA_LEISURE_VIG_TIME_MIN)%>%
#  mutate(total_PA_min=total_walk_min+total_mod_min+total_vig_min)%>%
#  filter(total_PA_min>960)%>%
#  select(group, total_PA_min)

#View(IPAQ_T1_gt960)
#group total_PA_min
#1   5246803         1080
#2 280206977         1110
#3 367351790         1741
#4 462487730         1040
#5 670504335         1140
#6 721607202         1080

#creating a tidy IPAQ data frame at followup
IPAQ_T2<-self_report_followup_IPAQ%>%
  mutate(total_walk_min=(PA_WRK_WALK_TIME_MIN)+(PA_TRANS_WALK_TIME_MIN) + (PA_LEISURE_WALK_TIME_MIN),
         total_mod_min=(PA_WRK_MOD_TIME_MIN) + (PA_CYCLING_TIME_MIN) + (PA_GARDEN_VIG_TIME_MIN),
         total_vig_min=(PA_WRK_VIG_TIME_MIN)+ PA_LEISURE_VIG_TIME_MIN)%>%
  mutate(total_PA_min=total_walk_min+total_mod_min+total_vig_min)%>%
  filter(is.na(total_PA_min)|total_PA_min<960)%>%
  mutate(total_walk_min_trunc=ifelse(total_walk_min>180,180, total_walk_min),
         total_mod_min_trunc=ifelse(total_mod_min>180,180,total_mod_min),
         total_vig_min_trunc=ifelse(total_vig_min>180,180, total_vig_min))%>%
  mutate(PA_WRK_VIG_TIME_DAY=PA_WRK_VIG_FREQ*PA_WRK_VIG_TIME_MIN,
         PA_WRK_MOD_TIME_DAY=PA_WRK_MOD_FREQ*PA_WRK_MOD_TIME_MIN,
         PA_WRK_WALK_TIME_DAY=PA_WRK_WALK_FREQ*PA_WRK_WALK_TIME_MIN,
         PA_CYCLING_TIME_DAY=PA_CYCLING_FREQ*PA_CYCLING_TIME_MIN,
         PA_TRANS_WALK_TIME_DAY=PA_TRANS_WALK_FREQ*PA_TRANS_WALK_TIME_MIN,
         PA_GARDEN_VIG_TIME_DAY=PA_GARDEN_VIG_FREQ*PA_GARDEN_VIG_TIME_MIN,
         PA_GARDEN_MOD_TIME_DAY=PA_GARDEN_MOD_FREQ*PA_GARDEN_MOD_TIME_MIN,
         PA_INSIDE_MOD_TIME_DAY=PA_INSIDE_MOD_FREQ*PA_INSIDE_MOD_MIN,
         PA_LEISURE_WALK_TIME_DAY=PA_LEISURE_WALK_FREQ*PA_LEISURE_WALK_TIME_MIN,
         PA_LEISURE_VIG_TIME_DAY=PA_LEISURE_VIG_FREQ*PA_LEISURE_VIG_TIME_MIN,
         PA_LEISURE_MOD_TIME_DAY=PA_LEISURE_MOD_FREQ*PA_LEISURE_MOD_TIME_MIN)%>%
  mutate(PA_WRK_MET=PA_WRK_VIG_TIME_DAY*8+PA_WRK_MOD_TIME_DAY*4+PA_WRK_WALK_TIME_DAY*3.3,
         PA_TRANS_MET=PA_CYCLING_TIME_DAY*6+PA_TRANS_WALK_TIME_DAY*3.3,
         PA_GARDEN_INSIDE_MET=PA_GARDEN_VIG_TIME_DAY*5.5+PA_GARDEN_MOD_TIME_DAY*4+PA_INSIDE_MOD_TIME_DAY*3,
         PA_LEISURE_MET=PA_LEISURE_WALK_TIME_DAY*3.3+PA_LEISURE_MOD_TIME_DAY*4+PA_LEISURE_VIG_TIME_DAY*8,
         PA_TOTAL_WALK_MET=PA_WRK_WALK_TIME_DAY*3.3+PA_TRANS_WALK_TIME_DAY*3.3+PA_LEISURE_WALK_TIME_DAY*3.3,
         PA_TOTAL_MOD_MET=PA_CYCLING_TIME_DAY*6+PA_GARDEN_VIG_TIME_DAY*5.5+PA_GARDEN_MOD_TIME_DAY*4+PA_INSIDE_MOD_TIME_DAY*3+PA_LEISURE_MOD_TIME_DAY*4,
         PA_TOTAL_VIG_MET=PA_WRK_VIG_TIME_DAY*8+PA_LEISURE_VIG_TIME_DAY*8)%>%
  mutate(TOTAL_PA_SCORE_WEEK=PA_TOTAL_WALK_MET+PA_TOTAL_MOD_MET+PA_TOTAL_VIG_MET)%>%
  mutate(PA_TOTAL_WALK_MET_trunc=ifelse(PA_TOTAL_WALK_MET>4158, 4158, PA_TOTAL_WALK_MET),
         PA_TOTAL_MOD_MET_trunc=ifelse(PA_TOTAL_MOD_MET>7560,7560, PA_TOTAL_MOD_MET),
         PA_TOTAL_VIG_MET_trunc=ifelse(PA_TOTAL_VIG_MET>10080, 10080, PA_TOTAL_VIG_MET))%>%
  mutate(TOTAL_PA_SCORE_WEEK_trunc=PA_TOTAL_WALK_MET_trunc+PA_TOTAL_MOD_MET_trunc+ PA_TOTAL_VIG_MET_trunc)%>%
  select(group,IPAQ_MET_MIN_WEEK_T2=TOTAL_PA_SCORE_WEEK, IPAQ_MET_MIN_WEEK_T2_trunc=TOTAL_PA_SCORE_WEEK_trunc)

#IPAQ excluded participants because gt>960, write code for NA_integer_ for exclusion? different than NA b/c don't want to impute data?
#IPAQ_T2_gt960<-self_report_followup_IPAQ%>%
#  mutate(total_walk_min=(PA_WRK_WALK_TIME_MIN)+(PA_TRANS_WALK_TIME_MIN) + (PA_LEISURE_WALK_TIME_MIN),
#         total_mod_min=(PA_WRK_MOD_TIME_MIN) + (PA_CYCLING_TIME_MIN) + (PA_GARDEN_VIG_TIME_MIN),
#         total_vig_min=(PA_WRK_VIG_TIME_MIN)+ PA_LEISURE_VIG_TIME_MIN)%>%
#  mutate(total_PA_min=total_walk_min+total_mod_min+total_vig_min)%>%
#  filter(total_PA_min>960)%>%
#  select(group, total_PA_min)

#View(IPAQ_T2_gt960)
#group total_PA_min
#1 670504335         1110
#2 870091392         1095