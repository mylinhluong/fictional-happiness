# this script creates new DF that includes tidy data for scoring self-report data for baseline and follow-up:  
  # the BREQ-3, 
  # physical exercise self-efficacy (PESES), 
  # attitudes about physical activity, 
  # the self-report behavioural automaticity index (SRBAI)
  # decisional intention & intention strength
  # pain numeric rating scale (NRS)
  # I haven't decided if I will put IPAQ in this script or another
  # sociodemographics

#to do
#need to review dataset for people who did not answer numerically for intention_min, weight, cm, school years completed
#make decision about how to include IPAQ data, in separate R script, or as part of the tidy_selfreport
#for IPAQ data, still working on creating a function, talk this out with R. think through 2-3 functions
#look into new.env()
#haven't dealt with sociodemographics yet. Will need to figure out how to deal w/ age, create variable for BMI

######BASELINE

#####Scoring SR scales: BREQ-3, PESES, attitudes, SRBAI#####
selfreport.keys<-list(group=c("group"),
                      identified = c("breq3_id1","breq3_id2","breq3_id3","breq3_id4"),
                      amotivation = c("breq3_amov1","breq3_amov2","breq3_amov3","breq3_amov4"),
                      intrinsic = c("breq3_intrins1","breq3_intrins2","breq3_intrins3","breq3_intrins4"),
                      introjected = c("breq3_intro1","breq3_intro2","breq3_intro3","breq3_intro4"),
                      integrated = c("breq3_integ1","breq3_integ2","breq3_integ3","breq3_integ4"),
                      extrinsic = c("breq3_ext1","breq3_ext2","breq3_ext3","breq3_ext4"),
                      selfefficacy=c("selfefficacy_1","selfefficacy_2","selfefficacy_3","selfefficacy_4","selfefficacy_5"),
                      instrumental = c("attitudes_i1","attitudes_i2"),
                      affective = c ("attitudes_a1","attitudes_a2"),
                      habit=c("srbai1","srbai2","srbai3","srbai4"))
selfreport.scales<-scoreItems (selfreport.keys, self_report_baseline)
selfreport.scales #show the output
selfreport.scores<-selfreport.scales$scores %>%
  as_data_frame(selfreport.scores) 

#####Decisional intention#####
intention<-self_report_baseline%>%
  select(group,intention=intention1a, "intention_min"=intention1b, intention_strength)

#*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*#
intention_min_review<-intention%>%
  select(intention_min)%>%
  filter_if(is.character(), any_vars(is.na(.)))

as.numeric(intention_min_review)

#*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*#


#####Pain NRS-baseline#####
pain_baseline<-self_report_baseline%>%
  select(group,pain_side,pain_both,"pain_T1"=pain_nrsl_v2.x)%>%
  mutate(study_knee=pain_side)%>%
  mutate(study_knee=na_if(study_knee,3))%>%
  mutate(study_knee=coalesce(study_knee,pain_both))%>%
  select(group,study_knee, pain_T1)

#####IPAQ-baseline
#source() #make sure group is a part of the df created

#####SOCIODEMOGRAPHICS
#Calculate age from year born
#Keep gender
#*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*#
#Calculate BMI from weight and cm (will probably have to deal with non-numeric data)


#Income-categorical
#*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*#
##not sure what to do with working/total adults/total children

#Education-categorical (keep)

#*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*##*#*#*# AN ISSUE TO DEAL WITH #*#*#*#
#Education-continous (will probably have to deal with non-numerical data)
#Location-categorical (keep)


######Creating clean BASELINE data with tidy data#####
baseline_scored<-selfreport.scores%>%
  left_join(intention, by="group")%>%
  left_join(pain_baseline, by="group")

left_join(IPAQ_baseline, by="group")%>%
  left_join(sociodems, by="group")



######FOLLOWUP

#####Pain NRS-FOLLOWUP#####
pain_followup<-self_report_followup%>%
  select(group,"pain_T2"=pain_nrsl_v2.y)

#####IPAQ-FOLLOWUP
#MAKE SURE TO ADD T2 TO VAR NAME

######Creating clean FOLLOW-UP data with tidy data#####
followup_scored<-pain_followup%>%
  left_join(IPAQ_T2, by="group")

#####Creating self-report data with tidy data#####
self_report_processed<-baseline_scored&>%
left_join(followup_scored, by="group")
