# this script creates new DF that includes tidy data for scoring self-report data for baseline and follow-up:  
  # the BREQ-3, 
  # physical exercise self-efficacy (PESES), 
  # attitudes about physical activity, 
  # the self-report behavioural automaticity index (SRBAI)
  # decisional intention & intention strength
  # pain numeric rating scale (NRS)
  # sources IPAQ from another script
  # sociodemographics

#to do
#look into local v global environmentment [new.env()]to clean up global environment and leave only dataset for analysis
#change intention_min to 0 of intention = 0

#str(self_report_processed)
library(psych)

#Create a .txt file within the errors folder
tidy_selfreport_03 <- file(here("02_scripts","Errors", "03_tidy_selfreport.txt"), open = "wt")
sink(tidy_selfreport_03, type = "message")

######BASELINE

#####Scoring SR scales: BREQ-3, PESES, attitudes, SRBAI#####
self_report_keys<-list(group=c("group"),
                      identified = c("breq3_id1","breq3_id2","breq3_id3","breq3_id4"),
                      amotivation = c("breq3_amov1","breq3_amov2","breq3_amov3","breq3_amov4"),
                      intrinsic = c("breq3_intrins1","breq3_intrins2","breq3_intrins3","breq3_intrins4"),
                      introjected = c("breq3_intro1","breq3_intro2","breq3_intro3","breq3_intro4"),
                      integrated = c("breq3_integ1","breq3_integ2","breq3_integ3","breq3_integ4"),
                      extrinsic = c("breq3_ext1","breq3_ext2","breq3_ext3","breq3_ext4"),
                      selfefficacy=c("selfefficacy_1","selfefficacy_2","selfefficacy_3","selfefficacy_4"),
                      selfefficacy_walk=c("selfefficacy_5"),
                      instrumental = c("attitudes_i1","attitudes_i2"),
                      affective = c ("attitudes_a1","attitudes_a2"),
                      habit=c("srbai1","srbai2","srbai3","srbai4"))
self_report_scales<-scoreItems (self_report_keys, self_report_baseline)
#self_report_scales #show the output

#       group identified amotivation intrinsic introjected integrated extrinsic selfefficacy instrumental affective habit
#alpha     1        0.8        0.82      0.94        0.79       0.92       0.8         0.91         0.92      0.91  0.95

#Standard errors of unstandardized Alpha:
#  group identified amotivation intrinsic introjected integrated extrinsic selfefficacy instrumental affective habit
#ASE     NaN      0.045       0.044     0.032       0.047      0.034     0.045        0.029        0.073     0.074 0.031

#Average item correlation:
#  group identified amotivation intrinsic introjected integrated extrinsic selfefficacy instrumental affective habit
#average.r   NaN       0.51        0.52      0.79        0.48       0.75      0.51         0.67         0.86      0.84  0.81

#Median item correlation:
#  group   identified  amotivation    intrinsic  introjected   integrated    extrinsic selfefficacy instrumental    affective 
#NA         0.55         0.54         0.79         0.48         0.74         0.52         0.69         0.86         0.84 
#habit 
#0.83 

#Guttman 6* reliability: 
#  group identified amotivation intrinsic introjected integrated extrinsic selfefficacy instrumental affective habit
#Lambda.6  0.13       0.85        0.84      0.94         0.8       0.94      0.81         0.92         0.89       0.9  0.95

#Signal/Noise based upon av.r : 
#  group identified amotivation intrinsic introjected integrated extrinsic selfefficacy instrumental affective habit
#Signal/Noise   NaN        4.1         4.4        15         3.7         12       4.1           10           12        11    17

#Scale intercorrelations corrected for attenuation 
#raw correlations below the diagonal, alpha on the diagonal 
#corrected correlations above the diagonal:
#  group identified amotivation intrinsic introjected integrated extrinsic selfefficacy instrumental affective  habit
#group         1.0000     0.0039      -0.056     0.068      -0.053      0.024    -0.075      -0.0013      -0.0077     0.061  0.047
#identified    0.0035     0.8044      -0.661     0.895       0.595      0.921    -0.323       0.5932       0.7081     0.767  0.694
#amotivation  -0.0507    -0.5349       0.815    -0.549      -0.248     -0.564     0.432      -0.4751      -0.5735    -0.573 -0.483
#intrinsic     0.0656     0.7772      -0.480     0.938       0.434      0.878    -0.325       0.6294       0.5841     0.877  0.710
#introjected  -0.0469     0.4735      -0.199     0.373       0.788      0.493     0.188       0.2509       0.3821     0.362  0.318
#integrated    0.0233     0.7946      -0.490     0.818       0.421      0.924    -0.315       0.6580       0.5943     0.784  0.766
#extrinsic    -0.0673    -0.2596       0.349    -0.282       0.149     -0.272     0.804      -0.4090      -0.1444    -0.317 -0.322
#selfefficacy -0.0012     0.5079      -0.409     0.582       0.213      0.604    -0.350       0.9113       0.4014     0.581  0.664
#instrumental -0.0074     0.6103      -0.498     0.544       0.326      0.549    -0.124       0.3683       0.9237     0.653  0.489
#affective     0.0587     0.6579      -0.494     0.812       0.308      0.721    -0.272       0.5300       0.5997     0.914  0.684
#habit         0.0460     0.6052      -0.424     0.668       0.275      0.716    -0.280       0.6167       0.4573     0.636  0.946

self_report_scores<-self_report_scales$scores

self_report_scores<-self_report_scales$scores%>%
  as_tibble(self_report_scores) 

#####Decisional intention#####
#converting numeric data, and non-numeric data to NA_integer_
intention<-self_report_baseline%>%
  select(group,intention=intention1a,intention_strength,intention_origin=intention1b)%>% #selecting vars#
  mutate(intention_min=intention_origin)%>%
  arrange(desc(intention_min))%>%
  mutate(intention_min=sub("lot of walking",NA_integer_,intention_min))%>% #re-coding non-numeric
  mutate(intention_min=sub("at work I don't count", NA_integer_, intention_min))%>%
  mutate(intention_min=sub("Am almost continually active in one way or another", NA_integer_, intention_min))%>%
  mutate(intention_min=sub("6 x 40-45mins", 240, intention_min))%>%
  mutate(intention_min=sub("1 hour  a day plus housework gardening",60,intention_min))%>%
  mutate(intention_min=sub("4-Mar", NA_integer_, intention_min))%>%
  mutate(intention_min=sub("300 to 360", 300, intention_min))%>%
  mutate(intention_min=sub("360 - 400 minutes", 360, intention_min))%>%
  mutate(intention_min=replace(intention_min,219,c(10)))%>% #this is calling by row and column since could not find string 10+
  mutate(intention_min=replace(intention_min,99,c(300)))%>%  #this is calling by row and column since could not find string 300+
  mutate(intention_min=as.numeric(as.character(intention_min, na.rm=FALSE)))%>% #coercing the data into numeric
  mutate(intention_min=replace(intention_min, which(is.na(intention_min)& intention==0),0))%>%
  select(group, intention, intention_min, intention_strength)
  
##Examples of data in intention_min that were not numeric
#if characters = NA_integer_
#if numeric = floor
#"lot of walking"=NA_integer_
#"at work I don't count"=NA_integer_
#"Am almost continually active in one way or another", NA_integer_
#"6 x 40-45mins" = 240
#"4-Mar"=NA_integer=
#"360 - 400 minutes"=360
#"300+"=300
#"300 to 360"=300
#"10+"=10
#"1 hour a day plus housework gardening=60

#####Pain NRS-baseline#####
pain_baseline<-self_report_baseline%>%
  select(group,pain_side,pain_both,"pain_T1"=pain_nrsl_v2.x)%>%
  mutate(study_knee=pain_side)%>%
  mutate(study_knee=na_if(study_knee,3))%>% #if participants selected both knees had pain, had to select which one was more painful (pain_both)
  mutate(study_knee=coalesce(study_knee,pain_both))%>%
  select(group,study_knee, pain_T1)

#####IPAQ-baseline
source('./02_scripts/03_tidy_selfreport_IPAQ.R') 
#source from another script, or include here?

#####SOCIODEMOGRAPHICS
#Data cleaning for height, weight and BMI
#Used data editing/cleaning from https://melbourneinstitute.unimelb.edu.au/assets/documents/hilda-bibliography/hilda-technical-papers/htec108.pdf
#inspected for cases that met one of the following criteria:
#• height less than 120cms or more than 210 cms;
#• weight less than 40 kgs or more than 200 kgs; or
#• a height and weight combination that led to a BMI of less than 15 or more than 50. 
#extremely unlikely values replaced with NA_integer_. 
#Cutoff points:
#       Men             Women
#    L       U         L         U
#H  130cms  229cms   110cms    210cms
#W  35kgs   300kgs    25kgs   300kgs

#group=unique participant ID
#sociodems_gender: 1=F, 2=M
#sociodems_weight=in kg
#sociodems_height=in cm

#check on values
#sociodems_BMI_m<-self_report_baseline%>%
#  select(group, sociodems_gender, sociodems_height,sociodems_weight)%>%
#  filter(sociodems_gender==2)%>%
#  arrange(desc(sociodems_height))%>%
#  #60 773094008                2              116             98.0
#  #61 835779665                2               16             75.0
#  arrange(desc(sociodems_weight))
#  #61       240                2              176             10.0

#sociodems_BMI_f<-self_report_baseline%>%
#  select(group, sociodems_gender, sociodems_height,sociodems_weight)%>%
#  filter(sociodems_gender ==1)%>%
#  arrange(desc(sociodems_height))%>%
#  #157  19665804                1            71.00             65.0
#  #158 970289459                1             5.60             77.0
#  arrange(desc(sociodems_weight))%>%
  

#HEIGHT AND WEIGHT DATA CLEANING & BMI CALC 
#select cols, only if gender is reported
sociodems_BMI<-self_report_baseline %>% 
  select(group, sociodems_gender, sociodems_height,sociodems_weight)%>% 
  filter(!is.na(sociodems_gender))

#parameters in for loop
num_row = 1

#screen the input data table line by line. Add the row to the tidy table if the line meets the criteria
for (r in 1:nrow(sociodems_BMI)) {
  print(r)
  
  #1st layer: gender-males
  if (sociodems_BMI[r,'sociodems_gender'] == 2){
    #2nd layer: height and weight  
    if (!(sociodems_BMI[r,'sociodems_height']%in% 130:229)){
      sociodems_BMI[r,'sociodems_height']= NA_integer_
    }
    if (!(sociodems_BMI[r,'sociodems_weight']%in% 35:300)){
      sociodems_BMI[r,'sociodems_weight']= NA_integer_}
  }
  
  #1st layer: gender-females
  if (sociodems_BMI[r,'sociodems_gender'] == 1){
    #2nd layer: height and weight
    if (!(sociodems_BMI[r,'sociodems_height']%in% 110:210)){
      sociodems_BMI[r,'sociodems_height']= NA_integer_
    }
    if (!(sociodems_BMI[r,'sociodems_weight']%in% 25:300)){
      sociodems_BMI[r,'sociodems_weight']= NA_integer_}
  }
  
  
  num_row = num_row +1
} 

#add col of BMI_calc
sociodems_BMI<-sociodems_BMI%>%
  mutate(BMI_calc=(sociodems_weight/sociodems_height/sociodems_height)*10000)%>% 
  select(group=group, gender=sociodems_gender, weight=sociodems_weight,height=sociodems_height, BMI_calc=BMI_calc)

#View(sociodems_BMI)


#DEALING WITH REMAINING SOCIODEMOGRAPHICS + MERGING WITH CLEANED HEIGHT, WEIGHT & BMI DATA
#*convert year of birth to age in years
#*converting str of data, variables of income, education level, state and post code to factors
#*rename variables
#*merge sociodemographic datasets
#*replace extremely unlikely values of height and weight combination that led to a BMI of 
#*...less than 15 or more than 50 with NA_integer_.
sociodems<-self_report_baseline%>%
  select(group, age_year, sociodems_income1:sociodems_post)%>%
  arrange(desc(age_year))%>%
  mutate(age_year=sub("5/5/1905","1905",age_year))%>% #is there really somebody who is 114?
  mutate(age_year=as.numeric(age_year))%>%
  mutate(age=2019-age_year)%>% #converting year of both to age, study ended in 2019
  mutate_at(vars(sociodems_loc, 
                 sociodems_post),
            list(factor)) %>% 
  select(group, 
         age, 
         income=sociodems_income1, 
         educ_lvl=sociodems_educ, 
         educ_years=sociodems_educ2, 
         state=sociodems_loc, 
         pcd=sociodems_post)%>% 
  left_join(sociodems_BMI,by="group")%>% 
  arrange(desc(BMI_calc))%>%
  #       group age income educ_lvl educ_years state  pcd gender weight height BMI_calc
  #1  844956205  59      3        3       12.0     6 3803      1  150.0 164.00 55.77037
  #2  301739772  52      2        3       12.0     5 2580      1   99.0 135.00 54.32099
  mutate(BMI_calc=ifelse(BMI_calc>50,NA_integer_,BMI_calc))%>%
  mutate(BMI_calc=ifelse(BMI_calc<15,NA_integer_,BMI_calc))

  
######Creating clean BASELINE data with tidy data#####
baseline_scored<-self_report_scores%>%
  left_join(intention, by="group")%>%
  left_join(pain_baseline, by="group")%>%
  left_join(IPAQ_T1, by="group")%>%
  left_join(sociodems, by="group")

#View(baseline_scored)
######FOLLOWUP

#####Pain NRS-FOLLOWUP#####
pain_followup<-self_report_followup%>%
  select(group,"pain_T2"=pain_nrsl_v2.y)

######Creating clean FOLLOW-UP data with tidy data#####
followup_scored<-pain_followup%>%
  left_join(IPAQ_T2, by="group")

###### Creating self-report data with tidy data#####
self_report_processed<-baseline_scored%>%
  left_join(followup_scored, by="group")%>%
  mutate_at(vars(study_knee,
                 gender),
            list(factor))
  #to convert these variables into factors

#View(self_report_processed)
#str(self_report_processed)

#Save object to an rds file to preserve column data types
#saveRDS(self_report_processed,"01_data/02_processed/self_report_processed.rds")

#Write to CSV file
#write.csv(self_report_processed,"01_data/02_processed/self_report_processed.csv", row.names=FALSE)

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(tidy_selfreport_03)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "03_tidy_selfreport.txt"))
