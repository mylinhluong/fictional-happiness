#this script selects specific variables for self-report (baseline & follow-up) data and IATs, and renames variables

library(dplyr)

#Create a .txt file within the errors folder
clean_names_02 <- file(here("02_scripts","Errors", "02_clean_names.txt"), open = "wt")
sink(clean_names_02, type = "message")

#####selecting columns for self-report data at baseline and follow-up#####
##baseline
self_report_baseline<-self_report%>%
  select(group,breq3_id1:breq3_ext4,selfefficacy_1:selfefficacy_6, attitudes_i1:attitudes_a2, srbai1:srbai4,
         intention1a:intention_strength, pain_side, pain_both,pain_nrsl_v2.x, 
         ipaq_sl_job.x:ipaq_sl_semin.x, age_year:sociodems_post)
#View(self_report_baseline)

##follow-up
self_report_followup<-self_report%>%
  select(group, pain_nrsl_v2.y:ipaq_sl_semin.y)
#View(self_report_followup)

#####re-naming variables for IPAQ#####
##baseline
self_report_baseline_IPAQ<-select(self_report_baseline,group=group,
                                  PA_JOB_UNPAID_WRK=ipaq_sl_job.x,
                                  PA_WRK_VIG_FREQ= ipaq_sl_ovday.x, PA_WRK_VIG_TIME_HR=ipaq_sl_ovdhrs.x, PA_WRK_VIG_TIME_MIN=ipaq_sl_ovdmin.x,
                                  PA_WRK_MOD_FREQ=ipaq_sl_omday.x, PA_WRK_MOD_TIME_HR=ipaq_sl_omdhrs.x, PA_WRK_MOD_TIME_MIN=ipaq_sl_omdmin.x,
                                  PA_WRK_WALK_FREQ=ipaq_sl_owday.x, PA_WRK_WALK_TIME_HR=ipaq_sl_owdhrs.x, PA_WRK_WALK_TIME_MIN=ipaq_sl_owdmin.x,
                                  PA_TRANS_FREQ=ipaq_sl_tmday.x, PA_TRANS_TIME_HOUR=ipaq_sl_tmdhrs.x, PA_TRANS_TIME_MIN=ipaq_sl_tmdmin.x,
                                  PA_CYCLING_FREQ=ipaq_sl_tbday.x, PA_CYCLING_TIME_HR=ipaq_sl_tbwhrs.x, PA_CYCLING_TIME_MIN=ipaq_sl_tbwmin.x,
                                  PA_TRANS_WALK_FREQ=ipaq_sl_twday.x, PA_TRANS_WALK_TIME_HR=ipaq_sl_twdhrs.x, PA_TRANS_WALK_TIME_MIN=ipaq_sl_twdmin.x, 
                                  PA_GARDEN_VIG_FREQ=ipaq_sl_gvday.x, PA_GARDEN_VIG_TIME_HR=ipaq_sl_gvdhrs.x, PA_GARDEN_VIG_TIME_MIN=ipaq_sl_gvmin.x,
                                  PA_GARDEN_MOD_FREQ=ipaq_sl_gmday.x, PA_GARDEN_MOD_TIME_HR=ipaq_sl_gmdhrs.x, PA_GARDEN_MOD_TIME_MIN=ipaq_sl_gmdmin.x,
                                  PA_INSIDE_MOD_FREQ=ipaq_sl_hmday.x, PA_INSIDE_MOD_HR=ipaq_sl_hmdhrs.x, PA_INSIDE_MOD_MIN=ipaq_sl_hmdmin.x,
                                  PA_LEISURE_WALK_FREQ=ipaq_sl_lwday.x, PA_LEISURE_WALK_TIME_HR=ipaq_sl_lwdhrs.x, PA_LEISURE_WALK_TIME_MIN=ipaq_sl_lwdmin.x,
                                  PA_LEISURE_VIG_FREQ=ipaq_sl_lvday.x, PA_LEISURE_VIG_TIME_HR=ipaq_sl_lvdhrs.x, PA_LEISURE_VIG_TIME_MIN=ipaq_sl_lvdmin.x, 
                                  PA_LEISURE_MOD_FREQ=ipaq_sl_lmday.x, PA_LEISURE_MOD_TIME_HR=ipaq_sl_lmdhrs.x, PA_LEISURE_MOD_TIME_MIN=ipaq_sl_lmdmin.x) 

##follow-up
self_report_followup_IPAQ<-select(self_report_followup,group=group,
                                  PA_JOB_UNPAID_WRK=ipaq_sl_job.y,
                                  PA_WRK_VIG_FREQ= ipaq_sl_ovday.y, PA_WRK_VIG_TIME_HR=ipaq_sl_ovdhrs.y, PA_WRK_VIG_TIME_MIN=ipaq_sl_ovdmin.y,
                                  PA_WRK_MOD_FREQ=ipaq_sl_omday.y, PA_WRK_MOD_TIME_HR=ipaq_sl_omdhrs.y, PA_WRK_MOD_TIME_MIN=ipaq_sl_omdmin.y,
                                  PA_WRK_WALK_FREQ=ipaq_sl_owday.y, PA_WRK_WALK_TIME_HR=ipaq_sl_owdhrs.y, PA_WRK_WALK_TIME_MIN=ipaq_sl_owdmin.y,
                                  PA_TRANS_FREQ=ipaq_sl_tmday.y, PA_TRANS_TIME_HOUR=ipaq_sl_tmdhrs.y, PA_TRANS_TIME_MIN=ipaq_sl_tmdmin.y,
                                  PA_CYCLING_FREQ=ipaq_sl_tbday.y, PA_CYCLING_TIME_HR=ipaq_sl_tbwhrs.y, PA_CYCLING_TIME_MIN=ipaq_sl_tbwmin.y,
                                  PA_TRANS_WALK_FREQ=ipaq_sl_twday.y, PA_TRANS_WALK_TIME_HR=ipaq_sl_twdhrs.y, PA_TRANS_WALK_TIME_MIN=ipaq_sl_twdmin.y, 
                                  PA_GARDEN_VIG_FREQ=ipaq_sl_gvday.y, PA_GARDEN_VIG_TIME_HR=ipaq_sl_gvdhrs.y, PA_GARDEN_VIG_TIME_MIN=ipaq_sl_gvmin.y,
                                  PA_GARDEN_MOD_FREQ=ipaq_sl_gmday.y, PA_GARDEN_MOD_TIME_HR=ipaq_sl_gmdhrs.y, PA_GARDEN_MOD_TIME_MIN=ipaq_sl_gmdmin.y,
                                  PA_INSIDE_MOD_FREQ=ipaq_sl_hmday.y, PA_INSIDE_MOD_HR=ipaq_sl_hmdhrs.y, PA_INSIDE_MOD_MIN=ipaq_sl_hmdmin.y,
                                  PA_LEISURE_WALK_FREQ=ipaq_sl_lwday.y, PA_LEISURE_WALK_TIME_HR=ipaq_sl_lwdhrs.y, PA_LEISURE_WALK_TIME_MIN=ipaq_sl_lwdmin.y,
                                  PA_LEISURE_VIG_FREQ=ipaq_sl_lvday.y, PA_LEISURE_VIG_TIME_HR=ipaq_sl_lvdhrs.y, PA_LEISURE_VIG_TIME_MIN=ipaq_sl_lvdmin.y, 
                                  PA_LEISURE_MOD_FREQ=ipaq_sl_lmday.y, PA_LEISURE_MOD_TIME_HR=ipaq_sl_lmdhrs.y, PA_LEISURE_MOD_TIME_MIN=ipaq_sl_lmdmin.y) 
                          

#####selecting columns for IATs#####
IAT<-IAT%>%
  select(group:expressions.percentcorrect)%>%
  rename(subject=group)

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(clean_names_02)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "02_clean_names.txt"))
