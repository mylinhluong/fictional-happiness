#todo#
#run a previous script 01_import.R

##run R script from another R script#
system("cmd.exe", input = paste('"C:\\Program Files\\R\\R-3.6.1\\bin\\Rscript.exe" C:\\Users\\nobody\\Documents\\R\\MyScript.R'))

#Source R script from another R script
source("C:/Users/nobody/Documents/R/MyScript.R")
source("code1.r")


library(gdata)
library(dplyr)
library(readr)

convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, function(x) FUN1(as.character(x)))
  as.data.frame(out)
}

#selecting necessary columns
IAT<-NCP_OA_PA_IAT_raw %>%
  select(group:expressions.percentcorrect)%>%
  rename(subject=group)

##cutting user practice trials and dividing into separate measures## (another option is to select by )
#Automatic Evals#
IAT_eval<-IAT[which (IAT$blocknum==11|IAT$blocknum==13|IAT$blocknum==15|IAT$blocknum==17),]
View(IAT_eval)

#Automatic Self-Schema#
IAT_id<-IAT[which (IAT$blocknum==3|IAT$blocknum==5|IAT$blocknum==7|IAT$blocknum==9),]
View(IAT_id)

##making variables to identify compatible and incompatible trials
#Automatic Evals#
IAT_eval$PA.pleas <-ifelse(IAT_eval$blockcode =="compatibletest1AB"|IAT_eval$blockcode=="compatibletest2AB", 1,0)
IAT_eval$PA.unpleas <-ifelse(IAT_eval$blockcode =="incompatibletest1AB"|IAT_eval$blockcode=="incompatibletest2AB", 1,0)
IAT_eval$subject <- as.numeric(as.character(IAT_eval$subject))
IAT_eval$latency_eval <- as.numeric(as.character(IAT_eval$latency))
View(IAT_eval)

#Automatic Self-Schema#
IAT_id$PA.self <-ifelse(IAT_id$blockcode =="compatibletest1IY"|IAT_id$blockcode=="compatibletest2IY", 1,0)
IAT_id$PA.other <-ifelse(IAT_id$blockcode =="incompatibletest1IY"|IAT_id$blockcode=="incompatibletest2IY", 1,0)
IAT_id$subject <- as.numeric(as.character(IAT_id$subject))
IAT_id$latency_id <- as.numeric(as.character(IAT_id$latency))
View(IAT_id)

##making a dataset with one row per person
#Automatic Evals#
Subj_eval<-tapply(IAT_eval$subject, IAT_eval$subject,mean)
Subja_eval<-data.frame(Subj_eval)
BW_eval<-data.frame("subject"=Subja_eval$Subj_eval)
View(BW_eval)

#Automatic Self-Schema#
Subj_id<-tapply(IAT_id$subject, IAT_id$subject,mean)
Subja_id<-data.frame(Subj_id)
BW_id<-data.frame(subject=Subja_id$Subj_id)
View(BW_id)

##cutting outliers as per Richetin et al 2015 and Chevance et al 2016##

##function to calculate mean and sd of RTs
f <- function(x) c( iMean=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE), iTen=quantile(x, prob = .10), iNinety=quantile(x, prob = .90) )

#Automatic Evals#
iRT_eval=do.call( "rbind", tapply( IAT_eval$latency_eval, IAT_eval$subject, f ))
iid_eval=do.call( "rbind", tapply( IAT_eval$subject, IAT_eval$subject, f ))

iRT_eval<-data.frame(iRT_eval)
iid_eval<-data.frame(iid_eval)
iRT_eval$subject<-iid_eval$iMean

#Automatic Self-Schema#
iRT_id=do.call( "rbind", tapply( IAT_id$latency_id, IAT_id$subject, f ))
iid_id=do.call( "rbind", tapply( IAT_id$subject, IAT_id$subject, f ))

iRT_id<-data.frame(iRT_id)
iid_id<-data.frame(iid_id)
iRT_id$subject<-iid_id$iMean

##adding i.RT values to big IAT file
#Automatic Evals#
iIAT_eval<-merge(IAT_eval, iRT_eval, by="subject")
View(iIAT_eval)

#Automatic Self-Schema#
iIAT_id<-merge(IAT_id, iRT_id, by="subject")
View(iIAT_id)

##cutting outliers per individual
#Automatic Evals#
iIAT_eval$RTa<-ifelse(iIAT_eval$latency_eval<iIAT_eval$iTen.10., iIAT_eval$iTen.10., iIAT_eval$latency_eval)
iIAT_eval$RT<-ifelse(iIAT_eval$latency_eval>iIAT_eval$iNinety.90., iIAT_eval$iNinety.90., iIAT_eval$RTa)

#Automatic Self-Schema#
iIAT_id$RTa<-ifelse(iIAT_id$latency_id<iIAT_id$iTen.10., iIAT_id$iTen.10., iIAT_id$latency_id)
iIAT_id$RT<-ifelse(iIAT_id$latency_id>iIAT_id$iNinety.90., iIAT_id$iNinety.90., iIAT_id$RTa)

##making separate latency variables per compatible and incompatible trials
#Automatic Evals#
iIAT_eval$PApleas.RT<-ifelse(iIAT_eval$PA.pleas==1,iIAT_eval$RT,NA)
iIAT_eval$PAunpleas.RT<-ifelse(iIAT_eval$PA.unpleas==1,iIAT_eval$RT,NA)

#Automatic Self-Schema#
iIAT_id$PA.self.RT<-ifelse(iIAT_id$PA.self==1,iIAT_id$RT,NA)
iIAT_id$PA.other.RT<-ifelse(iIAT_id$PA.other==1,iIAT_id$RT,NA)

##Calculating mean RTs by block, pooled SD, mean difference score, and D-score##
#Automatic Evals#
Mean.PApleas<-tapply(iIAT_eval$PApleas.RT, iIAT_eval$subject, mean, na.rm=TRUE)
Mean.PAunpleas<-tapply(iIAT_eval$PAunpleas.RT, iIAT_eval$subject, mean,na.rm = TRUE)

SD_eval<-tapply(iIAT_eval$RT, iIAT_eval$subject, sd, na.rm = TRUE)
Mean.PApleas<-data.frame(Mean.PApleas)
Mean.PApleas$subject<-BW_eval$subject
Mean.PAunpleas<-data.frame(Mean.PAunpleas)
Mean.PAunpleas$subject<-BW_eval$subject
SD_eval<-data.frame(SD_eval)
SD_eval$subject<-BW_eval$subject
BWd_eval<-merge(Mean.PApleas, Mean.PAunpleas, by="subject")
BWd_eval<-merge(BWd_eval, SD_eval, by="subject")
BWd_eval$Diff_eval<-BWd_eval$Mean.PAunpleas-BWd_eval$Mean.PApleas
BWd_eval$d_eval<-BWd_eval$Diff_eval/BWd_eval$SD

#Automatic Self-Schema#
Mean.PA.self<-tapply(iIAT_id$PA.self.RT, iIAT_id$subject, mean, na.rm=TRUE)
Mean.PA.other<-tapply(iIAT_id$PA.other.RT, iIAT_id$subject, mean,na.rm = TRUE)

SD_id<-tapply(iIAT_id$RT, iIAT_id$subject, sd, na.rm = TRUE)
Mean.PA.self<-data.frame(Mean.PA.self)
Mean.PA.self$subject<-BW_id$subject
Mean.PA.other<-data.frame(Mean.PA.other)
Mean.PA.other$subject<-BW_id$subject
SD_id<-data.frame(SD_id)
SD_id$subject<-BW_id$subject
BWd_id<-merge(Mean.PA.self, Mean.PA.other, by="subject")
BWd_id<-merge(BWd_id, SD_id, by="subject")
BWd_id$Diff_id<-BWd_id$Mean.PA.other-BWd_id$Mean.PA.self
BWd_id$d_id<-BWd_id$Diff_id/BWd_id$SD

##Splitting data to calculate reliability and calculating two split-half D-scores#
#Automatic Evals#
iIAT_eval$rand<-runif(nrow(iIAT_eval))
iIAT_eval$PApleas.RT1<-ifelse(iIAT_eval$rand <= mean(iIAT_eval$rand), iIAT_eval$PApleas.RT, NA) 
iIAT_eval$PApleas.RT2<-ifelse(iIAT_eval$rand > mean(iIAT_eval$rand), iIAT_eval$PApleas.RT, NA) 
iIAT_eval$PAunpleas.RT1<-ifelse(iIAT_eval$rand <= mean(iIAT_eval$rand), iIAT_eval$PAunpleas.RT, NA) 
iIAT_eval$PAunpleas.RT2<-ifelse(iIAT_eval$rand > mean(iIAT_eval$rand), iIAT_eval$PAunpleas.RT, NA)
iIAT_eval$RT1<-ifelse(iIAT_eval$rand <= mean(iIAT_eval$rand), iIAT_eval$RT, NA) 
iIAT_eval$RT2<-ifelse(iIAT_eval$rand > mean(iIAT_eval$rand), iIAT_eval$RT, NA) 

Mean.PApleas.RT1<-tapply(iIAT_eval$PApleas.RT1, iIAT_eval$subject, mean, na.rm = TRUE)
Mean.PApleas.RT2<-tapply(iIAT_eval$PApleas.RT2, iIAT_eval$subject, mean, na.rm = TRUE)
Mean.PAunpleas.RT1<-tapply(iIAT_eval$PAunpleas.RT1, iIAT_eval$subject, mean, na.rm = TRUE)
Mean.PAunpleas.RT2<-tapply(iIAT_eval$PAunpleas.RT2, iIAT_eval$subject, mean, na.rm = TRUE)
SD_eval.RT1<-tapply(iIAT_eval$RT1, iIAT_eval$subject, sd, na.rm = TRUE)
SD_eval.RT2<-tapply(iIAT_eval$RT2, iIAT_eval$subject, sd, na.rm = TRUE)

BWreld_eval<-data.frame(BWd_eval$subject)
BWreld_eval$Mean.PApleas.RT1<-Mean.PApleas.RT1
BWreld_eval$Mean.PApleas.RT2<-Mean.PApleas.RT2
BWreld_eval$Mean.PAunpleas.RT1<-Mean.PAunpleas.RT1
BWreld_eval$Mean.PAunpleas.RT2<-Mean.PAunpleas.RT2
BWreld_eval$SD_eval.RT1<-SD_eval.RT1
BWreld_eval$SD_eval.RT2<-SD_eval.RT2
BWreld_eval$Diff_eval.RT1<-BWreld_eval$Mean.PAunpleas.RT1-BWreld_eval$Mean.PApleas.RT1
BWreld_eval$Diff_eval.RT2<-BWreld_eval$Mean.PAunpleas.RT2-BWreld_eval$Mean.PApleas.RT2
BWreld_eval$rel_eval.D1<-BWreld_eval$Diff_eval.RT1/BWreld_eval$SD_eval.RT1
BWreld_eval$rel_eval.D2<-BWreld_eval$Diff_eval.RT2/BWreld_eval$SD_eval.RT2
View(BWreld_eval)

#Automatic Self-Schema#
iIAT_id$rand<-runif(nrow(iIAT_id))
iIAT_id$PA.self.RT1<-ifelse(iIAT_id$rand <= mean(iIAT_id$rand), iIAT_id$PA.self.RT, NA) 
iIAT_id$PA.self.RT2<-ifelse(iIAT_id$rand > mean(iIAT_id$rand), iIAT_id$PA.self.RT, NA) 
iIAT_id$PA.other.RT1<-ifelse(iIAT_id$rand <= mean(iIAT_id$rand), iIAT_id$PA.other.RT, NA) 
iIAT_id$PA.other.RT2<-ifelse(iIAT_id$rand > mean(iIAT_id$rand), iIAT_id$PA.other.RT, NA)
iIAT_id$RT1<-ifelse(iIAT_id$rand <= mean(iIAT_id$rand), iIAT_id$RT, NA) 
iIAT_id$RT2<-ifelse(iIAT_id$rand > mean(iIAT_id$rand), iIAT_id$RT, NA) 
Mean.PA.self.RT1<-tapply(iIAT_id$PA.self.RT1, iIAT_id$subject, mean, na.rm = TRUE)
Mean.PA.self.RT2<-tapply(iIAT_id$PA.self.RT2, iIAT_id$subject, mean, na.rm = TRUE)
Mean.PA.other.RT1<-tapply(iIAT_id$PA.other.RT1, iIAT_id$subject, mean, na.rm = TRUE)
Mean.PA.other.RT2<-tapply(iIAT_id$PA.other.RT2, iIAT_id$subject, mean, na.rm = TRUE)
SD_id.RT1<-tapply(iIAT_id$RT1, iIAT_id$subject, sd, na.rm = TRUE)
SD_id.RT2<-tapply(iIAT_id$RT2, iIAT_id$subject, sd, na.rm = TRUE)

BWreld_id<-data.frame(BWd_id$subject)
BWreld_id$Mean.PA.self.RT1<-Mean.PA.self.RT1
BWreld_id$Mean.PA.self.RT2<-Mean.PA.self.RT2
BWreld_id$Mean.PA.other.RT1<-Mean.PA.other.RT1
BWreld_id$Mean.PA.other.RT2<-Mean.PA.other.RT2
BWreld_id$SD_id.RT1<-SD_id.RT1
BWreld_id$SD_id.RT2<-SD_id.RT2
BWreld_id$Diff_id.RT1<-BWreld_id$Mean.PA.other.RT1-BWreld_id$Mean.PA.self.RT1
BWreld_id$Diff_id.RT2<-BWreld_id$Mean.PA.other.RT2-BWreld_id$Mean.PA.self.RT2
BWreld_id$rel_id.D1<-BWreld_id$Diff_id.RT1/BWreld_id$SD_id.RT1
BWreld_id$rel_id.D2<-BWreld_id$Diff_id.RT2/BWreld_id$SD_id.RT2

View(BWreld_id)

##merging and cleaning datasets##
#Automatic Evals#
BWreldx_eval<-data.frame(BWd_eval$subject)
BWreldx_eval$rel_evalD1<-BWreld_eval$rel_eval.D1
BWreldx_eval$rel_evalD2<-BWreld_eval$rel_eval.D2
BWreldx_eval$subject<-BWd_eval$subject
BW_eval<-merge(BWd_eval, BWreldx_eval, by="subject")
BW_eval<-remove.vars(BW_eval, names="BWd_eval.subject")
View(BW_eval)


#Automatic Self-Schema#
BWreldx_id<-data.frame(BWd_id$subject)
BWreldx_id$rel_idD1<-BWreld_id$rel_id.D1
BWreldx_id$rel_idD2<-BWreld_id$rel_id.D2
BWreldx_id$subject<-BWd_id$subject
BW_id<-merge(BWd_id, BWreldx_id, by="subject")
BW_id<-remove.vars(BW_id, names="BWd_id.subject")
View(BW_id)

#Merge Automatic evals + Automatic self-schema together
IAT_processed_dscore<-merge(BW_eval, BW_id, by="subject")
View(IAT_processed_dscore)

##converts data to numeric
IAT_processed_dscore<-convert.magic(IAT_processed_dscore, "numeric")

#Write to CSV file
write.csv(IAT_processed_dscore,"IAT_processed_dscore.csv", row.names=FALSE)


self-report<-

View(IAT)