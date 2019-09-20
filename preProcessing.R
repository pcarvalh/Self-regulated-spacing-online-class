## Pre Processing Script ----
# This script preprocesses the raw data.

rm(list=ls())

path = "~/selfregulated_spacing/"

setwd(path)

##load libraries
library(plyr)
library(lattice)
library(reshape2)
library(data.table)
library(sciplot)
library(tidyr)
library(lmerTest)
library(lme4)
library(sciplot)
library(sjPlot)

##load functions
source(file = "functions/addModuleInfo.R")
source(file = "functions/calculateTimesCounts.R")

#### run the following lines to get clean datasets -------------------------------------------------------
##import counts
Act_Word_Count = read.csv(file = "data/counts_summary.csv",header = T)

##OLI stream data
D.oli = fread(paste(path,"data/oli_query_result.txt",sep = ""),header = T,stringsAsFactors = F)

##import activity performance data
D.activitiy = fread(paste(path,"data/ds863_tx_All_Data_2287_2015_0813_191150.txt",sep = ""),header = T,stringsAsFactors = F)

##import test data
outcomeMeasures = fread(paste(path,"data/OLIMOOCTimeQuizData.txt",sep = ""),header = T,stringsAsFactors = F)
outcomeMeasures = as.data.frame(outcomeMeasures)

##change of subject column
names(outcomeMeasures)[1] = "ds_anon_user_id"

#students with exam data
length(unique(outcomeMeasures$ds_anon_user_id))
length(unique(D.oli$ds_anon_user_id))

students_with_outcome_measure = unique(outcomeMeasures$ds_anon_user_id)

## remove students without outcome measures
D.oli = D.oli[which(D.oli$ds_anon_user_id%in%students_with_outcome_measure),]
length(unique(D.oli$ds_anon_user_id))

D.activitiy = D.activitiy[which(D.activitiy$`Anon Student Id`%in%students_with_outcome_measure),]
length(unique(D.oli$ds_anon_user_id))

D.oli$Time2 = as.POSIXct(D.oli$server_receipt_time, format="%Y-%m-%d %H:%M:%S",dz="America/New_York",usetz=TRUE)
D.oli = D.oli[order(D.oli$ds_anon_user_id,D.oli$Time2),]

####### remove unnecessary columns from D.oli -------
D.oli = as.data.frame(D.oli)
myvars <- c("ds_anon_user_id", "sess_ref", "transaction_time","action","info","Time2")
D = D.oli[myvars]

#write.csv(x = D, file = "cleanupData.csv",row.names = F)

#####  for each student, get date they took each quiz and exam ----
#load date quizzes were taken
pretestDates = read.csv(file = "quizDates and byItem grades/psy-001 Quiz (53) prestest responses_DS_anonymous_id.csv",header=F,skip=3,stringsAsFactors = F)

ExamDates = read.csv(file = "quizDates and byItem grades/psy-001 Quiz (197) final responses_DS_anonymous_id.csv",header=F,skip=3,stringsAsFactors = F)

quiz1 = "quizDates and byItem grades/psy-001 Quiz (121) quiz 1 responses_DS_anonymous_id.csv"
quiz2 = "quizDates and byItem grades/psy-001 Quiz (137) quiz 2 responses_DS_anonymous_id.csv"
quiz3 = "quizDates and byItem grades/psy-001 Quiz (147) quiz 3 responses_DS_anonymous_id.csv"
quiz4 = "quizDates and byItem grades/psy-001 Quiz (149) quiz 4 responses_DS_anonymous_id.csv"
quiz5 = "quizDates and byItem grades/psy-001 Quiz (151) quiz 5 responses_DS_anonymous_id.csv"
quiz6 = "quizDates and byItem grades/psy-001 Quiz (155) quiz 6 responses_DS_anonymous_id.csv"
quiz7 = "quizDates and byItem grades/psy-001 Quiz (159) quiz 7 responses_DS_anonymous_id.csv"
quiz8 = "quizDates and byItem grades/psy-001 Quiz (161) quiz 8 responses_DS_anonymous_id.csv"
quiz9 = "quizDates and byItem grades/psy-001 Quiz (163) quiz 9 responses_DS_anonymous_id.csv"
quiz10 = "quizDates and byItem grades/psy-001 Quiz (165) quiz 10 responses_DS_anonymous_id.csv"
quiz11 = "quizDates and byItem grades/psy-001 Quiz (195) quiz 11 responses_DS_anonymous_id.csv"

##enter pretest date to the dataset
names(pretestDates)[c(1,3)] = c(names(D)[1],"Time")
pretestDates = pretestDates[,c(1,3)]

##get date for first time students took pretest
setDT(pretestDates)[, rep := seq_len(.N), by=rleid(ds_anon_user_id)]
pretestDates = pretestDates[pretestDates$rep==1,]

pretestDates$Time2 = as.POSIXct(pretestDates$Time, format="%a %d %b %Y %I:%M %p", tz="UTC")

pretestDates$pretestTime = format(pretestDates$Time2, tz="America/New_York",usetz=TRUE)

D = join(D,pretestDates[,c(1,5)])

rm(pretestDates)

## add exam date
names(ExamDates)[c(1,3)] = c(names(D)[1],"Time")
ExamDates = ExamDates[,c(1,3)]

#get the first time students completed the exam only
setDT(ExamDates)[, rep := seq_len(.N), by=rleid(ds_anon_user_id)]
ExamDates = ExamDates[ExamDates$rep==1,]

ExamDates$Time2 = as.POSIXct(ExamDates$Time, format="%a %d %b %Y %I:%M %p", tz="UTC")

ExamDates$ExamTime = format(ExamDates$Time2, tz="America/New_York",usetz=TRUE)

D = join(D,ExamDates[,c(1,5)])

rm(ExamDates)

## add quiz dates
quizlist = c(quiz1,quiz2,quiz3,quiz4,quiz5,quiz6,quiz7,quiz8,quiz9,quiz10,quiz11)

for(i in 1:length(quizlist)){
  quiz = read.csv(file = quizlist[i],header=F,skip=3,stringsAsFactors = F)
  names(quiz)[c(1,3)] = c(names(D)[1],"Time")
  quiz = quiz[,c(1,3)]
  setDT(quiz)[, rep := seq_len(.N), by=rleid(ds_anon_user_id)]
  quiz = quiz[quiz$rep==1,]
  
  quiz$Time2 = as.POSIXct(quiz$Time, format="%a %d %b %Y %I:%M %p", tz="UTC")
  
  quiz$quizTime = format(quiz$Time2, tz="America/New_York",usetz=TRUE)
  quiz$module = i
  
  if(exists("quizDates")){
    quizDates = rbind(quizDates,quiz[,c(1,5,6)])
  }else{
    quizDates = quiz[,c(1,5,6)]
  }
}

#### add module information to the data. ------
D = addModuleInfo(D)
D$module = ifelse(is.na(D$module),300,D$module)

LSpages_Activities = c("_u01_m01_metacog_1","_u01_m01_metacog_2","_u01_m01_metacog_3","_u01_m01_metacog_openfree_1","u0_devskills1_tutor1","u0_devskills2_tutor1","u0_devskills2_tutor2","u0_devskills2_tutor3","u0_devskills2_tutor4")
  
##add learning strategies
D$module = ifelse(D$info %in% LSpages_Activities,0,D$module)

##add quiz dates
D = join(D,quizDates)

##remove all quizzes without quizTime (how is this even possible?)
D = D[-which(is.na(D$quizTime)),]
D = D[order(D$ds_anon_user_id,D$Time2),] #always reorder by timestamp and student to maintain ordered sequence! 

##find the first and last event (page or activity) for each module.
d = D[!D$module%in%c(0,100,300) & D$Time2<D$quizTime,] #we only care for real modules, so no LS (0) or system stuff (100,300) and events need to happen before the quiz.
d = d[order(d$ds_anon_user_id,d$Time2),] #always reorder by timestamp and student to maintain ordered sequence! 
#d = d[-which(is.na(d$Time2)),] #remove any empty cells in the db

d$numRow = rep(1:max(length(d$info))) #number all db in the file for later reference.
setDT(d)[, first := seq_len(.N), by=rleid(ds_anon_user_id,module)] #number events per module and per student
setDT(d)[, runlength := max(.N), by=rleid(ds_anon_user_id,module)] #find the number of the last event per module and student
dd = d[d$first==1,] #keep only the first step of each event.
dd$LastRowOfRun = as.numeric(dd$numRow) + as.numeric(dd$runlength) - 1  #add what the last row of the run is for each event.


lines = dd$LastRowOfRun #find the number for the last event for each student and module
endTime = d[which(d$numRow %in% lines),'Time2'] #get the time of that last event from d
dd$endTime = endTime #add that time to dd (which right now only has the very first event)

dd$timeInModule = difftime(dd$endTime,dd$Time2,units="mins") #calculate time in module (last event minus first event)

dd$endToQuiz = difftime(dd$quizTime,dd$endTime,units="mins") #claculate how long between finishing the module and taking the quiz.

##find the time diference between each event and the quiz per student and module.
D$timeToQuiz = difftime(D$quizTime,D$Time2,units="mins")

##identify events (the rows) in D that are after the quizTime (for later exclusion)
D$exclude = ifelse(D$Time2>D$quizTime,"exclude","include")

##get time between sessions
#D3 = ddply(.data = D, .variable = .(ds_anon_user_id,module,sess_ref,Time2),.fun = summarize,n=length(ds_anon_user_id),.progress = "text")

D3 <- setDT(D)[,.(n=.N),by=.(ds_anon_user_id,module,sess_ref,Time2)]
setDT(D)[, first := seq_len(.N), by=rleid(ds_anon_user_id,module,sess_ref)] #number events per module and per student

D3 = D[D$first==1,]
D3$session_time_diff = append(0,diff(D3$Time2)) #this is SECONDS.
ds_anon_user_id = D3$ds_anon_user_id
nextStudent = c(tail(ds_anon_user_id,-1),NA)
prevStudent = c(NA, head(ds_anon_user_id, -1))

D3$prevStudent = prevStudent

sumD3 <- setDT(D3)[which(D3$ds_anon_user_id==D3$prevStudent),.(meanSessionDiff=mean(session_time_diff)),by=.(ds_anon_user_id,module)]
#sumD3 = ddply(.data = D3[which(D3$ds_anon_user_id==D3$prevStudent),],.variables = .(ds_anon_user_id,module),.fun = summarize,meanSessionDiff=mean(session_time_diff))

##calculate summarized data for analyses!
#SumD = ddply(.data = D[D$exclude=="include",],.variables = .(ds_anon_user_id,module),.fun=summarize,meantimeToQuiz=mean(timeToQuiz),nsessions = length(unique(sess_ref)),ndays = length(unique(format(Time2,"%d"))),npages=length(info[action=="VIEW_PAGE"]),nactivities=length(unique(info[!action=="VIEW_PAGE"]))) #timeToQuiz

SumD <- setDT(D)[exclude=="include",.(meantimeToQuiz=mean(timeToQuiz),nsessions = length(unique(sess_ref)),ndays = length(unique(format(Time2,"%d"))),npages=length(info[action=="VIEW_PAGE"]),nactivities=length(unique(info[!action=="VIEW_PAGE"]))),by=.(ds_anon_user_id,module)]

#SumD2 = ddply(.data = dd,.variables = .(ds_anon_user_id,module),.fun=summarize,timeInModule=mean(timeInModule),endToQuiz = mean(endToQuiz)) #timeInModule and endToQuiz

SumD2 = setDT(dd)[,.(timeInModule=mean(timeInModule),endToQuiz = mean(endToQuiz)),by=.(ds_anon_user_id,module)]

#SumD3 = ddply(.data = D, .variable = .(ds_anon_user_id,module,sess_ref),.fun = summarize,meanTime=mean())

SummarySpacingTime = join(SumD,SumD2) #join both datasets
SummarySpacingTime = join(SummarySpacingTime,sumD3)

##add the outcome measures
for(i in 1:nrow(SummarySpacingTime)){
  SummarySpacingTime$quizGrade[i] = outcomeMeasures$`raw score`[outcomeMeasures$quiz_num==SummarySpacingTime$module[i]&outcomeMeasures$ds_anon_user_id==SummarySpacingTime$ds_anon_user_id[i]]
  SummarySpacingTime$pretestGrade[i] = outcomeMeasures$`raw score`[outcomeMeasures$quiz_num==0&outcomeMeasures$ds_anon_user_id==SummarySpacingTime$ds_anon_user_id[i]]
  SummarySpacingTime$examGrade[i] = outcomeMeasures$`raw score`[outcomeMeasures$quiz_num==12&outcomeMeasures$ds_anon_user_id==SummarySpacingTime$ds_anon_user_id[i]]
}

##add count of pages and activities
SummarySpacingTime = join(SummarySpacingTime,Act_Word_Count) #note upper case N is available sutff, lower case is completed by the students

##classify students into crammers or spacers by module
SummarySpacingTime$crammers = ifelse(SummarySpacingTime$nsessions==1,"Crammer","Non-Crammer")

##aggregate things to analyze exam scores
aggSummarySpacingTime = setDT(SummarySpacingTime)[,.(meantimeToQuiz=mean(meantimeToQuiz,na.rm=T),nsessions=mean(nsessions,na.rm=T),ndays=mean(ndays,na.rm=T),npages=mean(npages,na.rm=T),nactivities=mean(nactivities,na.rm=T),timeInModule=mean(timeInModule,na.rm=T),endToQuiz=mean(endToQuiz,na.rm=T),meanSessionDiff=mean(meanSessionDiff,na.rm=T),pretestGrade=mean(pretestGrade,na.rm=T),examGrade=mean(examGrade,na.rm=T)),by=.(ds_anon_user_id)]

#aggSummarySpacingTime = ddply(.data = SummarySpacingTime,.variables = .(ds_anon_user_id),.fun = summarize,meantimeToQuiz=mean(meantimeToQuiz,na.rm=T),nsessions=mean(nsessions,na.rm=T),ndays=mean(ndays,na.rm=T),npages=mean(npages,na.rm=T),nactivities=mean(nactivities,na.rm=T),timeInModule=mean(timeInModule,na.rm=T),endToQuiz=mean(endToQuiz,na.rm=T),mean

## save data as spreadsheets for future reference if needed
write.csv(SummarySpacingTime,"SummarySpacingTime.csv",row.names = F)
write.csv(aggSummarySpacingTime,"aggSummarySpacingTime.csv",row.names = F)

##clean environment for analyses
rm(list=setdiff(ls(), c("SummarySpacingTime","aggSummarySpacingTime")))

SessionDiff=mean(meanSessionDiff,na.rm=T),pretestGrade=mean(pretestGrade,na.rm=T),examGrade=mean(examGrade,na.rm=T))