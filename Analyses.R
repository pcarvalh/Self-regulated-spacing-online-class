## Analyses Script ----
# This script includes analyses of the data.

## clean things up. ----
rm(list=ls())

path = "~/Dropbox/Work/Projects/Datashop Learning Strategies/"

setwd(path)

##load libraries -----
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

##load preprocessing to get data -------
#source(file = "functions/addModuleInfo.R")
#source(file = "functions/calculateTimesCounts.R")
source(file = "preProcessing.R")

## OR load file with data ------
SummarySpacingTime <- fread("SummarySpacingTime.csv")
aggSummarySpacingTime <- fread("aggSummarySpacingTime.csv")

### Data inspection
library(psych)
describe(SummarySpacingTime,na.rm=T)

library(ggplot2)
a <- ggplot(SummarySpacingTime, aes(x=nsessions)) + geom_histogram(color="black", fill="white") + geom_vline(aes(xintercept=mean(nsessions)),color="black", linetype="dashed", size=1) + labs(title = "(a) Number of Sessions",y="Number of Student x Unit records",x="Number of sessions")

b <- ggplot(SummarySpacingTime, aes(x=log(as.numeric(timeInModule)))) + geom_histogram(color="black", fill="white") + geom_vline(aes(xintercept=mean(log(as.numeric(timeInModule)),na.rm=T)),color="black", linetype="dashed", size=1) + labs(title = "(b) Total Time in the Module",y="Number of Student x Unit records",x="Log of Total time in the Unit")

c <- ggplot(SummarySpacingTime, aes(x=log(as.numeric(endToQuiz)))) + geom_histogram(color="black", fill="white") + geom_vline(aes(xintercept=mean(log(as.numeric(endToQuiz)),na.rm=T)),color="black", linetype="dashed", size=1) + labs(title = "(c) Retention Interval",y="Number of Student x Unit records",x="Log of Retention Interval")

d <- ggplot(SummarySpacingTime, aes(x=quizGrade)) + geom_histogram(color="black", fill="white") + geom_vline(aes(xintercept=mean(quizGrade)),color="black", linetype="dashed", size=1) + labs(title = "(d) Quiz Grade",y="Number of Student x Unit records",x="Quiz Grade")

e <- ggplot(SummarySpacingTime, aes(x=nactivities)) + geom_histogram(color="black", fill="white") + geom_vline(aes(xintercept=mean(nactivities)),color="black", linetype="dashed", size=1) + labs(title = "(e) Number of Activities Completed",y="Number of Student x Unit records",x="Number of Activities")

f <- ggplot(SummarySpacingTime, aes(x=log(as.numeric(meanSessionDiff)))) + geom_histogram(color="black", fill="white") + geom_vline(aes(xintercept=mean(log(as.numeric(meanSessionDiff)),na.rm=T)),color="black", linetype="dashed", size=1) + labs(title = "(f) Mean Time Between Sessions",y="Number of Student x Unit records",x="Log Mean time between sessions")

library(ggpubr)
ggarrange(plotlist = list(a,b,c,d,e,f),nrow=3,ncol=2)

#### ANALYSES AND PLOTS -------------------------------

#what is the percentage of crammers (this is per module, so out of all student/module)
length(SummarySpacingTime$ds_anon_user_id[SummarySpacingTime$ndays==1])/length(SummarySpacingTime$ds_anon_user_id)

#What is the relationship between spacing and quiz performance?
summary(m1 <-  lmer(scale(quizGrade)~scale(pretestGrade)+scale(timeInModule)+scale(endToQuiz)*scale(nsessions) + (1|ds_anon_user_id) + (1|module),data=SummarySpacingTime))

summary(m1_alt <-  lm(scale(examGrade)~scale(pretestGrade)+scale(timeInModule)+scale(endToQuiz)*scale(nsessions),data=aggSummarySpacingTime))

#Plots to analyze interactions ---------
### second plot for the paper
library(dplyr)
library(misc)
library(Rmisc)

dbs <- mutate(SummarySpacingTime,quantile_exam = ntile(SummarySpacingTime$examGrade,5),quantile_nsessions=ntile(SummarySpacingTime$nsessions,5))
dbs <- as.data.table(dbs)

dbs_sum <- dbs[,.(grade=mean(quizGrade),nsessions_avg=mean(nsessions),n=.N,confInt_lower=CI(quizGrade,0.95)[1],confInt_upper=CI(quizGrade,0.95)[3]),by=.(quantile_exam,quantile_nsessions)]

dbs_sum$quantile_exam <- as.factor(dbs_sum$quantile_exam)

library(ggplot2)
a <- ggplot(data=dbs_sum, aes(x=nsessions_avg, y=grade, group=quantile_exam)) + geom_line(aes(linetype=quantile_exam)) + geom_point(aes(shape=quantile_exam)) + theme_classic() + xlab("Number of Sessions") + ylab("Quiz Grade") + scale_linetype(name="Exam Grade Quantile") + scale_shape_discrete(name="Exam Grade Quantile") + theme(legend.justification=c(1,0), legend.position="top") + theme(legend.justification=c(1,0), legend.position="top",plot.title = element_text(hjust = 0.5),text = element_text(size=16),axis.text = element_text(size=16)) + ggtitle("Relation between Performance and Number of Sessions") + coord_cartesian(ylim=c(7,10)) + geom_errorbar(aes(ymin=confInt_lower, ymax=confInt_upper), colour="black", width=.1) 

dbs2 <- mutate(SummarySpacingTime,quantile_spacing = ntile(SummarySpacingTime$nsessions,5),quantile_activities=ntile(SummarySpacingTime$nactivities,5))
dbs2 <- as.data.table(dbs2)

dbs2_sum <- dbs2[,.(grade=mean(quizGrade),nactivities_avg=mean(nactivities),n=.N,confInt_lower=CI(quizGrade,0.95)[1],confInt_upper=CI(quizGrade,0.95)[3]),by=.(quantile_spacing,quantile_activities)]

dbs2_sum$quantile_spacing <- as.factor(dbs2_sum$quantile_spacing)

library(ggplot2)
b <- ggplot(data=dbs2_sum, aes(x=nactivities_avg, y=grade, group=quantile_spacing)) + geom_line(aes(linetype=quantile_spacing)) + geom_point(aes(shape=quantile_spacing)) + theme_classic() + xlab("Activities Completed") + ylab("Quiz Grade") + scale_linetype(name="Number of Sessions Quantile") + scale_shape_discrete(name="Number of Sessions Quantile") + theme(legend.justification=c(1,0), legend.position="top") + theme(legend.justification=c(1,0), legend.position="top",plot.title = element_text(hjust = 0.5),text = element_text(size=16),axis.text = element_text(size=16)) + ggtitle("Relation between Activity Completion and Number of Sessions") + coord_cartesian(ylim=c(7,10)) + geom_errorbar(aes(ymin=confInt_lower, ymax=confInt_upper), colour="black", width=.1) 


library(ggpubr)
ggarrange(plotlist = list(a,b),nrow=1,ncol=2)

#Do learners of different ability levels make different spacing decisions, and do these matter for learning? 
summary(m2_0 <-  lmer(formula =  scale(nsessions) ~ scale(timeInModule) + scale(pretestGrade) + scale(examGrade) + (1|module) + (1|ds_anon_user_id),data=SummarySpacingTime))

D = SummarySpacingTime
D$zquizGrade <- scale(D$quizGrade)
D$znsessions <- scale(D$nsessions)
D$znactivities <- scale(D$nactivities)
D$zexamGrade <- scale(D$examGrade) 
D$zendToQuiz <- scale(D$endToQuiz)     
D$zpretestGrade <- scale(D$pretestGrade) 
D$ztimeInModule <- scale(D$timeInModule) 

m2 <- lmer(formula = zquizGrade ~ ztimeInModule + zpretestGrade + znsessions*zexamGrade + (1|module) + (1|ds_anon_user_id),data=D)
summary(m2)

#p1 <- sjp.int(m2_0, type = "eff",geom.colors = "bw",p.kr = FALSE,int.term = "zexamGrade*znsessions")
p1 <- plot_model(m2,type = "int",terms="znsessions*zexamGrade",colors="bw",title="Interaction between exam grade and spacing",axis.title = c("z-score Number of Sessions","Predicted Quiz grade (z-score)"),legend.labels=c("Low","High"))

m2_alt <- lmer(formula = zquizGrade ~ ztimeInModule + zpretestGrade*znsessions + (1|module) + (1|ds_anon_user_id),data=D)
summary(m2_alt)

### Do learners with different practice levels make different spacing decisions, and do these matter for learning?  ---
summary(m3_0 <-  lmer(formula =  scale(nsessions) ~ scale(timeInModule) + scale(pretestGrade) + scale(nactivities) + (1|module) + (1|ds_anon_user_id),data=SummarySpacingTime))

m3 <- lmer(formula = zquizGrade ~ znactivities*znsessions + ztimeInModule + zpretestGrade + (1|module) + (1|ds_anon_user_id),data=D)
summary(m3)

p1 <- plot_model(m3,type = "int",terms="znactivities*znsessions",colors="bw",title="Interaction between Practice Activities and Spacing",axis.title = c("z-score Number of Activities","Predicted Quiz grade (z-score)"))

#relation between doing and session duration
mean(SummarySpacingTime$timeInModule[SummarySpacingTime$nactivities>mean(SummarySpacingTime$nactivities,na.rm=T)])

mean(SummarySpacingTime$nsessions[SummarySpacingTime$nactivities>mean(SummarySpacingTime$nactivities,na.rm=T)])

mean(SummarySpacingTime$timeInModule[SummarySpacingTime$nactivities<=mean(SummarySpacingTime$nactivities,na.rm=T)])

mean(SummarySpacingTime$nsessions[SummarySpacingTime$nactivities<=mean(SummarySpacingTime$nactivities,na.rm=T)])

### get correlations between variables at the subject level
tempD <- ddply(.data = SummarySpacingTime,.variables = .(ds_anon_user_id),.fun = summarize,meanQuizGrade = mean(quizGrade),pretestGrade=mean(pretestGrade,na.rm=T))
sumD = join(aggSummarySpacingTime[,c(1,11)],tempD)

cor.test(x = sumD$pretestGrade,y = sumD$meanQuizGrade)
cor.test(x = sumD$examGrade,y = sumD$meanQuizGrade)
cor.test(x = sumD$pretestGrade,y = sumD$examGrade)

summary(lm(scale(examGrade)~scale(pretestGrade)+scale(timeInModule)+scale(endToQuiz)*scale(nsessions)+scale(nsessions),data=aggSummarySpacingTime))

### Multicolinearity check -----
summary(multimodel <- lmer(scale(nsessions)~scale(timeInModule)+scale(pretestGrade)+(1|module)+ (1|ds_anon_user_id),data=SummarySpacingTime))

library(MuMIn)
r.squaredGLMM(multimodel)
