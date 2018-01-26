#import dataset: didact dataset from Combs et al 2014
#subjid: deidentified subjects
#inf5: – intraamniotic	infection/inflammation	status	(1=infection,	2=severe	inflammation,	3=mild inflammation,	4=colonization,	5=negative	fluid)
#days: number	of	days between	consent	(which	is	when	amniocentesis	was	done	to	obtain	the	amniotic	fluid	that	was	tested	to	establish	inf5) and	delivery 	(or	censoring)
#deliver: delivery status before censoring
#delcensor: delivery status with censoring (1=delivery date known/observed; 0=specified after number of days of follow-up)
library(readr)
library(survival)
library(survminer)
IAI_HW1 <- read_delim("~/Documents/2017 - 2018/Winter 2017/Survival Analysis/HW/HW1/IAI_HW1.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
IAI_HW1$delcensor <- as.numeric(IAI_HW1$delcensor)

#Survival data declared (part a, hw#1)
IAI_HW1$censor_status <- factor(IAI_HW1$delcensor,levels=c(0,1),labels=c("censored","observed"))
survival <- Surv(IAI_HW1$days, IAI_HW1$delcensor)
m1 <- survfit(survival ~ IAI_HW1$inf5) 
head(IAI_HW1)

#Survival data declared for (partb, hw#1)
#Restrict dataset to inf5=1 and inf5=2 as separate subsets 
sac <- subset(IAI_HW1, inf5 == 1 | inf5 ==2 , select=c(subjid,inf5,days,deliver,delcensor,censor_status)) 
#Restrict to inf5=3 and if5=5 (negative)
sac2 <- subset(IAI_HW1, inf5 == 3 | inf5 ==5 , select=c(subjid,inf5,days,deliver,delcensor,censor_status)) 
#select all columns if inf5 equals 1 or 2
#declare survival data for these subsetse
survsac <- Surv(sac$days,sac$delcensor)
survsac2 <- Surv(sac2$days,sac2$delcensor)
m2 <- survfit(survsac ~ sac$inf5)
m3 <- survfit(survsac2 ~ sac2$inf5)

#Survival data plot, limited to inf5=1 and inf5=2 (without c.i) - partb
ggsurvplot(m2,data=sac,conf.int=FALSE,legend.title=c("Amniotic sac status"),legend.labs=c("infection","severe inflammation"),xlab="Time between consent and delivery (days)",title="KM curve of set from Comb et al (2014)")

#Survival data plot, limited to inf5=1 and inf5=2 (with c.i) - partb
ggsurvplot(m2,data=sac,conf.int=TRUE,legend.title=c("Amniotic sac status"),legend.labs=c("infection","severe inflammation"),xlab="Time between consent and delivery (days)",title="KM curve of set from Comb et al (2014)")

#Survival table summary - part c
summary(m2)

#Quantile table - part d
quantile(m1)

#Survival data plot, limited to inf5=3 and inf5=5 (without c.i) - part e
ggsurvplot(m2,data=sac,conf.int=FALSE,legend.title=c("Amniotic sac status"),legend.labs=c("mild inflammation","negative fluid"),xlab="Time between consent and delivery (days)",title="KM curve of set from Comb et al (2014)")

ggsurvplot(m2,data=sac,conf.int=TRUE,legend.title=c("Amniotic sac status"),legend.labs=c("mild inflammation","negative fluid"),xlab="Time between consent and delivery (days)",title="KM curve of set from Comb et al (2014)")

#Survival data plot for all groups - part f
ggsurvplot(m1,data=IAI_HW1,conf.int=TRUE,legend.title=c("Amniotic sac status"),legend.labs=c("infection", "severe inflammation","mild inflammation","colonization","negative fluid"),xlab="Time between consent and delivery (days)",title="KM curve of set from Comb et al (2014)")

ggsurvplot(m1,data=IAI_HW1,conf.int=FALSE,legend.title=c("Amniotic sac status"),legend.labs=c("infection", "severe inflammation","mild inflammation","colonization","negative fluid"),xlab="Time between consent and delivery (days)",title="KM curve of set from Comb et al (2014)")