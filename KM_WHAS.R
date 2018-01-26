#Retrieve WHAS data - directory pre-set
library(lubridate)
library(survival)
whas100 <- read.delim("whas100.dat", sep="",header= FALSE)
colnames(whas100) <- c("ID","AdDate","FolDate","HosLenStay","FolTime","Deceased","Age","Gender","BMI")
head(whas100)

division <- function(x) {
  return (x/365)
}

#Status and gender defined
#Vital Status: 1= Dead, 0=Alive; Gender:0=Male, 1=Female
whas100$FolStatus <- factor(whas100$Deceased,levels=c(0,1),labels=c("alive","dead"))
whas100$Gender <- factor(whas100$Gender,levels=c(0,1),labels=c("male","female"))
whas100$Years <- as.numeric(lapply(whas100$FolTime,division))

#plot
psymbol <- whas100$Deceased #squares = 0 (alive or uncensored (censor2=1)), circle = 1 (dead or uncensored (censor2=1))
#note, survreg call does not work with factors...throws multi-state survival is not supported error
fit <- survreg(formula = Surv(whas100$Years, whas100$Deceased,type=c('right')) ~ whas100$Age, data=whas100, dist = "exponential")
plot(whas100$Years ~ whas100$Age, xlab='Age', ylab='Years', pch=psymbol)
tt <- seq(0, max(whas100$Age), length = 100)
lines(tt, predict(fit, list(Time = tt)))

#Plot K-M estimate
whas100$agenew <- 1000/whas100$Age
survival_data <- survfit(Surv(whas100$FolTime,whas100$FolStatus) ~ 1) 
summary(survival_data)
plot(survival_data)
