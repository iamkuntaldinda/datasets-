# Survival Analysis in R
# Copyright 2013 by Ani Katchova

# install.packages("survival")
library(survival)

mydata<- read.csv("survival_unemployment.csv")
attach(mydata)
dim(mydata)
str(mydata)

View(mydata)
# Define variables 
time <- spell
event <- event
X <- cbind(logwage, ui, age)
group <- ui

# Descriptive statistics
summary(time)
summary(event)
summary(X)
summary(group)

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")

# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)


# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS
exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)

############################################################33

df <- read.csv('leukemia.csv')
str(df)
df$sex <- as.factor(df$sex)
dim(df)
sex <- df$sex

require(fastDummies)
df <- fastDummies::dummy_cols(df, select_columns = "sex")
df$sex <- as.factor(df$sex)
#df$sex <- NULL
require(ggplot2)
df$status <- as.factor(df$status)
df$Rx <- as.factor(df$Rx)

colnames(df)
str(df)
g <- ggplot(df, aes(logWBC,Rx))
g + geom_bar(stat = "identity",aes(fill = status)) + coord_flip() 

g <- ggplot(df, aes(Rx,status))
g + geom_bar(stat = "identity",aes(fill = Rx)) + coord_flip() 

g <- ggplot(df, aes(Rx,status))
g + geom_bar(stat = "identity",aes(fill = Rx)) + coord_flip() 


##############################################
colnames(df)
attach(df)
# Define variables 
time <- survival.times
event <- status
X <- cbind(logWBC,Rx,sex)
group <- Rx

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability",col = "blue")

# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

###3 classes
x <- df$logWBC
logWBC <- ifelse((x>=0) & (x<=2.30),"low",
               ifelse((x>=2.31) & (x<3),"medium",
                      ifelse((x>=3),"high","na")))

df$logWBC <- logWBC
g <- df$logWBC

# Kaplan-Meier non-parametric analysis by  Logwbc group
kmsurvival1 <- survfit(Surv(time, event) ~ g)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")


##Fit the Cox PH model that can be used to assess the relationship of interest, which considers the potential confounders Sex and logWBC.
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)







