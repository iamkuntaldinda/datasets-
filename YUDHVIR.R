
require(MASS)
require(pscl)
require(AER)

whaledata <- read.csv('whale.csv')
whaledata2 <- whaledata

whaledata <- na.omit(whaledata)
as.factor(whaledata$Daytime.Hudson) -> whaledata$Daytime.Hudson
as.factor(whaledata$Daytime.Casey) -> whaledata$Daytime.Casey

sapply(whaledata, mean, na.rm=TRUE)
summary(whaledata)


require(Hmisc)
ggplot(whaledata, aes(whaledata$Bouts.Hudson, fill = Daytime.Hudson)) +
  geom_histogram(binwidth=2, position="dodge")+ ggtitle("Count of Bouts")
ggplot(whaledata, aes(whaledata$Lockons.Hudson, fill = Daytime.Hudson)) +
  geom_histogram(binwidth=2, position="dodge") + ggtitle("Count of Lockons")
ggplot(whaledata, aes(whaledata$Lockons.Casey, fill = Daytime.Casey)) +
  geom_histogram(binwidth=2, position="dodge") + ggtitle("Count of Lockons")


plot(density(whaledata$Lockons.Hudson) ) # plots the results
plot(density(whaledata$Lockons.Casey) ) # plots the results
plot(table(whaledata$Lockons.Hudson))
plot(table(whaledata$Lockons.Casey))


whaledata_new <- whaledata[,c(1,2,3,5,6,7)]

require(psych)
describe(whaledata_new)
describe.by(whaledata_new,whaledata$Daytime.Hudson)

#######creating dummy variables for daytime 
head(whaledata)

whaledata$Daytime.Hudson=as.factor(relevel(whaledata$Daytime.Hudson,ref="0"))
whaledata$Daytime.Hudson=as.factor(relevel(whaledata$Daytime.Hudson,ref="0"))
whaledata <- fastDummies::dummy_cols(whaledata, select_columns = "Daytime.Hudson")
whaledata <- fastDummies::dummy_cols(whaledata, select_columns = "Daytime.Casey")
colnames(whaledata)

###removing one dummy column
whaledata$Daytime.Hudson_0 <- NULL
whaledata$Daytime.Casey_0 <- NULL
whaledata$Daytime.Casey <- NULL
whaledata$Daytime.Hudson <- NULL


Y1 <- cbind(whaledata$Lockons.Hudson)
X1 <- cbind(whaledata$Period.Hudson,whaledata$Bouts.Hudson,whaledata$Daytime.Hudson)


Y2 <- cbind(whaledata$Lockons.Casey)
X2 <- cbind(whaledata$Period.Casey,whaledata$Bouts.Casey,whaledata$Daytime.Casey)

hudson_model <- glm(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson, family = 'poisson',data=whaledata)

summary(hudson_model)

anova(hudson_model)


casey_model <- glm(Lockons.Casey ~ Period.Casey+Bouts.Casey+Daytime.Casey, family = 'poisson', data=whaledata)

summary(casey_model)

anova(casey_model)


with(poisson, cbind(res.deviance = deviance, whaledata = whaledata.residual,
                    p = pchisq(deviance, whaledata.residual, lower.tail=FALSE)))

with(poisson2, cbind(res.deviance = deviance, whaledata = whaledata.residual,
                     p = pchisq(deviance, whaledata.residual, lower.tail=FALSE)))

# Test for overdispersion (dispersion and alpha parameters) from AER package

dispersiontest(hudson_model, trafo=1)

dispersiontest(casey_model, trafo=1)



require(foreign)
hud_binomial <- glm.nb(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson,data = whaledata)

summary(hud_binomial)
anova(hud_binomial)
par(mfrow=c(2,2))
plot(hud_binomial)
par(mfrow=c(1,1))

with(hud_binomial,cbind(res.deviance=deviance, whaledata=whaledata.residual,
                     p=pchisq(deviance, whaledata.residual, lower.tail=FALSE)))
anova(hud_binomial)
plot(hud_binomial)


casey_binomial <- glm.nb(Lockons.Casey ~ Period.Casey+Bouts.Casey+Daytime.Casey,  data=whaledata)

summary(casey_binomial)
anova(casey_binomial)
par(mfrow=c(2,2))
plot(casey_binomial)
par(mfrow=c(1,1))

with(casey_binomial,cbind(res.deviance=deviance, whaledata=whaledata.residual,
                    p=pchisq(deviance, whaledata.residual, lower.tail=FALSE)))
plot(casey_binomial)




ziP_hudson = zeroinfl(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson,data = whaledata, dist = "poisson")
summary(ziP_hudson)


hurdleP_hudson =hurdle(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson,data = whaledata, dist = "poisson")
summary(hurdleP_hudson)


################################################################################
##second question

leukemia <- read.csv('leukemia.csv')
str(leukemia)
leukemia$sex <- as.factor(leukemia$sex)
dim(leukemia)
View(leukemia)
require(fastDummies)
leukemia <- fastDummies::dummy_cols(leukemia, select_columns = "sex")
leukemia$sex <- as.factor(leukemia$sex)
leukemia$sex <- NULL
leukemia$sex_0 <- NULL
colnames(leukemia)

require(ggplot2)
leukemia$status <- as.factor(leukemia$status)
leukemia$Rx <- as.factor(leukemia$Rx)

colnames(leukemia)
str(leukemia)
describe.by(leukemia,leukemia$Rx)
plot(density(leukemia$survival.times))
plot(density(leukemia$logWBC))
library(magrittr)
summary(leukemia)

plot_density(leukemia$logWBC)

leukemia %>% ggplot(aes(x = Rx, y = survival.times, fill = Rx))+
  geom_boxplot()+facet_wrap(~status, scales = "free")

leukemia %>% ggplot(aes(x = Rx, y = survival.times, fill = Rx))+
  geom_boxplot()+facet_wrap(~sex, scales = "free")

leukemia %>% ggplot(aes(x = Rx, y = logWBC, fill = Rx))+
  geom_boxplot()+facet_wrap(~status, scales = "free")

leukemia %>% ggplot(aes(x = Rx, y = logWBC, fill = Rx))+
  geom_boxplot()+facet_wrap(~sex, scales = "free")

leukemia %>% 
  ggplot(aes(x = logWBC, y =..density..))+
  geom_density(show.legend = F, aes(fill = Rx))+labs(title = "Density Plot of each Rx")

leukemia %>% 
  ggplot(aes(x = logWBC, fill = Rx)) +
  geom_histogram(col = "black",binwidth = 1) +
  labs(title = "Count of patients for each Rx")


##############################################
colnames(leukemia)
attach(leukemia)
# Define variables 
time <- survival.times
event <- status
X <- cbind(logWBC,sex_1)
group <- Rx
event <- as.numeric(event)

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability",col = "blue")

kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability",col = "blue")

# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~X), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

###3 classes
x <- leukemia$logWBC
logWBC <- ifelse((x>=0) & (x<=2.30),"low",
                 ifelse((x>=2.31) & (x<3),"medium",
                        ifelse((x>=3),"high","na")))

leukemia$logWBC <- logWBC
g <- leukemia$logWBC

# Kaplan-Meier non-parametric analysis by  Logwbc group
kmsurvival1 <- survfit(Surv(time, event) ~ g)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)
plot(coxph)

# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS
exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)


##Missing data question

class_data <- read.csv('class_data.csv')
class_datab <- class_data
dim(class_data)

str(class_data)
View(class_data)

require(mice)
require(nnet)
require(caret)
require(DataExplorer)
require(scales)
require(data.table)
require(ggplot2)

summary(class_data)
plot_str(class_data)  #we have all variable int tye
plot_missing(class_data) # 5 variables have Missing values
plot_histogram(class_data)  #to plot histogram of continuous variables
plot_density(class_data)    #to plot density plot of variable
create_report(class_data) #summary of data analysis

class_data_clean <- na.omit(class_data)
sapply(class_data, function(x) sum(is.na(x)))

require(fastDummies)
class_data <- fastDummies::dummy_cols(class_data, select_columns = "Sex")
class_data$Sex <- NULL
class_data$Sex_0 <- NULL


m3 <- lm(Weight ~ Age + Height + Sex_1,data = class_data)
summary(m3)


require(VIM)
aggr_plot <- aggr(class_datab, col=c('yellow','green'), numbers=TRUE, sortVars=TRUE, labels=names(class_datab), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(class_datab[c(1,2)])

class_data.mis <- class_datab[,-c(3)]

imputed_Data <- mice(class_data.mis, m=5, maxit = 20, method = 'pmm', seed = 500)


summary(imputed_Data)
imputed_Data$imp$Age
imputed_Data$imp$Weight

completeData <- complete(imputed_Data,4)
completeData$Sex <- class_datab$Sex
completeData <- fastDummies::dummy_cols(completeData, select_columns = "Sex")
completeData$Sex <- NULL
completeData$Sex_0 <- NULL


m1 <- lm(Weight ~ Age + Height + Sex_1,data = completeData)
summary(m1)

#build predictive model
modelFit1 <- with(imputed_Data,lm(Weight ~ Age + Height))
summary(pool(modelFit1))
summary(modelFit1)


###comparing
densityplot(imputed_Data)
xyplot(imputed_Data,Weight ~ Age + Height,pch=18,cex=1)



