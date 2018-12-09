
# install.packages("MASS")
library(MASS)
#install.packages("pscl")
library(pscl)
#install.packages("AER")
library(AER)

df <- read.csv('whale.csv')
dim(df)
str(df)
df2 <- df
#View(df)

###summary statistics
df <- na.omit(df)
as.factor(df$Daytime.Hudson) -> df$Daytime.Hudson
as.factor(df$Daytime.Casey) -> df$Daytime.Casey

sapply(df, mean, na.rm=TRUE)
summary(df)
library(Hmisc)
ggplot(df, aes(df$Bouts.Hudson, fill = Daytime.Hudson)) +
  geom_histogram(binwidth=2, position="dodge")+ ggtitle("Count of Bouts")
ggplot(df, aes(df$Lockons.Hudson, fill = Daytime.Hudson)) +
  geom_histogram(binwidth=2, position="dodge") + ggtitle("Count of Lockons")
ggplot(df, aes(df$Lockons.Casey, fill = Daytime.Casey)) +
  geom_histogram(binwidth=2, position="dodge") + ggtitle("Count of Lockons")


plot(table(df$Lockons.Hudson))
plot(table(df$Lockons.Casey))


df_new <- df[,c(1,2,3,5,6,7)]

library(psych)
describe.by(df_new,df$Daytime.Hudson)

#######creating dummy variables for daytime 
head(df)

df$Daytime.Hudson=as.factor(relevel(df$Daytime.Hudson,ref="0"))
df$Daytime.Hudson=as.factor(relevel(df$Daytime.Hudson,ref="0"))
df <- fastDummies::dummy_cols(df, select_columns = "Daytime.Hudson")
df <- fastDummies::dummy_cols(df, select_columns = "Daytime.Casey")
colnames(df)
###removing one dummy column
df$Daytime.Hudson_0 <- NULL
df$Daytime.Casey_0 <- NULL
df$Daytime.Casey <- NULL
df$Daytime.Hudson <- NULL


# Define variables
Y1 <- cbind(df$Lockons.Hudson)
X1 <- cbind(df$Period.Hudson,df$Bouts.Hudson,df$Daytime.Hudson)
Y2 <- cbind(df$Lockons.Casey)
X2 <- cbind(df$Period.Casey,df$Bouts.Casey,df$Daytime.Casey)

# Poisson model coefficients
poisson <- glm(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson, family = 'poisson',data=df)
summary(poisson)
anova(poisson)
plot(poisson)

poisson2 <- glm(Lockons.Casey ~ Period.Casey+Bouts.Casey+Daytime.Casey, family = 'poisson', data=df)
summary(poisson2)
plot(poisson2)
anova(poisson2)


with(poisson, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))

with(poisson2, cbind(res.deviance = deviance, df = df.residual,
                     p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# Test for overdispersion (dispersion and alpha parameters) from AER package
dispersiontest(poisson)
dispersiontest(poisson, trafo=1)

dispersiontest(poisson2)
dispersiontest(poisson2, trafo=1)


###negative binomial test

library(foreign)
hudson_NB <- glm.nb(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson,data = df)

summary(hudson_NB)
anova(hudson_NB)
par(mfrow=c(2,2))
plot(hudson_NB)
par(mfrow=c(1,1))

with(hudson_NB,cbind(res.deviance=deviance, df=df.residual,
                     p=pchisq(deviance, df.residual, lower.tail=FALSE)))
anova(hudson_NB)
plot(hudson_NB)


###Casey

casey_NB <- glm.nb(Lockons.Casey ~ Period.Casey+Bouts.Casey+Daytime.Casey,  data=df)

summary(casey_NB)
anova(casey_NB)
par(mfrow=c(2,2))
plot(casey_NB)
par(mfrow=c(1,1))

with(casey_NB,cbind(res.deviance=deviance, df=df.residual,
                    p=pchisq(deviance, df.residual, lower.tail=FALSE)))
plot(casey_NB)


####Zero inflated and hurdle models

hurdleP_hudson =hurdle(Lockons.Hudson ~ Period.Hudson+Daytime.Hudson + Bouts.Hudson,data = df, dist = "poisson")
summary(hurdleP_hudson)


################################################################################

###second question

# install.packages("survival")
library(survival)

df <- read.csv('leukemia.csv')
str(df)
df$sex <- as.factor(df$sex)
dim(df)
View(df)
require(fastDummies)
df <- fastDummies::dummy_cols(df, select_columns = "sex")
df$sex <- as.factor(df$sex)
df$sex <- NULL
df$sex_0 <- NULL
colnames(df)

require(ggplot2)
df$status <- as.factor(df$status)
df$Rx <- as.factor(df$Rx)

library(magrittr)
summary(df)

df %>% ggplot(aes(x = Rx, y = survival.times, fill = Rx))+
  geom_boxplot()+facet_wrap(~status, scales = "free")

df %>% ggplot(aes(x = Rx, y = survival.times, fill = Rx))+
  geom_boxplot()+facet_wrap(~sex, scales = "free")

df %>% ggplot(aes(x = Rx, y = logWBC, fill = Rx))+
  geom_boxplot()+facet_wrap(~status, scales = "free")

df %>% ggplot(aes(x = Rx, y = logWBC, fill = Rx))+
  geom_boxplot()+facet_wrap(~sex, scales = "free")

df %>% 
  ggplot(aes(x = logWBC, y =..density..))+
  geom_density(show.legend = F, aes(fill = Rx))+labs(title = "Density Plot of each Rx")

df %>% 
  ggplot(aes(x = logWBC, fill = Rx)) +
  geom_histogram(col = "black",binwidth = 1) +
  labs(title = "Count of patients for each Rx")


##############################################
colnames(df)
attach(df)
# Define variables 
time <- survival.times
event <- status
X <- cbind(logWBC,sex_1)
group <- Rx
event <- as.numeric(event)


# Kaplan-Meier non-parametric analysis by group
km_model <- survfit(Surv(time, event) ~ group)
summary(km_model)
#install.packages('survMisc')
#install.packages('survminer')
require(survminer)
require(survMisc)
ggsurvplot(km_model, data = leukemia, risk.table = TRUE)

# Nelson-Aalen Cumulative Hazard analysis

nelson_aalen_fit <- basehaz(coxph(Surv(time,event)~group,data=leukemia))
plot(nelson_aalen_fit)
nelson_aalen_fit



# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~X), type="aalen")
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
as.factor(leukemia$logWBC) -> leukemia$logWBC
attach(df)
group <- logWBC


# Kaplan-Meier non-parametric analysis by  Logwbc group
km_model <- survfit(Surv(time, event) ~ group)
summary(km_model)

ggsurvplot(km_model, data = leukemia, risk.table = TRUE)



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


#####

df <- read.csv('class_data.csv')
dfb <- df
dim(df)

str(df)
View(df)
library(mice)
library(nnet)
library(caret)
library(DataExplorer)
#install.packages('scales')
library(scales)
#install.packages('data.table')
library(data.table)
require(ggplot2)

summary(df)
plot_str(df)  #we have all variable int tye
plot_missing(df) # 5 variables have Missing values
plot_histogram(df)  #to plot histogram of continuous variables
plot_density(df)    #to plot density plot of variable
create_report(df) #summary of data analysis

df_clean <- na.omit(df)
sapply(df, function(x) sum(is.na(x)))

require(fastDummies)
df <- fastDummies::dummy_cols(df, select_columns = "Sex")
df$Sex <- NULL
df$Sex_0 <- NULL


m3 <- lm(Weight ~ Age + Height + Sex_1,data = df)
summary(m3)

#install.packages("VIM")

library(VIM)
aggr_plot <- aggr(dfb, col=c('yellow','green'), numbers=TRUE, sortVars=TRUE, labels=names(dfb), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(dfb[c(1,2)])

df.mis <- dfb[,-c(3)]

imputed_Data <- mice(df.mis, m=5, maxit = 20, method = 'pmm', seed = 500)


summary(imputed_Data)
imputed_Data$imp$Age
imputed_Data$imp$Weight

completeData <- complete(imputed_Data,4)
completeData$Sex <- dfb$Sex
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



