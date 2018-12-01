# Count Data Models in R
# Copyright 2013 by Ani Katchova

# install.packages("MASS")
library(MASS)
# install.packages("pscl")
library(pscl)
#install.packages("AER")
library(AER)

mydata<- read.csv("count_docvisit.csv")
attach(mydata)
colnames(mydata)
dim(mydata)
# Define variables
Y <- cbind(docvis)
X <- cbind(private, medicaid, age, educyr)
X1 <- cbind(private, medicaid, age, educyr)
# Descriptive statistics
summary(Y)
summary(X)

# Poisson model coefficients
poisson <- glm(Y ~ X, family = poisson)
summary(poisson)

# Test for overdispersion (dispersion and alpha parameters) from AER package
dispersiontest(poisson)
dispersiontest(poisson, trafo=2)

View(mydata)
# Negative binomial model coefficients
negbin <- glm.nb(Y ~ X)
summary(negbin)

# Hurdle or truncated Poisson model coefficients
hpoisson <- hurdle(Y ~ X | X1, link = "logit", dist = "poisson")
summary(hpoisson)

# Hurdle or truncated negative binonomial model coefficients
hnegbin <- hurdle(Y ~ X | X1, link = "logit", dist = "negbin")
summary(hnegbin)

# Zero-inflated Poisson model coefficients
zip <- zeroinfl(Y ~ X | X1, link = "logit", dist = "poisson")
summary(zip)

# Zero-inflated negative binomial model coefficients
zinb <- zeroinfl(Y ~ X | X1, link = "logit", dist = "negbin")
summary(zinb)

#################################################################3

df <- read.csv('whale.csv')
dim(df)
str(df)
View(df)
###
as.factor(df$Daytime.Hudson) -> df$Daytime.Hudson

as.factor(df$Period.Hudson) -> df$Period.Hudson
require(ggplot2)
g <- ggplot(df, aes(df$Period.Hudson,df$Bouts.Hudson))
g + geom_bar(stat = "identity",aes(fill = Daytime.Hudson)) 


g <- ggplot(df, aes(df$Period.Casey,df$Period.Casey))
g + geom_bar(stat = "identity",aes(fill = Daytime.Hudson)) 

#######
colnames(df)
# Define variables
Y1 <- cbind(df$Bouts.Hudson)
X1 <- cbind(df$Lockons.Hudson,df$Daytime.Hudson)
Y2 <- cbind(df$Bouts.Casey)
X2 <- cbind(df$Lockons.Casey,df$Daytime.Casey)


# Descriptive statistics
summary(Y1)
summary(X1)
summary(Y2)
summary(X2)

# Poisson model coefficients
poisson <- glm(Y1 ~ X1, family = 'poisson')
summary(poisson)

poisson2 <- glm(Y2 ~ X2, family = 'poisson')
summary(poisson2)


# Test for overdispersion (dispersion and alpha parameters) from AER package
dispersiontest(poisson)
dispersiontest(poisson, trafo=2)

dispersiontest(poisson2)
dispersiontest(poisson2, trafo=2)

?dispersiontest()
