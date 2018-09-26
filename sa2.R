library(readxl)

library(MASS)
install.packages('plyr')
install.packages('arules')
install.packages('readr')
install.packages('readxl')
install.packages('arulesViz')
install.packages('xlsx')
install.packages('reshape2')


df1<- read_excel("Dataset.xls")

df2<- df1

##sample

set.seed(312)

data=df1[sample(nrow(df1), 1000),]

write.csv(data,"sampleset.csv")

head(data)

class(data)

colnames(data)

data$`AREA(Agrl)`

str(data)

sapply(data, function(x) sum(is.na(x)))



####cleaning data

require(dplyr)

#missing values

sapply(data, function(x) sum(is.na(x)))

df <- data %>%mutate(INCOME = ifelse(is.na(INCOME),0, INCOME))

#changed name of two columns AREA(Agrl), Gender(F:Female)

names(df)[names(df) == "AREA(Agrl)"] <- "Area"

names(df)[names(df) == "Gender(F:Female)"] <- "Gender"



df <- df %>% mutate(Area = ifelse(is.na(Area),0, Area))

sapply(df, function(x) sum(is.na(x)))

head(df)



str(df)

##changing datatypes

df$Gender <- as.factor(df$Gender)
df$AGE <- as.numeric(df$AGE)
df$CASTE <- as.factor(df$CASTE)
df$RELIGN <- as.factor(df$RELIGN)
df$MTONGUE <- as.factor(df$MTONGUE)
df$OCCU <- as.factor(df$OCCU)

###VISUALIZING

library(ggplot2)

g <- ggplot(df, aes(Gender,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(CASTE,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(RELIGN,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(MTONGUE,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +  coord_flip() 

g <- ggplot(df, aes(OCCU,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 





qplot(WRITE, data=df, geom="density", fill=Gender, alpha=I(.5)) ##normal

qplot(READ, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew

qplot(MATH, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew for female

qplot(TOTAL, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew

qplot(TOTAL, data=df, geom="density", alpha=I(.5))  ##left skew



###boxplot

boxplot(TOTAL~Gender,data=df)

df$TOTAL[df$TOTAL< 23] <- 23

#df$TOTAL[df$TOTAL> 90] <- 90

boxplot(INCOME~Gender,data=df)

boxplot(Area~Gender,data=df)

boxplot(AGE~Gender,data=df)



###outlier treatment

df$AGE[df$AGE >= 50]  <- NA

df <- df %>%na.omit()

quantile(df$INCOME,0.95)
df$INCOME[df$INCOME > 12000] <- 12000
boxplot(INCOME~Gender,data=df)
###

write.csv(df,'sampleset.csv')

#####correlation matrix

M<-df[,8:13]

M$AGE<- df$AGE



c <- cor(M)

head(round(c,2))

corrplot::corrplot(c,method = "color")

library(car)







##creating dummy variables

install.packages("fastDummies")

require(fastDummies)

df <- fastDummies::dummy_cols(df, select_columns = "Gender")

df <- fastDummies::dummy_cols(df, select_columns = "OCCU")

df <- fastDummies::dummy_cols(df, select_columns = "RELIGN")

df <- fastDummies::dummy_cols(df, select_columns = "CASTE")

df <- fastDummies::dummy_cols(df, select_columns = "MTONGUE")

###

write.csv(df,'sampleset.csv')

ncol(df)

colnames(df)

##total_Columns



ncol(df)



colnames(df)

###modelling

vif()

df$INCOME <- (log(df$INCOME + 1))

df <- df[-c(946, 572, 409,87), ]

df$log_total <- log(df$TOTAL+1) 



###

require(leaps)

reg <- regsubsets(TOTAL~AGE+INCOME+Gender_F+Gender_T+OCCU_C+OCCU_A+OCCU_H+OCCU_B+OCCU_U+RELIGN_H+RELIGN_C+RELIGN_M + CASTE_SC +CASTE_OT+CASTE_ST+MTONGUE_T+MTONGUE_K+MTONGUE_U+MTONGUE_D,data = df, nbest=10)

summary(reg)

m2 <- lm(TOTAL ~ AGE+ INCOME+Gender_F + CASTE_ST+ OCCU_B,data = df)

summary(m2)



###backward using regsubsets

reg2 = regsubsets(TOTAL~AGE+INCOME+Gender_F+Gender_T+OCCU_C+OCCU_A+OCCU_H+OCCU_B+OCCU_U+RELIGN_H+RELIGN_C+RELIGN_M + CASTE_SC +CASTE_OT+CASTE_ST+MTONGUE_T+MTONGUE_K+MTONGUE_U+MTONGUE_D,data = df,nbest=10,method = "backward")

summary(reg2)

plot(reg2,scale="r2")





m3 <- lm(TOTAL ~ AGE+ INCOME+Gender_F + CASTE_SC+ OCCU_H + OCCU_B + CASTE_OT + MTONGUE_T + MTONGUE_U ,data = df)

summary(m3)





####randomly modelling



m3 <- lm(TOTAL ~ AGE:Gender_F:CASTE_SC + CASTE_SC  + MTONGUE_K:OCCU_H +RELIGN_H:OCCU_B:AGE+MTONGUE_U+ CASTE_OT+ Gender_F+ CASTE_OT:Gender_F+ OCCU_U:MTONGUE_U  + OCCU_C:MTONGUE_U:CASTE_SC  ,data = df)

summary(m3)

plot(m3)


# Other useful functions 

coefficients(m3) # model coefficients

confint(m3, level=0.95) # CIs for model parameters 

fitted(m3) # predicted values

residuals(m3) # residuals

anova(m3) # anova table 

vcov(m3) # covariance matrix for model parameters 

influence(m3) # regression diagnostics





df <- df[-c(748,909,271),]

df2<- df

df <- df[-c(704,101,474),]

df<- df2



#### standardize

library(lm.beta)

lm.std.beta <- lm.beta(m3)

summary(lm.std.beta)

print(lm.std.beta,standardized=T)



#95% confidence interval for Beta coefficients

options(scipen = 999)

confint(m3,level=0.95)



head(data2)





#QQ-plot of the residuals

qqnorm(rstandard(m3))

qqline(rstandard(m3))



#residualPlot(m3)



##vif test

vif(m3)

car::vif(m3)



best.subset <- regsubsets(TOTAL ~ AGE:Gender_F:CASTE_SC + CASTE_SC  + MTONGUE_K:OCCU_H +RELIGN_H:OCCU_B:AGE+MTONGUE_U+ CASTE_OT+ Gender_F+ CASTE_OT:Gender_F+ OCCU_U:MTONGUE_U  + OCCU_C:MTONGUE_U:CASTE_SC  ,data = df,nvmax=6)

best.subset.summary <- summary(best.subset)

best.subset.summary$outmat

best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)

best.subset.by.adjr2

best.subset.by.cp <- which.min(best.subset.summary$cp)

best.subset.by.cp

best.subset.by.bic <- which.min(best.subset.summary$bic)

best.subset.by.bic







####





par(mfrow=c(2,2))

plot(best.subset$rss, xlab="Number of Variables", ylab="RSS", type="l")

plot(best.subset.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")

points(best.subset.by.adjr2, best.subset.summary$adjr2[best.subset.by.adjr2], col="red", cex =2, pch =20)

plot(best.subset.summary$cp, xlab="Number of Variables", ylab="CP", type="l")

points(best.subset.by.cp, best.subset.summary$cp[best.subset.by.cp], col="red", cex =2, pch =20)

plot(best.subset.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")

points(best.subset.by.bic, best.subset.summary$bic[best.subset.by.bic], col="red", cex =2, pch =20)





install.packages(c("olsrr", "v0.5.1"))

library(olsrr,v0.5.1)

k=ols_step_best_subset(m3,method="AIC")

plot(k)



##forward selection method

i <- ols_step_forward_p(m3)

plot(i)



##backward selection method

j<-ols_step_backward_p(m3)





#Cook's Distance:

cooksd <- cooks.distance(m3)

cooksd

# plot cook's distance

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 

#h signifies the average hat value (k+1)/n

abline(h = 4*mean(cooksd, na.rm=T), col="red")

# add labels

text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

# influential row numbers

influential <- names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]

influential



#DFBETAs

dfbetas=dfbetas(m3)

dfbetas

ols_plot_dfbetas(m3)

#DFFITs

ols_plot_dffits(m3)

dffits=dffits(m3)

dffits



#Studentized residual Plot

ols_plot_resid_stud(m3)



#Standarized residual Plot

ols_plot_resid_stand(m3)



#Leverage

leverage=ols_leverage(model = m3)



#Outlier & Leverage

ols_plot_resid_lev(m3)



##DW test

car::durbinWatsonTest(m3)

###backward elimination using stepaic

Model1=lm(TOTAL~AGE+INCOME+Gender_F+Gender_T+OCCU_C+OCCU_A+OCCU_H+OCCU_B+OCCU_U+RELIGN_H+RELIGN_C+RELIGN_M + CASTE_SC +CASTE_OT+CASTE_ST+MTONGUE_T+MTONGUE_K+MTONGUE_U+MTONGUE_D,data=df)

step <- stepAIC(Model1, direction="back")



m <- lm(TOTAL~AGE+ CASTE_SC + CASTE_OT+ OCCU_B,data = df)

summary(m)
summary(Model1)
plot(Model1)

###stepwise regression
fit_Step <- lm(TOTAL~AGE+INCOME+Gender_F+Gender_T+OCCU_C+OCCU_A+OCCU_H+OCCU_B+OCCU_U+RELIGN_H+RELIGN_C+RELIGN_M + CASTE_SC +CASTE_OT+CASTE_ST+MTONGUE_T+MTONGUE_K+MTONGUE_U+MTONGUE_D,data=df)

step <- stepAIC(fit_Step, direction="both")
step$anova # display results

#####LOGISTIC REGRESSION

head(df1)
dim(df1)
View(df)
####
df$acheivers[df$TOTAL > 70] <- 1
df$acheivers[df$TOTAL <= 70] <- 0

####sampling
train <- df[1:700,]
test <- df[701:992,]

###
table(train$acheivers)
table(test$acheivers)

## Model
train <- as.data.frame(train)
test <- as.data.frame(test)

MODEL1 <- glm(acheivers ~ AGE:Gender_F:CASTE_SC + CASTE_SC  + MTONGUE_K:OCCU_H +RELIGN_H:OCCU_B:AGE+MTONGUE_U+ CASTE_OT+ Gender_F+ CASTE_OT:Gender_F+ OCCU_U:MTONGUE_U  + OCCU_C:MTONGUE_U:CASTE_SC  ,data=train, family = binomial)
summary(MODEL1)

###

MODEL2 <- glm(acheivers ~ CASTE_SC + OCCU_C + MTONGUE_U + OCCU_H:MTONGUE_K + OCCU_U:MTONGUE_U  + OCCU_C:MTONGUE_U:CASTE_SC  ,data=train, family = binomial)
summary(MODEL2)

###

MODEL1 <- glm(acheivers ~  CASTE_OT:Gender_T  + Gender_T:MTONGUE_T +  OCCU_A:CASTE_SC,data=train, family = binomial)
summary(MODEL1)



anova(MODEL1, test="Chisq")
anova(MODEL2, test="Chisq")

df$acheivers
g <- ggplot(df, aes(acheivers,INCOME))
g + geom_bar(stat = "identity",aes(fill = Gender)) +coord_flip() 

g <- ggplot(df, aes(acheivers,INCOME))
g + geom_bar(stat = "identity",aes(fill = MTONGUE)) +coord_flip() 

g <- ggplot(df, aes(acheivers,INCOME))
g + geom_bar(stat = "identity",aes(fill = CASTE)) +coord_flip() 

g <- ggplot(df, aes(acheivers,INCOME))
g + geom_bar(stat = "identity",aes(fill = OCCU)) +coord_flip() 

###fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')

predicted <- predict(MODEL1, newdata = test, type = "response")
fitted.results <- ifelse(predicted > 0.18,1,0)
table(fitted.results)
x <- fitted.results != test$acheivers

length(which(x == TRUE))
###30% accuracy

## ROC curve
#install.packages('pROC')
library(pROC)
predicted <- predict(Donner.Model, newdata = donner, type = "response")
roccurve <- roc(test$acheivers ~ predicted)
plot(roccurve)

#####
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)  

tidy(MODEL2)
exp(coef(MODEL1))
confint(MODEL1)
caret::varImp(model3)

