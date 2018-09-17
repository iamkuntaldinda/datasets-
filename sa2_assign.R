library(readxl)
library(MASS)

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

df <- data %>%
  mutate(INCOME = ifelse(is.na(INCOME),0, INCOME))

#changed name of two columns AREA(Agrl), Gender(F:Female)
names(df)[names(df) == "AREA(Agrl)"] <- "Area"
names(df)[names(df) == "Gender(F:Female)"] <- "Gender"

df <- df %>%
  mutate(Area = ifelse(is.na(Area),0, Area))

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
g + geom_bar(stat = "identity",fill = "blue") +
  coord_flip() 
g <- ggplot(df, aes(CASTE,TOTAL))
g + geom_bar(stat = "identity",fill = "blue") +
  coord_flip() 
g <- ggplot(df, aes(RELIGN,TOTAL))
g + geom_bar(stat = "identity",fill = "blue") +
  coord_flip() 
g <- ggplot(df, aes(MTONGUE,TOTAL))
g + geom_bar(stat = "identity",fill = "blue") +
  coord_flip() 
g <- ggplot(df, aes(OCCU,TOTAL))
g + geom_bar(stat = "identity",fill = "blue") +
  coord_flip() 


qplot(WRITE, data=df, geom="density", fill=Gender, alpha=I(.5)) ##normal
qplot(READ, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew
qplot(MATH, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew for female
qplot(TOTAL, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew
qplot(TOTAL, data=df, geom="density", alpha=I(.5))  ##left skew

###boxplot
boxplot(TOTAL~Gender,data=df)
df$TOTAL[df$TOTAL< 20] <- 20
boxplot(~Gender,data=df)
boxplot(INCOME~Gender,data=df)
boxplot(Area~Gender,data=df)
boxplot(AGE~Gender,data=df)

###outlier treatment
 df$AGE[df$AGE > 50]  <- NA
df <- df %>%na.omit()
quantile(df$INCOME,0.95)

df$INCOME[df$INCOME > 11000] <- 11000
boxplot(INCOME~Gender,data=df)

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


####randomly

m3 <- lm(TOTAL ~ AGE:Gender_F:CASTE_SC + CASTE_SC  +RELIGN_H:OCCU_B:AGE+MTONGUE_U+ CASTE_OT+ Gender_F+ CASTE_OT:Gender_F+ OCCU_U:MTONGUE_U  + OCCU_C:MTONGUE_U:CASTE_SC ,data = df)
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
###CREATING new variables


###backward elimination using stepaic
Model1=lm(TOTAL~AGE+INCOME+Gender_F+Gender_T+OCCU_C+OCCU_A+OCCU_H+OCCU_B+OCCU_U+RELIGN_H+RELIGN_C+RELIGN_M + CASTE_SC +CASTE_OT+CASTE_ST+MTONGUE_T+MTONGUE_K+MTONGUE_U+MTONGUE_D,data=df)
step <- stepAIC(Model1, direction="back")

m <- lm(TOTAL~AGE+ CASTE_SC + CASTE_OT+ OCCU_B,data = df)
summary(m)
summary(Model1)

plot(Model1)





###forward elimination





###stepwise regression

fit_Step <- lm(TOTAL~AGE+INCOME+Gender_F+Gender_T+OCCU_C+OCCU_A+OCCU_H+OCCU_B+OCCU_U+RELIGN_H+RELIGN_C+RELIGN_M + CASTE_SC +CASTE_OT+CASTE_ST+MTONGUE_T+MTONGUE_K+MTONGUE_U+MTONGUE_D,data=df)
step <- stepAIC(fit_Step, direction="both")

step$anova # display results
