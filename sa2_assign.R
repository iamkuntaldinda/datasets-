library(readxl)

df1<- read_excel("Dataset.xls")
df2<- df1
##sample
set.seed(123)
data=df1[sample(nrow(df1), 1000),]
write.csv(data,"sampleset.csv")
head(data)
class(data)
colnames(data)

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

library(ggplot2)
qplot(TOTAL, data=df, geom="bar", fill=Gender, alpha=I(.5)) ##right skewed
qplot(WRITE, data=df, geom="density", fill=Gender, alpha=I(.5)) ##normal
qplot(READ, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew
qplot(MATH, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew for female
qplot(TOTAL, data=df, geom="density", fill=Gender, alpha=I(.5))  ##left skew
qplot(TOTAL, data=df, geom="density", alpha=I(.5))  ##left skew

M<-cor(df[,8:13])
M
head(round(M,2))
corrplot::corrplot(M,method = "color")
library(car)
vif(Model1)
df$INCOME <- (log(df$INCOME + 1))
df <- df[-c(946, 572, 409,87), ]
df$log_total <- log(df$TOTAL+1) 
Model1=lm(df$TOTAL~log_Age + MATH,data=df)

summary(Model1)
plot(Model1)
df[887,]

residualPlot(Model1)
qqPlot(Model1)

df$READ[df$READ < 5] <- 5

df$MATH[df$MATH == 0] <- mean(df$MATH)
##standardizing
x <- mean(data$TOTAL) - 1.5*sd(data$TOTAL)
df$TOTAL[df$TOTAL < x] <- x
y <- mean(data$TOTAL) + 1.5*sd(data$TOTAL)
df$TOTAL[df$TOTAL > y] <- y


##creating dummy
install.packages("fastDummies")
require(fastDummies)
d <- fastDummies::dummy_cols(df, select_columns = "Gender")


qplot(TOTAL, data=df, geom="density", fill=Gender, alpha=I(.5)) ##right skewed



# Boxplot of MPG by Car Cylinders 

df$log_Age <- log(df$CASTE)

df$log_Age[df$log_Age > 4.0] <- 4.0
boxplot(TOTAL~df$INCOME,data=df)
df$TOTAL[df$TOTAL< 20] <- 20 
