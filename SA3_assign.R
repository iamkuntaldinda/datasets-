df <- read.csv('class_data.csv')
dfb <- df
dim(df)

str(df)
View(df)
library(mice)
library(nnet)
library(caret)
library(DataExplorer)
install.packages('scales')
library(scales)
install.packages('data.table')
library(data.table)
require(ggplot2)

g <- ggplot(train_df, aes(user_group_id,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(campaign_id,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(gender,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

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

###comparing
densityplot(imputed_Data)
xyplot(imputed_Data,Weight ~ Age + Height,pch=18,cex=1)





