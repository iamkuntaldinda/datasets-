

train_dfb -> train_df
str(train_df)

train_df$webpage_id_51181 <- NULL
train_df$product <- NULL
train_df$product_category_1 <- NULL
train_df$gender <- NULL
train_df$var_1 <- NULL


train_df$age_level <- NULL
train_df$campaign_id <- NULL
train_df$webpage_id <- NULL
train_df$user_group_id <- NULL
train_df$user_depth <- NULL
train_df$gender_Female <- NULL
train_df$var_1_1 <- NULL
train_df$campaign_id_396664 <- NULL
train_df$product_F <- NULL
train_df$age_level_0 <- NULL
train_df$user_group_id_0 <- NULL
train_df$product_category_1_2 <- NULL
train_df$user_depth_1 <- NULL
train_df$age_level_NA <- NULL
train_df$user_group_id_NA <- NULL
train_df$user_depth_NA <- NULL


library(caTools)
set.seed(88)
split <- sample.split(df$is_click, SplitRatio = 0.75)
#get training and test data
dresstrain <- subset(df, split == TRUE)
dresstest <- subset(df, split == FALSE)

prop.table(table(dresstrain$is_click))

#logistic regression model
model <- glm (is_click ~ ., data = dresstrain, family = binomial)
summary(model)
predict <- predict(model, dresstest,type = 'response')

install.packages('DMwR')
library(DMwR)
dresstrain$is_click <- as.factor(dresstrain$is_click)
trainSplit <- SMOTE(is_click ~ ., dresstrain, perc.over = 100, perc.under=200)
trainSplit$is_click <- as.numeric(trainSplit$is_click)

prop.table(table(trainSplit$is_click))
dim(trainSplit)
dresstrain <- trainSplit
dresstrain$is_click <- as.factor(dresstrain$is_click)

# evaluate the SMOTE performance
# model using treebag
ctrl <- trainControl(method = "cv", number = 5)

tbmodel <- train(is_click ~ ., data = trainSplit, method = "treebag",
                 trControl = ctrl)


tbmodel <- train(is_click ~ ., data = dresstrain, method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

auc <- roc(testSplit$target, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

















































###naive bayes algo
train_df -> train_dfb
str(train_df)

df <- train_df[,-c(1,3)]
colnames(df)
df$is_click <- as.factor(df$is_click)
df$product_category_2 <- NULL
df$DateTime <- NULL
prop.table(table(df$is_click))

str(df)


df$age_level <- as.character(df$age_level)
df$user_depth <- as.character(df$user_depth)
df$city_development_index <- as.character(df$city_development_index)
df$age_level <- as.character(df$user_group_id)

df$age_level[is.na(df$age_level)] <- "3" 
df$user_depth[is.na(df$user_depth)] <- "3" 
df$city_development_index[is.na(df$city_development_index)] <- "3" 
df$user_group_id[is.na(df$user_group_id)] <- "3" 

df <- fastDummies::dummy_cols(df, select_columns = "gender")

df <- fastDummies::dummy_cols(df, select_columns = "var_1")

df <- fastDummies::dummy_cols(df, select_columns = "webpage_id")

df <- fastDummies::dummy_cols(df, select_columns = "campaign_id")

df <- fastDummies::dummy_cols(df, select_columns = "product")

df <- fastDummies::dummy_cols(df, select_columns = "age_level")

df <- fastDummies::dummy_cols(df, select_columns = "user_group_id")

df <- fastDummies::dummy_cols(df, select_columns = "user_depth")

df <- fastDummies::dummy_cols(df, select_columns = "product_category_1")

df <- fastDummies::dummy_cols(df, select_columns = "hour")

df <- fastDummies::dummy_cols(df, select_columns = "city_development_index")

colnames(df)
table(df$hour)

df$city_development_index_1 <- NULL
df$city_development_index <- NULL

df$hour <- NULL
df$hour_night <- NULL
df$webpage_id_51181 <- NULL
df$product <- NULL
df$product_category_1 <- NULL
df$gender <- NULL
df$var_1 <- NULL


df$age_level <- NULL
df$campaign_id <- NULL
df$webpage_id <- NULL
df$user_group_id <- NULL
df$user_depth <- NULL
df$gender_Female <- NULL
df$var_1_1 <- NULL
df$campaign_id_396664 <- NULL
df$product_F <- NULL
df$age_level_0 <- NULL
df$user_group_id_0 <- NULL
df$product_category_1_2 <- NULL
df$user_depth_1 <- NULL

str(df)
require(e1071)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(is_click ~., data=dresstrain)
#What does the model say? Print the model summary
Naive_Bayes_Model
  

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,dresstest)
#Confusion matrix to check accuracy

table(NB_Predictions,dresstest$is_click)


tp <- 1801
tn <- 147099
fp <- 10222
fn <- 18897

pre <- tp/(tp+fp)
rec <- tp/(tp+fn)
total <- tp+ fp+ fn+ tn
prevalance <- (tp+fn)/total
pre
rec
total
prevalance
library(pROC)

ruc_obj <- roc(dresstest$is_click,as.numeric(NB_Predictions))

str(df)
dim(df)
split <- sample.split(df$is_click, SplitRatio = 0.70)
#get training and test data
dresstrain <- subset(df, split == TRUE)
dresstest <- subset(df, split == FALSE)

prop.table(table(dresstrain$is_click))

###test_Data

test_df <- read.csv('test.csv')

df1 <- test_df[,-c(1,3)]
colnames(df1)
df1$product_category_2 <- NULL
df1$DateTime <- NULL
##imputing missing values for test data

df1$age_level <- as.character(df1$age_level)
df1$user_depth <- as.character(df1$user_depth)
df1$city_development_index <- as.character(df1$city_development_index)
df1$age_level <- as.character(df1$user_group_id)

df1$age_level[is.na(df1$age_level)] <- "3" 
df1$user_depth[is.na(df1$user_depth)] <- "3" 
df1$city_development_index[is.na(df1$city_development_index)] <- "3" 
df1$user_group_id[is.na(df1$user_group_id)] <- "3" 

df1 <- fastDummies::dummy_cols(df1, select_columns = "gender")

df1 <- fastDummies::dummy_cols(df1, select_columns = "var_1")

df1 <- fastDummies::dummy_cols(df1, select_columns = "webpage_id")

df1 <- fastDummies::dummy_cols(df1, select_columns = "campaign_id")

df1 <- fastDummies::dummy_cols(df1, select_columns = "product")

df1 <- fastDummies::dummy_cols(df1, select_columns = "age_level")

df1 <- fastDummies::dummy_cols(df1, select_columns = "user_group_id")

df1 <- fastDummies::dummy_cols(df1, select_columns = "user_depth")

df1 <- fastDummies::dummy_cols(df1, select_columns = "product_category_1")

df1 <- fastDummies::dummy_cols(df1, select_columns = "hour")

df1 <- fastDummies::dummy_cols(df1, select_columns = "city_development_index")

colnames(df1)
table(df1$hour)

df1$city_development_index_1 <- NULL
df1$city_development_index <- NULL

df1$hour <- NULL
df1$hour_night <- NULL
df1$webpage_id_51181 <- NULL
df1$product <- NULL
df1$product_category_1 <- NULL
df1$gender <- NULL
df1$var_1 <- NULL


df1$age_level <- NULL
df1$campaign_id <- NULL
df1$webpage_id <- NULL
df1$user_group_id <- NULL
df1$user_depth <- NULL
df1$gender_Female <- NULL
df1$var_1_1 <- NULL
df1$campaign_id_396664 <- NULL
df1$product_F <- NULL
df1$age_level_0 <- NULL
df1$user_group_id_0 <- NULL
df1$product_category_1_2 <- NULL
df1$user_depth_1 <- NULL


NB_Predictions_test=predict(Naive_Bayes_Model,df1)
prop.table(table(NB_Predictions_test))
df1$session_id -> Session_id
df2<- cbind(Session_id,NB_Predictions_test)
table(NB_Predictions_test)
write.csv(df2,'submit1.csv')

df2 <- as.data.frame(df2)
table(df2$NB_Predictions_test)
Session_id


dfn <- test_df

dfn$session_id-> Session_id



library(DMwR)
dresstrain$is_click <- as.factor(dresstrain$is_click)
trainSplit <- SMOTE(is_click ~ ., dresstrain, perc.over = 100, perc.under=200)
trainSplit$is_click <- as.numeric(trainSplit$is_click)

prop.table(table(trainSplit$is_click))
dim(trainSplit)
dresstrain <- trainSplit
dresstrain$is_click <- as.factor(dresstrain$is_click)


############################################################################################################

train_df <- train_dfb
train_df$product_category_2 <- NULL
str(train_df)
train_df[train_df$age_level == 1] <- median(train_df$age_level) 
train_df$age_level[train_df$age_level == "NA"] <- "7"

replace(train_df$age_level, is.na(train_df$age_level), median(train_df$age_level[!is.na(train_df$age_level)]))
replace(train_df$city_development_index, is.na(train_df$city_development_index), median(train_df$city_development_index[!is.na(train_df$city_development_index)]))
replace(train_df$age_level, is.na(train_df$age_level), median(train_df$age_level[!is.na(train_df$age_level)]))

plot_missing(train_df)
age <- train_df$age_level

mean()

library(DMwR)
dresstrain$is_click <- as.factor(dresstrain$is_click)

dresstrain <- SMOTE(is_click ~ ., dresstrain, perc.over = 100, perc.under=150)
prop.table(table(dresstrain$is_click))

dresstrain <- fastDummies::dummy_cols(dresstrain, select_columns = "city_development_index")
dresstest <- fastDummies::dummy_cols(dresstest, select_columns = "city_development_index")

dresstest$city_development_index_1 <- NULL
dresstrain$city_development_index_1 <- NULL
dresstest$city_development_index <- NULL
dresstrain$city_development_index <- NULL
dresstest$gender_ <- NULL
dresstrain$gender_ <- NULL
colnames(dresstrain)
colnames(dresstest)

####date time

as.character(df$DateTime) -> df$DateTime

Hours <- format(as.POSIXct(strptime(df$DateTime,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")

Dates <- format(as.POSIXct(strptime(df$DateTime,"%Y-%m-%d %H:%M",tz="")) ,format = "%d/%d/%Y")

str(Hours)
#hours <- strsplit(Hours,":")
#str(hours)

hrs <- format(as.POSIXct(strptime(df$DateTime,"%Y-%m-%d %H:%M",tz="")) ,format = "%H")
hrs <- as.numeric(hrs)

hour <- ifelse((hrs>=0) & (hrs<=6),"night",
               ifelse((hrs>6) & (hrs<=12),"morning",
                      ifelse((hrs>12) & (hrs<=18),"afternoon",
                             ifelse((hrs>18) & (hrs<24),"evening","NA"))))


table(hour)
length(hour)
df$hour <- hour 
str(hour)
as.factor(df$hour) -> df$hour
as.factor(df$is_click) -> df$is_click

require(ggplot2)
g <- ggplot(train_df, aes(hour,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 


