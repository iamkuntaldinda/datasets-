
library(mice)
library(nnet)
library(caret)
library(DataExplorer)


train_df <- read.csv('train.csv')
train_dfb <- train_df
hist <- read.csv('historical_user_logs.csv')
hist_b <- hist
train_df$is_click <- as.factor(train_df$is_click)

colnames(train_df)

str(train_df)

train_df$product_category_1 <- as.factor(train_df$product_category_1)
train_df$webpage_id <- as.factor(train_df$webpage_id)
train_df$user_group_id <- as.factor(train_df$user_group_id)
train_df$age_level <- as.factor(train_df$age_level)
train_df$user_depth <- as.factor(train_df$user_depth)
train_df$city_development_index <- as.factor(train_df$city_development_index)
train_df$campaign_id <- as.factor(train_df$campaign_id)
train_df$var_1 <- as.factor(train_df$var_1)


require(ggplot2)

g <- ggplot(train_df, aes(user_group_id,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(campaign_id,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(gender,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(product,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip()  

g <- ggplot(train_df, aes(product_category_1,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip()  

g <- ggplot(train_df, aes(age_level,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 


g <- ggplot(train_df, aes(webpage_id,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(user_depth,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(var_1,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

g <- ggplot(train_df, aes(city_development_index,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 


g <- ggplot(train_df, aes(city_development_index,is_click))
g + geom_bar(stat = "identity",aes(fill = is_click)) + coord_flip() 

prop.table(table(train_df$is_click))

###removing missing data
train_df$product_category_2 <- NULL
train_df$city_development_index <- NULL
train_df <- na.omit(train_df)
train_df$session_id <- NULL
train_df$user_id <- NULL
train_df$DateTime <- NULL


train_dfb -> train_df
str(train_df)

train_df <- fastDummies::dummy_cols(train_df, select_columns = "gender")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "var_1")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "webpage_id")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "campaign_id")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "product")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "age_level")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "user_group_id")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "user_depth")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "product_category_1")

colnames(train_df)

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
train_df$is_click    <- as.character(train_df$is_click)
dim(train_df)
##partition the data
library(caret)
train.rows <- createDataPartition(y=train_df$is_click, p=0.70, list=FALSE)  
library(caret)
train.data<- train_df[train.rows,] # 75% data goes in here
prop.table(table(train.data$is_click))
23524/323945
test.data<- train_df[-train.rows,] # 25% data goes in here
prop.table(table(test.data$is_click))


str(train.data)
####
install.packages('ROSE')
library(ROSE)
library(rpart)
treeimb <- rpart(is_click ~ ., train.data, method = "class")
dim(test.data)
dim(train.data)
test.data$is_click -> c
test.data$is_click <- NULL
pred.treeimb <- predict(treeimb, test.data)
accuracy.meas(test.data$is_click, pred.treeimb[,2])

####
test_df <- read.csv('test.csv')
test_dfb <- test_df

str(test_df)
str(train_df)

sapply(train_df, function(x) sum(is.na(x)))
sapply(test_df, function(x) sum(is.na(x)))


library(DataExplorer)

plot_str(train_df)  #we have all variable int tye
plot_missing(train_df) # 5 variables have Missing values
plot_histogram(train_df)  #to plot histogram of continuous variables
plot_density(train_dfb)    #to plot density plot of variable
create_report(train_df) #summary of data analysis

table(train_df$age_level)



############################################################################
#data imputation  : Missing value treatment

library(mice)

md.pattern(train_df)  #displays missing value pattern data

# NA's are available in 

train_df <- train_dfb
dim(train_df)
dim(test_df)


#na.omit(train_df, cols="user_depth")

#na.omit(train_df, cols="age_level")

#age_level - rem
#age_level - rem
#user-depth - rem
#city_dev_index - pred
#category-2 - not inc



train_df$product_category_2 <- NULL
train_df$user_group_id <- NULL
train_df$city_development_index -> city_dev

#mice.impute.polyreg(city_dev, ry, x, wy = NULL, nnet.maxit = 100)


library(dplyr) 
train_df$age_level = as.factor(train_df$age_level)
train_df$user_depth <- as.factor(train_df$user_depth)
train_df$city_development_index <- as.factor(train_df$city_development_index)

init = mice(train_df, maxit=0) 
meth = init$method
predM = init$predictorMatrix

train_df$var_1 <- as.factor(train_df$var_1)
train_df$campaign_id <- as.factor(train_df$campaign_id)
train_df$webpage_id <- as.factor(train_df$webpage_id)
train_df$product_category_1 <- as.factor(train_df$product_category_1)

colnames(train_df)
str(train_df)

####predictors not included 
predM[, c("session_id","DateTime","user_id")]=0

meth[c("age_level")]="polyreg" 
meth[c("user_depth")]="polyreg" 
meth[c("city_development_index")]="polyreg"


set.seed(124)
imputed = mice(train_df, method=meth, predictorMatrix=predM, m=5)
#####################3

##Assign other to na values in 3 var

as.character(train_df$age_level) -> train_df$age_level
train_df$age_level[train_df$age_level == "NA"] <- "7"

as.character(train_df$user_depth) -> train_df$user_depth
train_df$age_level[train_df$age_level == "NA"] <- "4"

as.character(train_df$city_development_index) -> train_df$city_development_index
train_df$age_level[train_df$age_level == "NA"] <- "5"

str(train_df)
colnames(train_df)
require(fastDummies)

train_df <- fastDummies::dummy_cols(train_df, select_columns = "gender")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "var_1")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "webpage_id")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "campaign_id")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "product")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "age_level")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "city_development_index")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "user_depth")

train_df <- fastDummies::dummy_cols(train_df, select_columns = "product_category_1")



library(caret)
train.rows <- createDataPartition(y=train_df$is_click, p=0.75, list=FALSE)  
library(caret)
train.data<- train_df[train.rows,] # 75% data goes in here
table(train.data$is_click)
23524/323945
test.data<- train_df[-train.rows,] # 25% data goes in here
table(test.data$is_click)
7807/108015

df <- train_dfb

str(df)
as.factor(df$product_category_1) ->df$product_category_1

library(ggplot2)

g <- ggplot(df, aes(gender,is_click))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(CASTE,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(RELIGN,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(MTONGUE,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +  coord_flip() 

g <- ggplot(df, aes(OCCU,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

colnames(train_df)
str(train_df)
as.factor(train.data$is_click) -> train.data$is_click
as.factor(test.data$is_click) -> test.data$is_click




model1 <- glm(is_click ~ user_depth_1 + var_1_1 + gender_Female +product_category_1_1 + city_development_index_1*campaign_id_396664 +product_C*webpage_id_1734,data=train_df, family = binomial)
summary(model1)

```
install.packages("rpart.plot")	
library(rpart)
library(rpart.plot)
 train_df$DateTime <- NULL
dt2 <- rpart(is_click~., data = train_df, method = 'class')
dt2$y
table(dt2$y)
colnames(train_df)
test_df$session_id <- NULL
train_df$user_id <- NULL
test_df$user_id <- NULL
train_df$session_id <- NULL
test_df$DateTime <- NULL
p_dt <- predict(dt2, test_df, type = 'class')
table(p_dt)





#For model 1 , 
#p-value is less than 0.05 and it is significant, so reject null hypothesis and this model is good for prediction as it has got least Deviance Residual
1-pchisq(641.40-621.34,699-689)

1-pchisq(641.40-626.51,699-693)

#Visualisation of variables with target variable
g <- ggplot(df, aes(performance,TOTAL))
g + geom_bar(stat = "identity",aes(fill = Gender)) +coord_flip() 

g <- ggplot(df, aes(performance,TOTAL))
g + geom_bar(stat = "identity",aes(fill = CASTE)) +coord_flip() 

g <- ggplot(df, aes(performance,TOTAL))
g + geom_bar(stat = "identity",aes(fill = OCCU)) +coord_flip() 


p <- predict(model1,test.data, type = "response")
fit <- ifelse(p > 0.09,1,0)
table(fit)


#Variables Importance and their estimates can be find out using this.
library(broom)
tidy(model1)

#McFladden R-square shows that the R-square values of the 3 diffrent models are vary low and model1 has 0.03 R- square
require(pscl)
list(MODEL1 = pscl::pR2(model1)["McFadden"],
     MODEL2 = pscl::pR2(model2)["McFadden"],
     MODEL3 = pscl::pR2(model3)["McFadden"])

#ROC curve for the model1 and it is the plot between True positive rate and False positive rate.AUC of this curve is near to 0.5 
library(pROC)
roccurve <- roc(test.data$is_click ~ p)
plot(roccurve)
roc_obj <- roc(test.data$is_click, p)
auc(roc_obj)




#Hosmer - lemeshow test shows that the model is logistic regression model as Null hypothesis shows that model follows Logistic regression model...
library(generalhoslem)
logitgof(test$performance, p)

#Confusion matrix shows that 15% of the observation fall in Type -1 error and 
#13 % observations are in Type -2 error. 
#only 4% values predicted are high achievers and 67% are true negative values means low achievers. 
c = table(test$performance, p > 0.20) %>% prop.table()
c


########################################################################################################
test_df$user_group_id <- NULL

test_df$product_category_2 <- NULL

train_df$age_level_NA
#as.character(test_df$age_level) -> test_df$age_level
#test_df$age_level[test_df$age_level == "NA"] <- "7"

#as.character(train_df$user_depth) -> train_df$user_depth
#train_df$age_level[train_df$age_level == "NA"] <- "4"

as.character(train_df$city_development_index) -> train_df$city_development_index
train_df$age_level[train_df$age_level == "NA"] <- "5"

str(train_df)
colnames(train_df)
require(fastDummies)

test_df <- fastDummies::dummy_cols(test_df, select_columns = "gender")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "var_1")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "webpage_id")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "campaign_id")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "product")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "age_level")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "city_development_index")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "user_depth")

test_df <- fastDummies::dummy_cols(test_df, select_columns = "product_category_1")



library(caret)
train.rows <- createDataPartition(y=train_df$is_click, p=0.75, list=FALSE)  
library(caret)
train.data<- train_df[train.rows,] # 75% data goes in here
table(train.data$is_click)
23524/323945
test.data<- train_df[-train.rows,] # 25% data goes in here
table(test.data$is_click)
7807/108015

df <- train_dfb

str(df)
as.factor(df$product_category_1) ->df$product_category_1

library(ggplot2)

g <- ggplot(df, aes(gender,is_click))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(CASTE,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(RELIGN,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

g <- ggplot(df, aes(MTONGUE,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +  coord_flip() 

g <- ggplot(df, aes(OCCU,TOTAL))

g + geom_bar(stat = "identity",fill = "blue") +coord_flip() 

colnames(test_df)
str(test_df)
dim(test_df)

p <- predict(model1,test_df, type = "response")
p
fit <- ifelse(p > 0.09,1,0)
test_df$is_click <- fit
table(fit)
roccurve <- roc(test_df$is_click ~ p)
plot(roccurve)
roc_obj <- roc(test_df$is_click, p)
auc(roc_obj)

c = table(test_df$is_click, p > 0.09)
c
train_df$var_1 <- NULL
train_df$city_development_index <- NULL
train_df$product <- NULL
train_df$user_depth <- NULL
train_df$age_level <- NULL
train_df$gender <- NULL
train_df$product_category_1 <- NULL
train_df$webpage_id <- NULL
train_df$campaign_id <- NULL

test_df$var_1 <- NULL
test_df$city_development_index <- NULL
test_df$product <- NULL
test_df$user_depth <- NULL
test_df$age_level <- NULL
test_df$gender <- NULL
test_df$product_category_1 <- NULL
test_df$webpage_id <- NULL
test_df$campaign_id <- NULL

