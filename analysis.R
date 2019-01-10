library(readxl)
df<- read_excel("RGdata.xlsx",sheet = "ads_analysis%202_1fd2e6d3-bf38-")
colnames(df)

dim(df)

str(df)

df_numeric <- df[,c(2,3,4)]
pairs(df_numeric)
cor.test()

df$treatment <- as.factor(df$treatment)

require(ggplot2)
ggplot(aes(x = treatment, y = calls, fill = restaurant_type), data = df) + geom_boxplot()


require(ggplot2)
g <- ggplot(df, aes(treatment,calls))
g <- ggplot(df, aes(treatment,reservations))
g <- ggplot(df, aes(treatment,pageviews))

g + geom_bar(stat = "identity",aes(fill = treatment))

colnames(df)
str(df)
require(fastDummies)

df_current <- df[df$treatment == 1,]
df_current <- as.data.frame(df_current)

df_alternative <- df[df$treatment == 2,]
df_alternative <- as.data.frame(df_alternative)
str(df_current)
str(df_alternative)
df_alternative$restaurant_type <- as.factor(df_alternative$restaurant_type)
##model for alternative treatment ads
head(df_alternative)
model1 <- lm(calls ~ reservations+pageviews+restaurant_type , data = df_alternative)
summary(model1)

##model for current treatment ads

model2 <- lm(calls ~ reservations+pageviews+restaurant_type , data = df_current)
summary(model2)

#################
new_model <- lm(calls ~ reservations+pageviews+restaurant_type+treatment , data = df)
summary(new_model)

##for Chain type restaurant
df_chain <- df[df$restaurant_type == 'chain',]
new_model1 <- lm(calls ~ reservations+pageviews+treatment , data = df_chain)
summary(new_model1)


###for independent type restaurant
df_independent <- df[df$restaurant_type == 'independent',]
new_model2 <- lm(calls ~ reservations+pageviews+treatment , data = df_independent)
summary(new_model2)


# Single Sample t-test- Test against hypothesis value

# Question: Comparing card usage of post campaign of 1 month with last year 

#hypothesized value 50



t.test(cust_seg$Post_usage_1month, mu = 50)




##divide the two groups on basis of treatments..

#Dependent Group Test - Compare means for the same sample at two different time periods

#Question: Comparing means for card usage of pre & post usage of campaign

t.test(cust_seg$Post_usage_1month, cust_seg$pre_usage, paired = TRUE)


#Independent Group Test - Compare means for the two different samples 

#Question: Comparing means of card usage for males & females

t.test(cust_seg$Latest_mon_usage ~ cust_seg$sex, data = cust_seg, var.equal = TRUE) # Equal variance Assumed

t.test(cust_seg$Latest_mon_usage ~ cust_seg$sex, data = cust_seg) # Un equal variance


x <- df$treatment

ad_type <- ifelse((x==0),"control group",
                 ifelse((x==1),"advertisement",
                        ifelse((x==2),"advertisement","no")))

df$ad_type <- ad_type

##new model
model3 <- lm(calls ~ reservations+pageviews+restaurant_type+ad_type , data = df)
summary(model3)
