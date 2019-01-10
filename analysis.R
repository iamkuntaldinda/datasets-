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

model2 <- lm(calls ~ reservations+treatment+pageviews+restaurant_type , data = df)
summary(model2)



#################


# Single Sample t-test- Test against hypothesis value

# Question: Comparing card usage of post campaign of 1 month with last year 

#hypothesized value 50



t.test(cust_seg$Post_usage_1month, mu = 50)





#Dependent Group Test - Compare means for the same sample at two different time periods

#Question: Comparing means for card usage of pre & post usage of campaign



t.test(cust_seg$Post_usage_1month, cust_seg$pre_usage, paired = TRUE)





#Independent Group Test - Compare means for the two different samples 

#Question: Comparing means of card usage for males & females



t.test(cust_seg$Latest_mon_usage ~ cust_seg$sex, data = cust_seg, var.equal = TRUE) # Equal variance Assumed

t.test(cust_seg$Latest_mon_usage ~ cust_seg$sex, data = cust_seg) # Un equal variance


