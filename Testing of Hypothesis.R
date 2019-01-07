#=====================================================================================#
#     Testing of Hypothesis: Z Test, t Test, F Test, chi-square Test and Correlation #
#=====================================================================================#

cust_seg <- read.csv("cust_seg.csv")
View(cust_seg)
names(cust_seg)

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

# ANOVA:F-Statistics
#Question: Comparing means of card usage of different segment customers

#Method-1

ANOVA_segment <- aov(Latest_mon_usage ~ segment, data = cust_seg)
summary(ANOVA_segment)

# Show the means
model.tables(ANOVA_segment, "means")

#Method-2
m <- lm(Latest_mon_usage ~ segment, data = cust_seg)
anova(m)
summary(m)


# Chi-Square Statistics - finding the relationship between categorical variabels(Correlation between categorical variables)
# Question: FInding the relationship between region and Segment

tab <- xtabs(~region + segment, data = cust_seg)
chisq.test(tab)


 # Correlation - Finding the relationship between continuous variables
#Question: What is the correlation between latest_mon_usage and pre_usage

cor(cust_seg$Latest_mon_usage,cust_seg$pre_usage)


df <- read.csv('dietstudy.csv')
head(df,3)
colnames(df)
t.test(df$tg0, df$tg4,paired = TRUE)
t.test(df$wgt0, df$wgt4, paired = TRUE)

df1 <- read.csv('creditpromo.csv')
head(df1)
df1$insert <- as.factor(df1$insert)

cor.test(df1$insert,df1$dollars)


