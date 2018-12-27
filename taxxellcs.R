
library(readxl)
df<- read_excel("FINAL_DATA_CASESTUDY.xlsx",sheet = "RAW DATA")
colnames(df)
df_m <- df[,c(10,13,14,35)]
df_m <- na.omit(df_m)
dim(df_m)

sapply(df,function(x) sum(is.na(x)))
str(df$weekend)
require(fastDummies)
as.factor(df_m$weekend) -> df_m$weekend

df_m <- fastDummies::dummy_cols(df_m, select_columns = "weekend")
colnames(df_m)

model1 <- lm(price_pre_texon ~ weekend+cushing , data = df_m)
summary(model1)

str(df_m$weekend)
ggplot(aes(x = weekend, y = df_m$price_pre_texon, fill = weekend), data = df_m) + geom_boxplot()

df_n <- df[,c(11,35)]
sapply(df_n,function(x) sum(is.na(x)))
