sessionInfo() #question 1
installed.packages()

#ques2,3,4
abc <- 3
a = 2
b = "a1"
c = TRUE
ls()

#ques5
x <- c(4,4,5,6,7,2,9)

length(x) 
mean(x)
max(x)
min(x)
sum(x)
var(x)

print(x)
print(x[3])

n <- length(x)
x[seq(n) %% 2 == 1] # odd position elements

print(x[2:6])


#ques6

mat <- matrix(1:24,6,4)
mat

#data frame - Q7

df <- data.frame(StoreID = c(111, 208, 113, 408),
                 Tenure = c(25, 34, 28, 52),
                 StoreType = c("Type1", "Type2", "Type1", "Type1"),
                 status = c("Poor", "Improved", "Excellent", "Poor"))

#Ques 8
prog1 <- data.frame(df$StoreID,df$Tenure)
prog2 <- data.frame(df$StoreType,df$status)
prog3 <- data.frame(df$Tenure)

# factors - Q9
ethnicity <- factor(c("White", "African american", "White", "Asian"))
status <- factor(c("Poor", "Improved", "Excellent", "Poor"))
sort(levels(status)) -> a1
outcome <- c(1, 3, 2, 4, 3, 1, 1)
outcome <- factor(outcome,levels = c('1','2','3','4'),labels = c("Poor","Average","Good","Excellent"))
attributes(outcome)


# question10
my_list <- list(h <- c(25,26,18,39), j <- matrix(1:10,5,2), k<- c("one","two","three"))
names(my_list) <- c("h","j","k")
attributes(my_list)

###how to get title of list??


#ques11
stores1 <- read.csv('stores.csv',stringsAsFactors = FALSE)
View(stores1)
summary(stores1) -> c1
class(c1) # class is "table"

#question 12 - using with function , class is "summaryDefault" "table"

with(stores1,summary(stores1$OperatingCost)) -> c
class(c)

#ques13
class(stores1)
names(stores1)
length(stores1)
dim(stores1)
str(stores1)
head(stores1)
tail(stores1)
fix(stores1)

#ques14
#method1
stores1$total_cost <- stores1$OperatingCost +stores1$AcqCostPercust

#method2
stores1 <- transform(stores1, total_cost <- OperatingCost*AcqCostPercust) 

#ques15
stores1$store_class <- cut(stores1$TotalSales,
                           breaks = c(-Inf,120,240,Inf),
                           labels = c('Low Perform store','Average Perform store','High Perform store')) 


#ques16
names(stores1)[names(stores1) == "AcqCostPercust"] <- "AcqCost"


#ques17
#to fetch missing values
is.na(stores1)
#to recode missing values with 0
stores1[is.na(stores1)] <- 0
View(stores1)  

#to delete missing observations 
stores2 <- na.omit(stores1)


#ques18

newstores <- stores1[order(stores1$StoreType),] #first part
newstores <- stores1[order(stores1$Location,-stores1$TotalSales),] #second part
View(newstores)

#ques19

date1 <- c("22/06/14","13/02/14")
date2 <- c("01/05/1965", "08/16/1975")

date1 <- as.Date(date1,format = "%d/%m/%y")
date2 <- as.Date(date2,format = "%m/%d/%Y")

#ques20
#part - 1
s1 <- stores1[c(5,7,8,9)]

#part - 2
s1 <- stores1[-c(5,7,8,9)]
names(s1)

#part - 3
s2 <- stores1[1:10,]

#part - 4
s3 <- stores1[stores1$StoreType == "Apparel" & stores1$TotalSales > 100,]

#part - 5
require(dplyr)
s4 <- stores1[between(stores1$TotalSales,100,300),c("StoreCode","StoreName","Location","TotalSales")]


#part -6
s5 <- stores1[stores1$StoreType =="Electronincs"& stores1$TotalSales > 100, 1:10]
View(s5)



