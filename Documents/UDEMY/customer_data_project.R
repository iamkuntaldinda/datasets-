
getwd()
# get working dir

cust <- read.csv('Customers.csv',header = TRUE) #import customer dataset
View(cust)
summary(cust)
str(cust)
ncol(cust)
nrow(cust)
dim(cust)
head(cust)
tail(cust)
is.na(cust$Customer.Value) -> V  #to find missing values in customer dataset

V[V == "TRUE"]-> B
(length(B)/length(V))*100 -> percent
percent # 0.36% is the percent of missing values in Customer value variable..

Unique_cust <- unique(cust)
dup_customerID <- cust[duplicated(cust$Customer.ID),]
View(dup_customerID)


my_list <- cust[cust$Customer.Value > 10000,]

dim(my_list)
##binning variables
cust$Customer_Value_segment[cust$Customer.Value >= 25000 ] <- "High_value_segment"
cust$Customer_Value_segment[cust$Customer.Value > 10000 & cust$Customer.Value < 20000] <- "medium_value_segment"
cust$Customer_Value_segment[cust$Customer.Value <= 10000] <- "low_value_segment"

cust$customer_Value_segment  <- NULL #to delete variable
names(cust)

cust$balance_points <- cust$Points.earned - cust$Points.redeemed
Sys.Date()
cust$first.Date <- as.character(cust$first.Date, format = "%Y%m%d")

cust$first.Date <- as.Date(cust$first.Date, format = "%Y%m%d")
class(cust$first.Date)

cust$recent.date <- as.Date(as.character(cust$recent.date),format = "%Y%m%d") #to change format of the dates

class(cust$recent.date)

str(cust)

x <- Sys.Date() - cust$recent.date

install.packages("sqldf", dependencies = TRUE)
require(sqldf) ## for sql queries
names(cust)[names(cust) == "Customer.Value"] <- "Customer_Value"

sql.df <- sqldf('SELECT (Customer_Value)*100/SUM(Customer_Value) as percent_sales,Last_city ,Last_state,Last_region from cust group by Last_city ,Last_state,Last_region')

names(cust)[names(cust) == "Customer.ID"] <- "Customer_ID"   ##to change cusotmer name
sql.df1 <- sqldf('SELECT count(DISTINCT Customer_ID) from cust') 



