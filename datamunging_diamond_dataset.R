library(ggplot2)
data("diamonds")
head(diamonds)

###Aggregating data

aggregate(diamonds$price, by = list(diamonds$cut),mean)

##second way---
aggregate(price~cut,diamonds,mean)

###third way----
tapply(diamonds$price, diamonds$cut, mean)

### ----------
require(plyr)

ddply(diamonds,"cut", summarize, Price = mean(price))
daply(diamonds,"cut", summarize, Proce = mean(price))


###---Parallel processing

install.packages("doParallel")
require(doParallel)

cl <- makeCluster(2)
registerDoParallel(cl)

ddply(diamonds,"cut", function(x) mean(x$price), .parallel = TRUE)


###USE DATA.TABLE PACKAGE - one of the fastest way to do data aggregation in R
install.packages('data.table')
require(data.table)
head(diamonds)
dia<- data.table(diamonds)
dia

dia[,mean(price), by = cut]


require(dplyr)
dim(diamonds)

diamonds %>% head()
diamonds %>% dim()

diamonds %>% group_by(cut) %>% dplyr::summarize(Price = mean(price))


####---- in Database using dplyr
download.file("http://www.jaredlander.com/data/diamonds.db",destfile = "diamonds.db", method = "curl")

diaDBS <- src_sqlite("diamonds.db")
diaDB <- tbl(diaDBS,"diamonds")
diaDB

diaDB %>% group_by(cut) %>% dplyr::summarize(Price = mean(price))
