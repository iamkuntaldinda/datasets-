library(plyr)
library(arules)
library(readr)
library(readxl)
require(xlsx)
require(readxl)
input <- readxl::read_excel("IMB575-XLS-ENG.xls",sheet ="POS DATA")
head(input,10)
summary(input)
str(input)

colnames(input)

##by member and desc
require(reshape2)

##by member and desc

x <- melt(input) 
c <- dcast(x, Member ~ Description,length)
dim(c)
head(c)
write.csv(c,'member.csv')

##by order and desc

y <- melt(input, id=c("Order","Description")) 
d <- dcast(y, Order ~ Description)
dim(d)
head(d)
write.csv(d,'item.csv')

##customer - M08075 - we take the data----

M08075 <- input[input$Member == 'M08075',]
head(M08075)

y1 <- melt(M08075, id=c("Order","Description")) 
d1 <- dcast(y1, Order ~ Description,length)
dim(d1)
head(d1)
write.csv(d1,'M08075.csv')

###










