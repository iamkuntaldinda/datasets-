install.packages('plyr')
install.packages('arules')
install.packages('readr')
install.packages('readxl')
install.packages('arulesViz')
install.packages('xlsx')
install.packages('reshape2')


library(plyr)

library(arules)

library(readr)

library(readxl)

require(xlsx)

require(readxl)

library(arulesViz)

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

head(d)

write.csv(d,'item.csv')



##customer - M08075 - we take the data----



M08075 <- input[input$Member == 'M08075',]

head(M08075)

dim(M08075)

y1 <- melt(M08075, id=c("Order","Description")) 

d1 <- dcast(y1, Order ~ Description,length)

d2 <- d1[,c(-1)]

dim(d2)

head(d2)

View(d2)

d2[d2 != 0] <- 1

dim(d1)





write.csv(d1,'M08075.csv')

write.csv(d2,'M08075_products.csv')



###

df.mat <- as.matrix(d2)

trs <- as(df.mat,"transactions")

inspect(trs)

rules = apriori(trs, parameter=list(support=0.18, confidence=0.10,target = "rules")) 



rules_high_lift <- (head(sort(rules, by="lift"),n=15))

inspect(head(sort(rules, by="lift"),n=15))

plot(rules_high_lift, method="graph", control=list(type="items"))



plot(rules)



##did you forget items ---

## {Banana,Beans,Other Vegetables} => {Gourd & Cucumber} 0.2181818  0.9230769 1.410256    12

#############



##Q6

y <- melt(input, id=c("Order","Description")) 

d <- dcast(y, Order ~ Description)

dim(d)

head(d)

write.csv(d,'item.csv')

items_All <- d[,c(-1)]

dim(items_All)

head(items_All)

items_All[items_All != 0] <- 1

write.csv(items_All,'all_products.csv')



###

df.mat <- as.matrix(items_All)

trs <- as(df.mat,"transactions")

inspect(trs)



rules = apriori(trs, parameter=list(support=0.10, confidence=0.50,target = "rules")) 

rules_high_lift <- (head(sort(rules, by="lift"),n=18))

inspect(head(sort(rules, by="lift"),n=18))



##> inspect(head(sort(rules, by="lift"),n=18))

#lhs                                    rhs                support   confidence lift     count

#[1]  {Brinjals,Other Vegetables}         => {Gourd & Cucumber} 0.1002742 0.5812025  1.935111  841 

#[2]  {Gourd & Cucumber,Other Vegetables} => {Brinjals}         0.1002742 0.5137447  1.885679  841 

#[14] {Other Vegetables,Root Vegetables}  => {Beans}            0.1535710 0.6351085  1.591471 1288 



plot(rules_high_lift, method="graph", control=list(type="items"))

plot(rules_high_lift, method = "grouped", control = list(k = 10))

plot(rules_high_lift,measure=c("support","lift"),shading="confidence",engine = 'interactive')



##Q7

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector

length(subsetRules)  #> 3913

rules <- rules[-subsetRules] # remove subset rules. 



#############
install.packages('recommenderlab')
library(recommenderlab)

library(reshape2)



View(input)
input$const <- 1
members_matrix <- as.matrix(acast(input, Member~Description, fun.aggregate = mean))
head(members_matrix)

###

R <- as(members_matrix, "realRatingMatrix")
rec1 = Recommender(R, method="UBCF") ## User-based collaborative filtering
Recfinal=Recommender(R,method="UBCF",param=list(normalize = "Z-score",method="cosine",nn=5))

rec2 = Recommender(R, method="IBCF") ## Item-based collaborative filtering

rec3 = Recommender(R, method="SVD")

rec4 = Recommender(R, method="POPULAR")

rec5 = Recommender(binarize(R,minRating=2), method="UBCF") ## binarize all 2+ rating to 1



## create n recommendations for a user

uid = "M08075" 

prediction <- predict(rec1, R[uid], n=30) ## you may change the model here

as(prediction, "list")



# to obtain the top 4

prediction_M08075.top3 <- bestN(prediction, n = 4)

as(prediction_M08075.top3, "list")



##using Recfinal

prediction_model <- predict(Recfinal, R[uid], n=10) ## you may change the model here

as(prediction_model, "list")



#to predict affinity to all non-rated items 

predicted.affinity.M08075 <- predict(rec1, R["M08075",], type="ratings")

# to see the user "M08075"'s predicted affinity for items we didn't have any value for

as(predicted.affinity.M08075, "list")

# .. and the real affinity for the items obtained from the affinity.matrix

as(R["M08075",], "list")





###testing for 4 more users 



uid = "M38622" 

items <- subset(c, c$Member==uid)

prediction <- predict(rec1, R[uid], n=5) ## you may change the model here

as(prediction, "list")



uid = "M14746" 
prediction <- predict(rec1, R[uid], n=5) ## you may change the model here

as(prediction, "list")



uid = "M78720" 
prediction <- predict(rec1, R[uid], n=5) ## you may change the model here
as(prediction, "list")



uid = "M36702" 

prediction <- predict(rec1, R[uid], n=5) ## you may change the model here

as(prediction, "list")

###Testing using validation method- removing some users products data and then testing the accuracy.

##for users - M78720 and M36702, I will remove 2 items that they buy, and will test in recommendation , whether they get it or not.
M78720 <- input[input$Member == 'M78720',]
head(M78720)

dim(M78720)
M78720 <- M78720[M78720$Description != 'Chips' & M78720$Description != 'Instant Pastas',]  
dim(M78720)


###deleting rows of M78720 from input datASET
input <-  input[input$Member != 'M78720',]
dim(input)

input <- rbind(input,M78720)
dim(input)
#testing it
members_matrix <- as.matrix(acast(input, Member~Description, fun.aggregate = mean))
head(members_matrix)

R <- as(members_matrix, "realRatingMatrix")
rec1 = Recommender(R, method="UBCF") ## User-based collaborative filtering
uid = "M78720" 
prediction <- predict(rec1, R[uid], n=100) ## you may change the model here
as(prediction, "list")

##chips and Instant Pastas are the items...
####
M36702 <- input[input$Member == 'M36702',]
head(M36702,19)
dim(M36702)
M36702 <- M36702[M36702$Description != 'Banana' & M36702$Description != 'Face Wash',]  
dim(M36702)

##Banana and Face Wash will be removed

###deleting rows of M36702 from input datASET
input <-  input[input$Member != 'M36702', ]
dim(input)

input <- rbind(input,M36702)
dim(input)
#testing it
members_matrix <- as.matrix(acast(input, Member~Description, fun.aggregate = mean))
head(members_matrix)

R <- as(members_matrix, "realRatingMatrix")
rec1 = Recommender(R, method="UBCF") ## User-based collaborative filtering
uid = "M36702" 
prediction <- predict(rec1, R[uid], n=100) ## you may change the model here
as(prediction, "list")
###Banana and face wash are present


