
#MA Assignment

#Load Data

mydata <- read.csv("C:\\Users\\hp\\Desktop\\PersonalFolder\\ISB CBA\\Marketing Analytics\\Partworth Data.csv", header = TRUE, sep = ",")
#mydata[1] <- NULL
head(mydata)
dim(mydata)
normalized_data <- scale(mydata)
Cluster_Variability <- matrix(nrow=15, ncol=1)
for (i in 1:15) Cluster_Variability[i] <- kmeans(mydata,centers=i,iter.max=20, nstart=4)$tot.withinss
plot(1:15, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot


fit <- kmeans(mydata, centers=2, iter.max=20, nstart=4)
fit

fit$cluster      
fit$centers      
fit$totss        
fit$withinss     
fit$tot.withinss 
fit$betweenss    
fit$size         
fit$iter         

mydataWithClusterMember <- cbind(mydata, fit$cluster) # append cluster membership
head(mydataWithClusterMember)

write.csv(mydataWithClusterMember,file="mydataWithClusterMember.csv")

getwd()

#Linear Discriminant Analysis


cluster2 <- read.csv("C:\\Users\\hp\\Desktop\\PersonalFolder\\ISB CBA\\Marketing Analytics\\2ClusterDiscriminant.csv", header = TRUE, sep = ",")
#mydata[1] <- NULL
head(cluster2)
dim(cluster2)

#install.packages("psych")
library(psych)
pairs.panels(cluster2[1:5], gap = 0, bg = c("red","green")[cluster2$Cluster.Group], pch = 21)


#Data Partition
ind <- sample(2, nrow(cluster2), replace = TRUE, prob = c(.6, .4))
training <- cluster2[ind == 1, ]
test <- cluster2[ind == 2, ]

library(MASS)
linear2 <- lda(Cluster.Group ~ ., training)
linear2
attributes(linear2)


p <- predict(linear2, training)
p

ldahist(data = p$x[,1], g = training$Cluster.Group)

install.packages("klaR")
library(klaR)
partimat(Cluster.Group ~ ., data = training, method = "lda")

#Confusion Matrix and Accuracy

p1 <- predict(linear2, training)$class
tab <- table(Predicted = p1, Actual = training$Cluster.Group)
tab

sum(diag(tab))/ sum(tab)
