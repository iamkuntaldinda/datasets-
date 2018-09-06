X <- read.csv('olympics.csv')
dim(X)
colnames(X)
str(X)
head(X)

##find count of missing values
sapply(X, function(x) sum(is.na(x)))

##no of medals by each team
# plot
library(ggplot2)
theme_set(theme_classic())

##no of teams
library(dplyr)
X %>%
  group_by(Year) %>%
  summarise(n_distinct(Medal))

# Plot
g <- ggplot(X, aes(Year, Medal))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2")

library("sqldf")

x <- sqldf("SELECT Year,COUNT(ID) as no_of_obs FROM X GROUP BY Year")
x

y_1992 <- sqldf("SELECT * FROM X WHERE Year >= 1992")
dim(y_1992)


##find count of missing values
sapply(y_1992, function(x) sum(is.na(x)))


##data preparation
library("cluster")
library("factoextra")
library("magrittr")


library("dplyr")
y_1192_numeric <- select_if(y_1992, is.numeric)

###
# Replace values with mean: Height
y_1992$Height[is.na(y_1992$Height)] <-mean(y_1992$Height,na.rm=T)
table(is.na(y_1992$Height ))

y_1992$Weight[is.na(y_1992$Weight)] <-mean(y_1992$Weight,na.rm=T) 
table(is.na(y_1992$Weight ))

y_1992$Age[is.na(y_1992$Age)] <-mean(y_1992$Age,na.rm=T) 
table(is.na(y_1992$Age ))


###
myvars <- c("Age", "Height", "Weight")
newdata <- y_1192_numeric[myvars]
dim(newdata)

# Replace values with mean: Height
newdata$Height[is.na(newdata$Height)] <-mean(newdata$Height,na.rm=T)
table(is.na(newdata$Height ))

newdata$Weight[is.na(newdata$Weight)] <-mean(newdata$Weight,na.rm=T) 
table(is.na(newdata$Weight ))


my_data <- newdata %>%
  na.omit() %>%          # Remove missing values (NA)
  scale() 
dim(my_data)

my_data <- as.data.frame(my_data)
## Box Plot: Height
boxplot(newdata$Height)
## Quantile values: Height
quantile(newdata$Height,prob=c(0,0.01,0.05,0.25,0.5,0.75,0.95,0.99,1.0))
## Variable value capping: Height
newdata$Height[newdata$Height > 205] 
newdata$Height[newdata$Height < 150] 
## BOx Plot: Weight
boxplot(newdata$Weight)
newdata$Weight[newdata$Weight < 45] 
quantile(newdata$Weight,prob=c(0,0.01,0.05,0.25,0.5,0.75,0.95,0.99,1.0))

## Determine number of clusters
Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(my_data,centers=i, nstart=4)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot

##clustering
set.seed(100)
km <- kmeans(my_data, 4, nstart = 25)
summary(km)
dd <- cbind(y_1992, cluster = km$cluster)
head(dd)

write.csv(dd,file = 'clusters.csv')

##visualising top 2 dimensions
fviz_cluster(km, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

