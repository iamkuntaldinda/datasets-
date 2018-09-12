require(forecast)
require(zoo)

X <- read.csv('olympics.csv')
dim(X)
colnames(X)
str(X)
head(X)
X$Medal <- as.character(X$Medal)
X <- subset(X, Medal!= "NA" )
dim(X)
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


library("sqldf")
x <- sqldf("SELECT Year,COUNT(ID) as no_of_obs FROM X GROUP BY Year")
x

x1 <- sqldf("SELECT Year,COUNT(Medal) as no_of_medals FROM X GROUP BY Year")
x1 <- as.data.frame(x1)

y_total <- X

##find count of missing values
sapply(y_total, function(x) sum(is.na(x)))


##data preparation
library("cluster")
library("factoextra")
library("magrittr")
library("dplyr")
y_total_numeric <- select_if(y_total, is.numeric)

# Replace values with mean: Height
y_total_numeric$Height[is.na(y_total_numeric$Height)] <-mean(y_total_numeric$Height,na.rm=T)
table(is.na(y_total_numeric$Height ))

y_total_numeric$Weight[is.na(y_total_numeric$Weight)] <-mean(y_total_numeric$Weight,na.rm=T) 
table(is.na(y_total_numeric$Weight ))

y_total_numeric$Age[is.na(y_total_numeric$Age)] <-mean(y_total_numeric$Age,na.rm=T) 
table(is.na(y_total_numeric$Age ))

###
myvars <- c("Age", "Height", "Weight")
newdata <- y_total_numeric[myvars]
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

##clustering by season
colnames(X)
summer <- X[X$Season == 'Summer',]
dim(summer)
dim(X)
summer_numeric <- select_if(summer, is.numeric)

###
# Replace values with mean: Height
summer_numeric$Height[is.na(summer_numeric$Height)] <-mean(summer_numeric$Height,na.rm=T)
table(is.na(summer_numeric$Height ))

summer_numeric$Weight[is.na(summer_numeric$Weight)] <-mean(summer_numeric$Weight,na.rm=T) 
table(is.na(summer_numeric$Weight ))

summer_numeric$Age[is.na(summer_numeric$Age)] <-mean(summer_numeric$Age,na.rm=T) 
table(is.na(summer_numeric$Age ))


###
myvars <- c("Age", "Height", "Weight")
newdata <- summer_numeric[myvars]
dim(newdata)

my_data <- newdata %>%
  na.omit() %>%          # Remove missing values (NA)
  scale() 

## Determine number of clusters
Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(my_data,centers=i, nstart=4)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot


set.seed(100)
km_s <- kmeans(my_data, 4, nstart = 25)
summary(km_s)
km_s$withinss
dd <- cbind(summer_numeric, cluster = km_s$cluster)
head(dd)

write.csv(dd,file = 'summer_clusters.csv')

####same for winter
winter <- X[X$Season == 'Winter',]
dim(winter)
winter_numeric <- select_if(winter, is.numeric)

###
# Replace values with mean: Height
winter_numeric$Height[is.na(winter_numeric$Height)] <-mean(winter_numeric$Height,na.rm=T)
table(is.na(winter_numeric$Height ))

winter_numeric$Weight[is.na(winter_numeric$Weight)] <-mean(winter_numeric$Weight,na.rm=T) 
table(is.na(winter_numeric$Weight ))

winter_numeric$Age[is.na(winter_numeric$Age)] <-mean(winter_numeric$Age,na.rm=T) 
table(is.na(winter_numeric$Age ))

###
myvars <- c("Age", "Height", "Weight")
newdata <- winter_numeric[myvars]
dim(newdata)

my_data <- newdata %>%
  na.omit() %>%          # Remove missing values (NA)
  scale() 

## Determine number of clusters
Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(my_data,centers=i, nstart=4)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot

set.seed(100)
km_w <- kmeans(my_data, 4, nstart = 25)
summary(km_w)
km_w$withinss
dd <- cbind(winter_numeric, cluster = km_w$cluster)
head(dd)
write.csv(dd,file = 'winter_clusters.csv')
##find count of missing values
sapply(winter, function(x) sum(is.na(x)))


##VISUALISATION
g <- ggplot(summer, aes(summer$Year))
g + geom_bar(aes(fill=Medal), width = 2) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 


###line plot
x_gold <- sqldf("SELECT Year,COUNT(Medal) as no_of_medals FROM summer WHERE Medal == 'Gold' GROUP BY Year")
x_gold <- as.data.frame(x_gold)


g <- ggplot(x_gold, aes(x_gold$Year))
g + geom_line(aes(Year,no_of_medals),col = "blue") + lines()+ 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 

gold.ts <- ts(x_gold$no_of_medals, start = c(1896), end = c(2016), freq = 1/2)
plot(gold.ts, type = 'o',col= 'blue')
plot(gold.ts, ylab="Shampoo Sales",ylim=c(min(gold.ts),max(gold.ts)),bty="l", type="b")
gold <- tslm(gold.ts~trend)
lines(gold$fitted, lwd=2)
# plot
treeMapCoordinates <- treemapify(proglangs,
                                 area = "value",
                                 fill = "parent",
                                 label = "id",
                                 group = "parent")

treeMapPlot <- ggplotify(treeMapCoordinates) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2")

print(treeMapPlot)

#####  which countries dominate

x <- sqldf("SELECT Team,COUNT(Medal) as no_of_medals FROM X GROUP BY Team ORDER BY no_of_medals DESC LIMIT 20")
x
g <- ggplot(x, aes(Team,no_of_medals))
g + geom_bar(stat = "identity",fill = "blue") +
  coord_flip() 

###medals distribution by top 20 countries
#####  which countries dominate

x <- sqldf("SELECT Team,COUNT(Medal) as no_of_medals,Medal FROM X GROUP BY Team, Medal ORDER BY no_of_medals DESC LIMIT 50")
x
g <- ggplot(x, aes(Team,no_of_medals))
g + geom_bar(stat = "identity",aes(fill = Medal)) +
  coord_flip() 

#### Top sport
x <- sqldf("SELECT Sport,COUNT(Medal) as no_of_medals,Medal FROM X GROUP BY Sport, Medal ORDER BY no_of_medals DESC LIMIT 50")
x

g <- ggplot(x, aes(Sport,no_of_medals))
g + geom_bar(stat = "identity",aes(fill = Medal)) +
  coord_flip() 
box + scale_fill_manual(values=c( "#E69F00", "#56B4E9"))

###by gender
X$Sex
x <- sqldf("SELECT Sex,COUNT(Medal) as no_of_medals,Medal FROM X GROUP BY Sex, Medal ORDER BY no_of_medals DESC LIMIT 50")
x

g <- ggplot(x, aes(Sex,no_of_medals))
g + geom_bar(stat = "identity",aes(fill = Medal)) +
  coord_flip() 


###top 20  athletes
x <- sqldf("SELECT Name,COUNT(Medal) as no_of_medals,Sex,team  FROM X GROUP BY Name, Medal ORDER BY no_of_medals DESC LIMIT 20")
x

###top 10 male athletes
y <- sqldf("SELECT Name,COUNT(Medal) as no_of_medals,Sex,team  FROM X WHERE Sex = 'M' GROUP BY Name, Medal ORDER BY no_of_medals DESC LIMIT 10")
y

###top 10 female athletes
x <- sqldf("SELECT Name,COUNT(Medal) as no_of_medals,Sex,team  FROM X WHERE Sex = 'F' GROUP BY Name, Medal ORDER BY no_of_medals DESC LIMIT 10")
x

####by mean of age


####by mean of height

####Best male and female athletes



###clustering by gdp and population of countries

