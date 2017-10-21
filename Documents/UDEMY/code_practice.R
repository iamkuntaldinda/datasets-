

library(ggplot2)
diamonds
qplot(color, data = diamonds, fill=cut) + geom_bar(position = "fill") 

mpg
qplot(cty, hwy, data = mpg) +geom_point(position = "jitter")
install.packages('hexbin')
qplot(carat, price, data = diamonds, geom = "hex")

library(hexbin)
qplot(carat, price, data = diamonds, geom = c("point","density2d"))
qplot(carat, price, data = diamonds, geom = "smooth", group = cut)
qplot(carat, price, data = diamonds, color = I("blue"))

getwd()

ggsave("my.pdf")

qplot(carat,price, data = diamonds, color = cut)

qplot(price, data = diamonds,color = cut, binwidth = 400, geom = "density") + zoom
zoom <- coord_cartesian(xlim = c(300, 18000))

##########
colnames(airports_dat) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")


library(maps)

counties  <- map_data("county")
qplot(long, lat, data = counties, group = group, fill = region, geom = "polygon")

help(package = "maps")

install.packages("MASS")
library(MASS)  
attach(iris)
s <- lm(Petal.Length~Petal.Width+0)
par(mfrow=c(2,2))
plot(s)

abline(lm(Petal.Length~Petal.Width))
data("Cars93")
attach(Cars93)
model <- lm(Weight~MPG.city+EngineSize+Horsepower+Passengers)
colnames(Cars93)
attributes(model)

plot(residuals(model))
fitted(model)


plot(Weight,model$fitted.values,col ='blue')
abline(c(1,1))
training_index <- sample(1:nrow(Cars93), size = round(nrow(Cars93)* .75))
traincars <- Cars93[training_index,]
testcars <- Cars93[-training_index,]
test_pr <- predict(model, testcars)
test_pr
test_actual <- Cars93$Weight[-training_index]
errors <- test_actual - test_pr
plot(errors)

rmseetst <- sqrt(mean(errors^2)) 
rmsetrain <- sqrt(mean(model$residuals^2))

pc_iris <- prcomp(iris[,1:4],scale = T)
summary(pc_iris)

par(mfrow = c(1,1))
biplot(pc_iris)
####XTS object

##extensible time series 

Sys.time()
class(Sys.time())
install.packages('xts')

library(xts)

######

f1 <- function(x,y)
{
  return(x+y)
}


f1 <- function(x,y)
{
  if(is.numeric(x) & is.numeric(y))
  {
    return(x + y)
  }
  else
  {
    print("not numb")
  }
  
  
}


install.packages("asa")

x <- c(1,2,2)

formals(f1)
args(f1)


####debugging functions

Q - will stop debug mode and stop executing the code
c  - will execute remainig code in debug mode itself
undebug command to unflag th debug function

##browser - function will browse through the code individually and will return the answer...

numbers <- numeric(length = 10)
for(i in 1:10)
{
  browser()
  numbers[i] <- i
}

#options(error = browser) R calls brow func if any error

#options(error = NULL) to switch back
##traceback funciton tells where the error occurs


##recursive function
#system.time() func calc time of exceution of func


x <-  matrix(1:15, nrow = 3)
save(x,file = "x.RData")

#to remove all functions from thiss workspace -- use rm(list = ls) function
# load() function will load the dataset
rm(list = ls())
load("x.RDaTa")
x


##can save other file types using foreign package

##read.csv
#read.delim(): tab seperated
#read.delim(sep = "|"):| seperated
#read.fwf() - fixed width



raw<- read.csv("pew.csv",check.names = F)

install.packages("reshape2")
library(reshape2)
tidy <- melt(raw, id = "religion")
names(tidy) <- c("religion","income","n") 

head(tidy)
tidy
weather
Titanic
df <- Titanic
df

##cbind is column appending data
### rbind is row binding data


##for long term storage -- 
write.csv(tidy, file = "tidy.csv", row.names = FALSE)


##for short term caching--- Use RDS file type - use for storing metadata

#files stored with saveRDS command are automatically compressed....
## easy to store compressed files to save space:
write.csv(tidy, file = bzfile("tidy.csv.bz2"))

library(ggplot2)
options(stringsAsFactors = FALSE)  ##prevent R frm reading strings as factors

bnames <- read.csv('bnames.csv.bz2')
births <- read.csv('births.csv')

colnames(bnames)
colnames(births)

x <- bnames[bnames$name == "Garett", ]
x
qplot(year,prop, data = x , geom = "line")

f <- bnames[bnames$name == "Michael" | bnames$name == "Michelle",]
qplot(year, prop, data = f, geom="point", color = interaction(sex,name))

library(dplyr)  ##filter,select, mutate , summarise,arrange
##first arg is dataframe
##output is a dataframe

filter(bnames, sex == "girl" & (year == 1900 | year == 1200))
## pick  - rows by criteria
## select - picks columns by name
###arange(table, column) - arange table by column
##summarise func gives summary of a column

arrange(bnames, desc(prop))

mutate(bnames, prc = prop*100) -> g
head(g)
summarise(bnames, min = min(prop)) -> l
head(l)

summarise(bnames, min = mean(prop)) -> l1
l1
##leftjoin is :: left_join(x,y,by = "name") all rows of left table and matching records of both tables
#all joins :: same syntax
##inner_join :: matching records of both tables
#semi_join()  :: returns match records from first table and some portion of first table
#anti_join()  :: returns rows of first dataframe that do not have records in 2nd dataframe...

left_join(bnames,births,by = c("year","sex"))

inner_join(bnames,births,by = "year")

semi_join(bnames,births,by = "year")

anti_join(bnames,births,by = "year")


b1<- left_join(bnames,births, by = c("year","sex"))
b1 <- mutate(b1, n = round(prop * births))

b2 <- filter(b1, name == "John")
head(b2)
head(b1)
summarise(b2 , sum = sum(n))
summarise(b1, sum = sum(n))


d <- group_by(b1, name)
summarise(d, total = sum(n))

bnames <- tbl_df(bnames)

head(bnames)
bnames
##tbl_df shows the data in a fit way and concise manner as per the size of console

## un-wrap groups by summarise function - move 1 level down  ##
b3 <- group_by(b1, name,sex)
head(b3)

b4 <- summarise(b3, total = sum(n))
head(b4)

b5 <- summarise(b4, total = sum(total))
head(b5)

ungroup(b3)
## group_by() and ungroup()

min(x)
max(x)
median(x)


f <- group_by(b1, soundex)
head(f)
f1 <- summarise(f, total = sum(n))

head(f1)

arrange(f1, desc(total))

J500 <- filter(b1, soundex == "J500")
head(J500)
unique(J500$name)

### advanced manipulation funcs
min() , median(), max(), n(), nth(), n_distinct(), sd(), var(), iqr(), mad().


#### total no f boys for each year...

q1 <-  group_by(b1, year, sex)

q2 <- summarise(q1, total = sum(n))
head(q2)
qplot(year, total, data = q2, geom = "line", color =  sex)
q3 <- group_by(b1, year,sex)
q4 <- mutate(q3, rank = rank(desc(prop)))
head(q4)

ones <- filter(q4, rank == 2)

head(ones)
group_by(q4, year,sex,name) -> q5
arrange(q5, rank) -> q6

head(q6)

summarise(q5, sum = sum(n)) -> q7

arrange(q7, rank(desc(sum))) -> q8
head(q8)

###browseVignettes(package = "dplyr")

###modelling
#response() ~ expalnatory
#dependent ~ independt
#outcome ~ predictors


glm, gam, lm, rlm, ## uses same syntax

df <- read.csv('crime.csv')

x <- lm(df$tc2009~df$low)
plot(x) 

resid(x)
summary(x)

nrow(df)
predict(x)
df$tc2009


#y = ax + b + err

# b is expected value of y when x is 0::
# a is expected increase in y asso. with one unit increase in x:: 

### coef(model) - > 
coef(x)
coefficients(x)

qplot(low,predict(x), data = df, geom = "point")

qplot(low,tc2009, data = df) + geom_smooth(method = lm, se = FALSE)

## every linear model without intercept forces model to (0,0)

# add 1 to independent var - variable for having interceppt term.in model..
# add 0 or (-1) to independent var to remove intercept

s <- lm(tc2009 ~ low - 1, data = df)
summary(s)

wages <- read.csv("wages.csv")
l1 <- lm(earn~height,data = wages)
summary(l1)
plot(l1)

qplot(height,earn, data = wages) + geom_smooth(method = lm)

### every one inch increase in hght is associated with  2387 increase in earnings...

##summary

1. describe relation in data with formula::
  earn ~ heights

2. use a model function to fit a formula ::
  hmod <- lm(earn ~ heights, data = wages)

3. examine output in various ways::
  summary()
plot()


s2 <- lm(earn~race, data = wages)
summary(s2)  

qplot(earn,race,data = wages) + geom_smooth(method = lm)
coef(s2)


##make lowest estimate the baseline coef  - can change order of levels with factor
wages$race <- factor(wages$race, levels = c("hispanic", "white","black", "other"))
s3 <- lm(earn~race, data = wages)
summary(s3)


coef(s3)
anova(s3)

s4 <- lm(earn~sex,data = wages)
summary(s4)

wages$sex
qplot(earn,sex,data = wages)
coef(s4)

wages$sex < - factor(wages$sex, levels = c("sexmale","sexfemale"))
coef(s4)
anova(s4)

qplot(earn, data = wages, geom = "density", color = sex) + theme_bw()

m1 <- lm(earn~height + sex + age +  race + ed, data = wages)
summary(m1)

qplot(height, predict(m1), data = wages) + geom_smooth(method = lm)
qplot(sex, earn, data = wages, color = race) 


####variable selection - 3 ways ::
1.  Theory driven
2.  Data driven
3.  Penalized Regression  - LASSO


2. models shud have small residuals,,,  accurate prdictions.on new data..

---Overfitting means when we make our model 100% fit to training data - then our model will have low training error
but , hight test error-----if we increase variables to increase flexibility of the model --then it can cause overfitting
---
  
  w <- read.csv("wages.csv")
colnames(wages)


s4 <- lm(earn~race,data = wages)
summary(s4)


###data driven
---combination of variables which provides best subset selection  &&& stepwise selection---
  ---best model has less test error 

##best subset selection of variables
-- find prediction error/ test error fr every possible set of variables -- use subset with lowest test error--
  
  
  -- best subset selection in R::
  1 - find best subsets of each size with regsubsets in leaps package::
  2 = compare different sizes with adjusted Rsquare - , Cp, BIC statistical models::
  
  3. sometimes, if no of variables are huge, it will be difficult to search best model, 
we will use stepwise regression in this case:
  
  1. backward selection
2. forward selection


install.packages('leaps')
library(leaps)
s1 <- regsubsets(earn~.,data = wages)
summary(s1)

summary(s1)$adjr2
summary(s1)$cp
summary(s1)$bic

x <- rep(1:7,3)

x
df <- data.frame(est = c(summary(s1)$adjr2, summary(s1)$cp, summary(s1)$bic),x <- rep(1:7,3), type = rep(c("adjr2","cp","bic"), each = 7))
install.packages('ggplot2')
library(ggplot2)
qplot(x,est,data = df,geom = "line") + theme_bw() + facet_grid(type ~ ., scales = "free_y")

coef(s1,3)

library('leaps')
###for crime dataset
at <- read.csv('crime.csv')
s2 <- regsubsets(crime~.,data=x)
summary(a2)
summary(a2)$adjr
summary()$cp
summary()$bic

colnames(at)

x<- read.csv('counties.csv')
library(dplyr)
c1 <- select(x,'state','county')
colnames(c1)


full <- lm(x$crime ~., data = x)
empty <- lm(x$crime ~ 1, data = x)
start <- lm(x$crime ~ pop, data = x)

step(start, scope = list(upper = full, lower = empty), direction = "forward")


---step function can be used for iterating the stepwise regression --
-- step regression also leads to overfitting oof model
























