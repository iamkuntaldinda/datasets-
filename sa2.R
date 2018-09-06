# Read The Excel Data
require(xlsx)
AB=read.xlsx("C:/Users/31074/Desktop/AB.xlsx", sheetName = "Alcoholic Beverages")
#Boxplot
par(mfrow = c(1,2))
boxplot(AB$Advertising.Expenditure,data=AB,main="Advertising.Expenditure")
boxplot(AB$Profit,data=AB,main="Profit")
par(mfrow = c(1,1))
#Scatter Plots
library(gridExtra)
p1=ggplot(AB, aes(x=Year, y=Advertising.Expenditure)) + geom_point(color="blue")
p2=ggplot(AB, aes(x=Year, y=Profit)) + geom_point(color="red")
grid.arrange(p1, p2,nrow = 2)
#Build the model
model1=lm(Profit~Advertising.Expenditure,data=AB)
# Regression summary
summary(model1)
#Regression Line
library(ggplot2)
ggplot(data = AB, aes(x = Advertising.Expenditure, y = Profit)) + geom_point(color='blue') +geom_smooth(color="red",method='lm')
#QQplot
library(car)
car::qqPlot(model1)
#or,
#QQ-plot of the residuals
qqnorm(rstandard(model1))
qqline(rstandard(model1))
#residual Plots
car::residualPlots(model1)
#ANOVA Table
anova(model1)

#Adding Regression lines and break it up
coeff=coefficients(model1)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(AB$Profit,AB$Advertising.Expenditure,xlab="Advertising.Expenditure",ylab="Profit",main=eq,col="red",pch=19)
abline(model1,lwd=2)
abline(h=mean(AB$Profit),lwd=2,col="blue")

#Correlation Plot
library(dplyr)
mydata=select(AB, -Year)
library(corrplot)
M=cor(mydata)
corrplot(M, method="number",type="lower")
#Partial Correlation:
library(corpcor)
cor2pcor(M)