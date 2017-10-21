install.packages('Quandl')
library(Quandl)
install.packages('tidyverse')
install.packages('tidyquant')
install.packages('timetk')
install.packages('forecast')
install.packages('gridExtra')
library(tidyverse)
library(tidyquant)
library(timetk)
library(forecast)
library(gridExtra)
##-iWsHhuftk1-DKFbKZ2v
Quandl.api_key("-iWsHhuftk1-DKFbKZ2v")
ICICI = Quandl("NSE/ICICIBANK",collapse="daily",start_date="2016-10-05",type="raw")
AUROPHARMA = Quandl("NSE/AUROPHARMA",collapse="daily",start_date="2016-10-05",type="raw")
YESBANK = Quandl("NSE/YESBANK",collapse="daily",start_date="2016-10-05",type="raw")
GLENMARK = Quandl("NSE/GLENMARK",collapse="daily",start_date="2016-10-05",type="raw")
TCS = Quandl("NSE/TCS",collapse="daily",start_date="2016-10-05",type="raw")

##
ICICI <- cbind(ICICI,stock = "")
AUROPHARMA <- cbind(AUROPHARMA,stock = "")
YESBANK <- cbind(YESBANK,stock = "")
GLENMARK <- cbind(GLENMARK,stock = "")
TCS <- cbind(TCS,stock = "")


###
ICICI$stock <- paste(ICICI$stock,"ICICI",sep = "")
AUROPHARMA$stock <- paste(AUROPHARMA$stock,"AUROPHARMA",sep = "")
YESBANK$stock <- paste(YESBANK$stock,"YESBANK",sep = "")
GLENMARK$stock <- paste(GLENMARK$stock,"GLENMARK",sep = "")
TCS$stock <- paste(TCS$stock,"TCS",sep = "")

ICICI

###

Master_Data <- rbind(ICICI,AUROPHARMA,YESBANK,GLENMARK,TCS)
str(Master_Data)
Master_Data$Date <- as.Date(Master_Data$Date)

##
end <- ymd("2017-10-05")
  start <- ymd("2016-10-05")

  
  Master_Data <- Master_Data %>% tibble::as_tibble() %>% group_by(stock)

  
  library(ggplot2)
  ## Visualisation of BBand in ggplot2 
  
  Master_Data%>%filter(stock=="ICICI"|stock=="YESBANK"|stock=="TCS"| stock == "GLENMARK")%>%ggplot(aes(x=Date,y=Close))+
    geom_line(size=1)+
    geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
                color_ma = "royalblue4", color_bands = "red1")+
    coord_x_date(xlim = c(start, end), expand = TRUE)+
    facet_wrap(~ stock, scales = "free_y")+
    labs(title = "Bollinger Band", x = "Date",y="Price") +
    theme(text = element_text(family = 'Calibri', color = "#444444",hjust=0.5)
          ,panel.background = element_rect(fill = 'lightyellow')
          ,panel.grid.minor = element_blank(),
          ,panel.grid.major = element_blank()
          ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
          ,axis.title = element_text(size = 18, color = '#555555')
          ,axis.title.y = element_text(hjust=0.5,size=15)
          ,axis.title.x = element_text(hjust = 0.5,size=15)) +theme(legend.position="none")
  
  
  
  Master_Data%>%filter(stock=="ICICI"|stock=="YESBANK"|stock=="TCS"| stock == "GLENMARK")%>%ggplot(aes(x=Date,y=Close))+
    geom_line(size=1)+
    geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
                color_ma = "royalblue4", color_bands = "red1")+
    coord_x_date(xlim = c(start, end), expand = TRUE)+
    facet_wrap(~ stock, scales = "free_y")+
    labs(title = "Bollinger Band", x = "Date",y="Price") +
    theme(text = element_text(family = 'Calibri', color = "#444444",hjust=0.5)
          ,panel.background = element_rect(fill = 'lightyellow')
          ,panel.grid.minor = element_blank()
          ,panel.grid.major = element_blank()
          ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
          ,axis.title = element_text(size = 18, color = '#555555')
          ,axis.title.y = element_text(hjust=0.5,size=15)
          ,axis.title.x = element_text(hjust = 0.5,size=15)) +theme(legend.position="none")
  
  
  
  
  Master_Data%>%filter(stock=="AUROPHARMA"| stock == "GLENMARK")%>%ggplot(aes(x=Date,y=Close))+
    geom_line(size=1)+
    geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
                color_ma = "royalblue4", color_bands = "red1")+
    coord_x_date(xlim = c(start, end), expand = TRUE)+
    facet_wrap(~ stock, scales = "free_y")+
    labs(title = "Bollinger Band", x = "Date",y="Price") +
    theme(text = element_text(family = 'Calibri', color = "#444444",hjust=0.5)
          ,panel.background = element_rect(fill = 'lightyellow')
          ,panel.grid.minor = element_blank()
          ,panel.grid.major = element_blank()
          ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
          ,axis.title = element_text(size = 18, color = '#555555')
          ,axis.title.y = element_text(hjust=0.5,size=15)
          ,axis.title.x = element_text(hjust = 0.5,size=15)) +theme(legend.position="none")
  
  
  ####
  
  NTPC = Quandl("NSE/NTPC",collapse="monthly",start_date="2016-10-01",type="raw")
  AMBUJACEM =Quandl("NSE/AMBUJACEM",collapse="monthly",start_date="2016-10-01",type="raw")
  
  ##
  NTPC_df=NTPC
  AMBUJACEM_df= AMBUJACEM
  colnames(NTPC_df)<-c("Date","Open","High","Low","Last","Close","TTQ","Turnover")
  colnames(AMBUJACEM_df)<-c("Date","Open","High","Low","Last","Close","TTQ","Turnover")
  
  NTPC_df$TTQ<-NTPC_df$TTQ/100000
  AMBUJACEM_df$TTQ<-AMBUJACEM_df$TTQ/100000
  
  ## Regression models
  
  m1=lm(NTPC_df$Close~NTPC_df$High+NTPC_df$Low+NTPC_df$TTQ)
  
  p1.df=as.data.frame(predict(m1,interval="predict"))
  
  m3=lm(AMBUJACEM_df$Close~AMBUJACEM_df$High+AMBUJACEM_df$Low+AMBUJACEM_df$TTQ)
  
  p3.df=as.data.frame(predict(m3, interval="predict"))
  
  
  
  
  ## Forecast using ARIMA to take out the seasonality and cyclic part of the stock
  
  m2=arima(diff(NTPC_df$Close),order=c(1,0,0))
  m4=arima(diff(AMBUJACEM_df$Close),order=c(1,0,0))
  p2.df=as.data.frame(predict(m2,n.ahead=3))
  p4.df=as.data.frame(predict(m4,n.ahead=3))
  
  ## Combining the Random and Stock  together
  
  p1.df=p1.df[1:3,]
  p1.df$fit=p1.df$fit+p2.df$pred
  p3.df=p3.df[1:3,]
  p3.df$fit=p3.df$fit+p4.df$pred
  
  ## Create the date df for next three months
  
  date<-as.data.frame(as.Date(c("2017-10-06","2017-11-06","2017-12-06")))
  colnames(date)=c("date")
  
  ## Modify the predict dataset and add "key" variable for NTPC
  
  p1.df<-cbind(p1.df,date)
  p1.df["Key"]<-"Predicted"
  p1.df<-p1.df[,c("date","fit","lwr","upr","Key")]
  
  ## Modify the predict dataset for Axis and add variable "Key"
  
  p3.df<-cbind(p3.df,date)
  p3.df["Key"]<-"Predicted"
  p3.df<-p3.df[,c("date","fit","lwr","upr","Key")]
  
  
  ## Rename the columns
  colnames(p1.df)<-c("Date","Close","lwr","upr","Key")
  colnames(p3.df)<-c("Date","Close","lwr","upr","Key")
  
  ## Modify the NTPC_df dataset
  
  NTPC_df<-NTPC%>%select("Date","Close")
  AMBUJACEM_df<-AMBUJACEM%>%select("Date","Close")
  
  ## Add two variable for confidence interval "lwr" and "upr"
  var<-c("lwr","upr")
  
  NTPC_df[var]<-NA
  AMBUJACEM_df[var]<-NA
  
  ## Add the Key variable for Actual data
  
  NTPC_df["Key"]<-"Actual"
  AMBUJACEM_df["Key"]<-"Actual"
  
  ## Rbind the predicted and actual value for both of the Stocks
  
  NTPC_com=rbind(NTPC_df,p1.df)
  NTPC_com$Date<-as.Date(NTPC_com$Date)
  
  AMBUJACEM_com=rbind(AMBUJACEM_df,p3.df)
  AMBUJACEM_com$Date<-as.Date(AMBUJACEM_com$Date)
  
  ## Visualisation
  
  NTPC_Plot<-ggplot(data=NTPC_com,aes(x= Date, y = Close,color=Key,label=Close)) +
    # Prediction intervals
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Key), 
                fill = "khaki2", size = 0)+
    geom_line(size = 1.7) + 
    geom_point(size = 2)+
    labs(title = "Actual and Predicted Price, NTPC", x = "Date",y="Price") +
    theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
          ,panel.background = element_rect(fill = "honeydew")
          ,panel.grid.minor = element_blank()
          ,panel.grid.major = element_blank()
          ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
          ,axis.title = element_text(size = 18, color = '#555555')
          ,axis.title.y = element_text(hjust=0.5,size=15)
          ,axis.title.x = element_text(hjust = 0.5,size=15))
  
  AMBUJACEM_Plot<- ggplot(data=AMBUJACEM_com,aes(x= Date, y = Close,color=Key,label=Close)) +
    # Prediction intervals
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Key), 
                fill = "khaki2", size = 0)+
    geom_line(size = 1.7) + 
    geom_point(size = 2)+
    labs(title = "Actual and Predicted Price, AMBUJACEM", x = "Date",y="Price") +
    theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
          ,panel.background = element_rect(fill = "honeydew")
          ,panel.grid.minor = element_blank()
          ,panel.grid.major = element_blank()
          ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
          ,axis.title = element_text(size = 18, color = '#555555')
          ,axis.title.y = element_text(hjust=0.5,size=15)
          ,axis.title.x = element_text(hjust = 0.5,size=15))
  
  ## Plots
  
  grid.arrange(NTPC_Plot,AMBUJACEM_Plot,ncol = 1, nrow = 2)