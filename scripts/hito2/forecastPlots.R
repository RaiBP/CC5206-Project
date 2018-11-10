
source("scripts\\hito2\\forecastLagCorrelation.R")
source("scripts\\hito2\\forecast.R")


#--Produces a data.frame with the Source Data+Training Data, Fitted Values+Forecast Values, forecast data Confidence Intervals
funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date <- seq(from=startTest,to=ymd("2017-12-01"), by = "month")
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}



#---Extract the Data for ggplot using funggcast()
pd<-funggcast(x1,fcast1) ## fcast1 viene de forecastLagCorrelation.R
#pd <- funggcast(x1,fcast2) ## fcast2 viene de forecast.R (para plotear modelo sin covariantes)

#---Plot in ggplot2 0.9
library(ggplot2)
library(scales)


p1a<-ggplot(data=pd,aes(x=date,y=observed)) 
p1a <- p1a+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=0.3,fill="grey20")
p1a<-p1a+geom_line(aes(colour="data"),size=2)
p1a<-p1a+geom_line(aes(y=fitted,colour="fit"),size=2)
p1a<-p1a+geom_point(aes(y=fitted),colour="#218B4F",size=4,shape=21,fill=NA,stroke=2)
p1a<-p1a+geom_line(aes(y=forecast,colour="prediction"),size=2)
p1a<-p1a+scale_x_date(name="Año",breaks='1 year',minor_breaks='1 month',labels=date_format("%Y"),expand=c(0,0))
p1a<-p1a+scale_y_continuous(name="Egresos hospitalarios asociados a diagnóstico de neumonía en Puchuncaví")
p1a<-p1a+ggtitle("Modelo basado en SO2, MP10, O3, HCT, HCNM y egresos previos, con estacionalidad")
p1a <- p1a + scale_colour_manual(name="",
                                 values=c(data="black", fit="#218B4F", prediction="#AFFFDD"))
p1a <- p1a+theme(axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),axis.text.x = element_text(angle=90,hjust=1,size=15), axis.text.y = element_text(size=15), plot.title = element_text(size = 25),legend.text=element_text(size=20),legend.position = c(0.77, 0.9),legend.key = element_rect(colour = "transparent", fill = alpha('grey20', 0.1)),legend.background = element_rect(fill=alpha('grey20', 0)))

p1a
