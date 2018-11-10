
library(ggplot2)
library(corrr)
library(reshape2)
library(timeDate)

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}



conpartos <- read.csv("datasets\\time series\\PegresosTotalesTS.csv")
sinpartos <- read.csv("datasets\\time series\\PTotalSinPartosTS.csv")
tsAbortoP <- read.csv("datasets\\time series\\PAbortoEspontaneoTS.csv")
tsNeumoniaQ <- read.csv("datasets\\time series\\QNeumoniaTS.csv")
tsNeumoniaP <- read.csv("datasets\\time series\\PNeumoniaTS.csv")
tsACVQ <- read.csv("datasets\\time series\\QACVTS.csv")
tsACVP <- read.csv("datasets\\time series\\PACVTS.csv")

x1 <- ts(conpartos$amount,start=c(2001,1), end=c(2017,12),frequency=12) ##´elegir TS aca

## seleccionar ventana de tiempo de interés
startx1 <- ymd("2010-01-01")
endx1 <- ymd("2013-01-01")

x1window <- window(x1,
start=as.numeric(c(format(as.Date(startx1, format="%Y-%m-%d"),"%Y"),format(as.Date(startx1, format="%Y-%m-%d"),"%m"))),
end=as.numeric(c(format(as.Date(endx1, format="%Y-%m-%d"),"%Y"),format(as.Date(endx1, format="%Y-%m-%d"),"%m"))))

output <- as.data.frame(cbind(0,0,0,0,0,0))

colnames(output) <- c("Lag","SO2","MP10","O3","HCT","HCNM")

endlag <- elapsed_months(startx1,"1993-01-01")

for (lag in 0:endlag){

  startx2 <- startx1 %m-% months(lag) 
  endx2 <- endx1 %m-% months(lag) 
  
  x2window <- window(
  x2,
  start=as.numeric(c(format(as.Date(startx2, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx2, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2, format="%Y-%m-%d"),"%m"))))
  
  
  x3window <- window(
  x3,
  start=as.numeric(c(format(as.Date(startx2, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx2, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2, format="%Y-%m-%d"),"%m"))))
  
  x4window <- window(
  x4,
  start=as.numeric(c(format(as.Date(startx2, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx2, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2, format="%Y-%m-%d"),"%m"))))
  
  x5window <- window(
  x5,
  start=as.numeric(c(format(as.Date(startx2, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx2, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2, format="%Y-%m-%d"),"%m"))))
  
  x6window <- window(
  x6,
  start=as.numeric(c(format(as.Date(startx2, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx2, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2, format="%Y-%m-%d"),"%m"))))
  
  
  d1 <- data.frame(x1=as.matrix(x1window))
  d2 <- data.frame(x2=as.matrix(x2window))
  d3 <- data.frame(x3=as.matrix(x3window))
  d4 <- data.frame(x4=as.matrix(x4window))
  d5 <- data.frame(x5=as.matrix(x5window))
  d6 <- data.frame(x6=as.matrix(x6window))
  
  d <- cbind(d1,d2,d3,d4,d5,d6)
  
  corr <- correlate(d)[1,3:7]
  
  row <-  as.data.frame(cbind(lag,corr))
  
  colnames(row) <- c("Lag","SO2","MP10","O3","HCT","HCNM")
  
  output <- rbind(output,row)
  
  print(lag)
  
}

output <- output[-1,]

#write.csv(output, file="datasets\\correlacion\\Jan2007toJan2009corrNeumoniaPuch.csv") #guardar archivo indicando la ventana de tiempo usada

outputlong <- melt(output[,c(1,2,3,4,5,6)], id="Lag") ## aqui quitar o meter parametros en el vector c(1,...,6) para plotear

colnames(outputlong)[2] <- "Factor"

ggplot(data=outputlong,
       aes(x=Lag, y=value,colour=Factor)) +
  geom_line(size=1)+
  ggtitle("Correlación vs Lag, ventana 2010/01-2011/12") + 
  xlab("Lag") + ylab("Correlación")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))