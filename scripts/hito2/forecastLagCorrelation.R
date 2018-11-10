library(forecast)
library(TSA) 
library(imputeTS)
library(lubridate)
library(readxl)
library(data.table)
library(TSPred)
library(dplyr)
library(ggplot2)

#source("scripts\\hito2\\joinTS.R")
source("scripts\\hito2\\obtenerContaminacionMensual.R")

########## SERIES DE TIEMPO EGRESOS #########

## todo lo que tenga P es de Puchuncavi, todo lo que tenga Q es de Quintero
tsAbortoP <- read.csv("datasets\\time series\\PAbortoEspontaneoTS.csv")
tsNeumoniaQ <- read.csv("datasets\\time series\\QNeumoniaTS.csv")
tsNeumoniaP <- read.csv("datasets\\time series\\PNeumoniaTS.csv")
tsACVPQ <- read.csv("datasets\\time series\\QACVTS.csv")
tsTotalP <- read.csv("datasets\\time series\\PegresosTotalesTS.csv") ## serie de tiempo de la cantidad de egresos totales mensuales

########## SERIES DE TIEMPO CLIMA ##########

noaa <- read.csv("datasets\\clima\\noaa.csv") ## datos climáticos estación Stgo de Chile, NOAA
precipitacionTS <- ts(noaa$TPCP, start=c(1993,01), end=c(2016,3),frequency=12) ## precipitacion Stgo de Chile
precipitacionTS[precipitacionTS==-9999] <- NA

########### PRELIMINARES ##########


## asignacion de variables, na.kalman rellena los datos NA
x7 <- na.kalman(precipitacionTS) 
x6 <- na.kalman(tsPHCNM)
x5 <- na.kalman(tsPHCT)
x4 <- na.kalman(tsPOzono)
x3 <- na.kalman(tsPMP10)
x2 <- na.kalman(tsPSO2)
x1 <- ts(tsNeumoniaP$amount,start=c(2001,1), end=c(2017,12),frequency=12) ##aqui se puede cambiar el TS de egreso por el que se desee

 ### agregar o quitar covariantes (x2, x3, x4, x5, x6, etc)

lag <-4 ### aqui elegir el lag en meses entre el TS de egresos y covariantes 
# para obtener el lag con mayor correlacion entre covariantes, usar scatterplots al final

corr <- read.csv("datasets\\correlacion\\Jan2007toJan2009corrNeumoniaPuch.csv")


#### MAXIMA CORRELACION ######
corrtempx2 <- corr[order(corr$SO2,decreasing=T),]
lagx2 <- corrtempx2[corrtempx2$Lag < 97,]$Lag[1]

corrtempx3 <- corr[order(corr$MP10,decreasing=T),]
lagx3 <- corrtempx3[corrtempx3$Lag < 97,]$Lag[1]

corrtempx4 <- corr[order(corr$O3,decreasing=T),]
lagx4 <- corrtempx4[corrtempx4$Lag < 97,]$Lag[1]

corrtempx5 <- corr[order(corr$HCT,decreasing=T),]
lagx5 <- corrtempx5[corrtempx5$Lag < 97,]$Lag[1]

corrtempx6 <- corr[order(corr$HCNM,decreasing=T),]
lagx6 <- corrtempx6[corrtempx6$Lag < 97,]$Lag[1]

#### MINIMA CORRELACION ######

# corrtempx2 <- corr[order(corr$SO2),]
# lagx2 <- corrtempx2[corrtempx2$SO2 > 0 & corrtempx2$Lag < 97,]$Lag[1]
# 
# corrtempx3 <- corr[order(corr$MP10),]
# lagx3 <- corrtempx3[corrtempx3$MP10 > 0 & corrtempx3$Lag < 97,]$Lag[1]
# 
# corrtempx4 <- corr[order(corr$O3),]
# lagx4 <- corrtempx4[corrtempx4$O3 > 0 & corrtempx4$Lag < 97,]$Lag[1]
# 
# corrtempx5 <- corr[order(corr$HCT),]
# lagx5 <- corrtempx5[corrtempx5$HCT > 0 & corrtempx5$Lag < 97,]$Lag[1]
# 
# corrtempx6 <- corr[order(corr$HCNM),]
# lagx6 <- corrtempx6[corrtempx6$HCNM > 0 & corrtempx6$Lag < 97,]$Lag[1]




#### fechas de inicio y final de los test y train data ####
endTrain <- ymd("2016-12-01") ## fecha de término para train data e egresos
startTest <-  endTrain %m+% months(1) ## fecha de inicio para test data de egresos


startx3Train <- ymd("2001-01-01") %m-% months(lagx3) ## fecha de incio para train data de covariantes (se restan los meses de lag) 
endx3Train <- endTrain %m-% months(lagx3) ## fecha de término para train data de covariantes (se restan los meses de lag)
startx3Test <-  endx3Train %m+% months(1) ## fecha de inicio para test data de covariantes
endx3Test <- ymd("2017-12-01") %m-% months(lagx3)

startx2Train <- ymd("2001-01-01") %m-% months(lagx2) ## fecha de incio para train data de covariantes (se restan los meses de lag) 
endx2Train <- endTrain %m-% months(lagx2) ## fecha de término para train data de covariantes (se restan los meses de lag)
startx2Test <-  endx2Train %m+% months(1) ## fecha de inicio para test data de covariantes
endx2Test <- ymd("2017-12-01") %m-% months(lagx2)

startx4Train <- ymd("2001-01-01") %m-% months(lagx4) ## fecha de incio para train data de covariantes (se restan los meses de lag) 
endx4Train <- endTrain %m-% months(lagx4) ## fecha de término para train data de covariantes (se restan los meses de lag)
startx4Test <-  endx4Train %m+% months(1) ## fecha de inicio para test data de covariantes
endx4Test <- ymd("2017-12-01") %m-% months(lagx4)

startx5Train <- ymd("2001-01-01") %m-% months(lagx5) ## fecha de incio para train data de covariantes (se restan los meses de lag) 
endx5Train <- endTrain %m-% months(lagx5) ## fecha de término para train data de covariantes (se restan los meses de lag)
startx5Test <-  endx5Train %m+% months(1) ## fecha de inicio para test data de covariantes
endx5Test <- ymd("2017-12-01") %m-% months(lagx5)

startx6Train <- ymd("2001-01-01") %m-% months(lagx6) ## fecha de incio para train data de covariantes (se restan los meses de lag) 
endx6Train <- endTrain %m-% months(lagx6) ## fecha de término para train data de covariantes (se restan los meses de lag)
startx6Test <-  endx6Train %m+% months(1) ## fecha de inicio para test data de covariantes
endx6Test <- ymd("2017-12-01") %m-% months(lagx6)




trainx3 <- window(
  x3,start=as.numeric(c(format(as.Date(startx3Train, format="%Y-%m-%d"),"%Y"),format(as.Date(startx3Train, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx3Train, format="%Y-%m-%d"),"%Y"),format(as.Date(endx3Train, format="%Y-%m-%d"),"%m"))))

trainx2 <- window(
  x2,start=as.numeric(c(format(as.Date(startx2Train, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2Train, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx2Train, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2Train, format="%Y-%m-%d"),"%m"))))

trainx4 <- window(
  x4,start=as.numeric(c(format(as.Date(startx4Train, format="%Y-%m-%d"),"%Y"),format(as.Date(startx4Train, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx4Train, format="%Y-%m-%d"),"%Y"),format(as.Date(endx4Train, format="%Y-%m-%d"),"%m"))))

trainx5 <- window(
  x5,start=as.numeric(c(format(as.Date(startx5Train, format="%Y-%m-%d"),"%Y"),format(as.Date(startx5Train, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx5Train, format="%Y-%m-%d"),"%Y"),format(as.Date(endx5Train, format="%Y-%m-%d"),"%m"))))

trainx6 <- window(
  x6,start=as.numeric(c(format(as.Date(startx6Train, format="%Y-%m-%d"),"%Y"),format(as.Date(startx6Train, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endx6Train, format="%Y-%m-%d"),"%Y"),format(as.Date(endx6Train, format="%Y-%m-%d"),"%m"))))

train2 <- cbind(unclass(trainx3),unclass(trainx2),unclass(trainx4),unclass(trainx5),unclass(trainx6))

testx2 <- window(
  x2,
  start=as.numeric(c(format(as.Date(startx2Test, format="%Y-%m-%d"),"%Y"),format(as.Date(startx2Test, format="%Y-%m-%d"),"%m"))),end=as.numeric(c(format(as.Date(endx2Test, format="%Y-%m-%d"),"%Y"),format(as.Date(endx2Test, format="%Y-%m-%d"),"%m"))))

testx3 <- window(
  x3,
  start=as.numeric(c(format(as.Date(startx3Test, format="%Y-%m-%d"),"%Y"),format(as.Date(startx3Test, format="%Y-%m-%d"),"%m"))),end=as.numeric(c(format(as.Date(endx3Test, format="%Y-%m-%d"),"%Y"),format(as.Date(endx3Test, format="%Y-%m-%d"),"%m"))))

testx4 <- window(
  x4,
  start=as.numeric(c(format(as.Date(startx4Test, format="%Y-%m-%d"),"%Y"),format(as.Date(startx4Test, format="%Y-%m-%d"),"%m"))),end=as.numeric(c(format(as.Date(endx4Test, format="%Y-%m-%d"),"%Y"),format(as.Date(endx4Test, format="%Y-%m-%d"),"%m"))))

testx5 <- window(
  x5,
  start=as.numeric(c(format(as.Date(startx5Test, format="%Y-%m-%d"),"%Y"),format(as.Date(startx5Test, format="%Y-%m-%d"),"%m"))),end=as.numeric(c(format(as.Date(endx5Test, format="%Y-%m-%d"),"%Y"),format(as.Date(endx5Test, format="%Y-%m-%d"),"%m"))))

testx6 <- window(
  x6,
  start=as.numeric(c(format(as.Date(startx6Test, format="%Y-%m-%d"),"%Y"),format(as.Date(startx6Test, format="%Y-%m-%d"),"%m"))),end=as.numeric(c(format(as.Date(endx6Test, format="%Y-%m-%d"),"%Y"),format(as.Date(endx6Test, format="%Y-%m-%d"),"%m"))))

test2 <- cbind(unclass(testx3),unclass(testx2),unclass(testx4),unclass(testx5),unclass(testx6))

train1 <- window(
  x1,
  end=as.numeric(c(format(as.Date(endTrain, format="%Y-%m-%d"),"%Y"),format(as.Date(endTrain, format="%Y-%m-%d"),"%m"))))

test1 <- window(
  x1,
  start=as.numeric(c(format(as.Date(startTest, format="%Y-%m-%d"),"%Y"),format(as.Date(startTest, format="%Y-%m-%d"),"%m"))))

## modelo
fit1 <- auto.arima(train1, xreg = train2,D=1) ### D=1 activa la estacionalidad
#fit1 <- auto.arima(train1, xreg = train2) ### sin estacionalidad

fcast1 <- forecast(fit1, xreg = test2)

plot(fcast1)
accuracy(fcast1,test1)

#write.csv(as.data.frame(accuracy(fcast1,test1)),file="plots\\hito2\\PuchNeumoniaLagsMinCorrConEstacContaminantesAll.csv")

plot(fit1$x,col="red")
lines(fitted(fit1),col="blue")

## plotea predicción y datos a la vez
plotarimapred(x1,fit1,xreg=test2,xlim=c(2001,2017))