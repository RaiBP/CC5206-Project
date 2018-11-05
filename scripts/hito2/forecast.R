library(forecast)
library(TSA) 
library(imputeTS)
library(lubridate)
library(readxl)
library(data.table)
library(TSPred)
library(dplyr)

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

covariates=cbind(x2,x3,x4,x5,x6) ### agregar o quitar covariantes (x2, x3, x4, x5, x6, etc)

lag <- 1 ### aqui elegir el lag en meses entre el TS de egresos y covariantes 
# para obtener el lag con mayor correlacion entre covariantes, usar scatterplots al final

### fechas de inicio y final de los test y train data
startXregTrain <- ymd("2001-01-01") %m-% months(lag) ## fecha de incio para train data de covariantes (se restan los meses de lag) 
endTrain <- ymd("2015-01-01") ## fecha de término para train data e egresos
endXregTrain <- endTrain %m-% months(lag) ## fecha de término para train data de covariantes (se restan los meses de lag)
startTest <-  endTrain %m+% months(1) ## fecha de inicio para test data de egresos
startXregTest <-  endXregTrain %m+% months(1) ## fecha de inicio para test data de covariantes
endXregTest <- ymd("2017-12-01") %m-% months(lag)  ## fecha de término para test data de covariantes (se restan los meses de lag)

##con la configuración actual (lag=1), se entrenará el modelo con los covariantes desde 2000-12-01 hasta 2014-12-01, y los egresos desde 
##2001-01-01 hasta 2015-01-01. Se testeará el modelo con los covariantes desde 2015-01-01 hasta 2017-11-01, y los egresos desde 
##2015-02-01 hasta 2017-12-01

## los egresos solo pueden ir entre 2001-01-01 y 2017-12-01. Para los covariantes, depende de cada uno.

##########MODELO CON COVARIANTES #########
## se definen los conjuntos train/test
train1 <- window(
  x1,
  end=as.numeric(c(format(as.Date(endTrain, format="%Y-%m-%d"),"%Y"),format(as.Date(endTrain, format="%Y-%m-%d"),"%m"))))
train2 <- window(
  covariates,start=as.numeric(c(format(as.Date(startXregTrain, format="%Y-%m-%d"),"%Y"),format(as.Date(startXregTrain, format="%Y-%m-%d"),"%m"))),
  end=as.numeric(c(format(as.Date(endXregTrain, format="%Y-%m-%d"),"%Y"),format(as.Date(endXregTrain, format="%Y-%m-%d"),"%m"))))

test1 <- window(
  x1,
  start=as.numeric(c(format(as.Date(startTest, format="%Y-%m-%d"),"%Y"),format(as.Date(startTest, format="%Y-%m-%d"),"%m"))))
test2 <- window(
  covariates,
  start=as.numeric(c(format(as.Date(startXregTest, format="%Y-%m-%d"),"%Y"),format(as.Date(startXregTest, format="%Y-%m-%d"),"%m"))),end=as.numeric(c(format(as.Date(endXregTest, format="%Y-%m-%d"),"%Y"),format(as.Date(endXregTest, format="%Y-%m-%d"),"%m"))))

## modelo
fit1 <- auto.arima(train1, xreg = train2, D=1) ### D=1 activa la estacionalidad
#fit1 <- auto.arima(train1, xreg = train2) ### sin estacionalidad

fcast1 <- forecast(fit1, xreg = test2)

plot(fcast1)
accuracy(fcast1,test1)

## plotea predicción y datos a la vez
plotarimapred(x1,fit1,xreg=test2,xlim=c(2001,2017))

checkresiduals(fit1)

######## MODELO SIN COVARIANTES ########
train <- window(
  x1,
  end=c(2015,1))
test <- window(
  x1,
  start=c(2015,2))

fit2 <- auto.arima(train,D=1)

fcast2 <- forecast(fit2,h=length(test))

plot(fcast2)
accuracy(fcast2,test)

plotarimapred(x1,fit2,xlim=c(2001,2017))


####### SCATTERPLOTS ########
library(astsa)
lag2.plot (x7,x1,24) ##(variable 1, variable2, número de lags a plotear)