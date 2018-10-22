library(readxl)
library(data.table)
library(forecast)
library(TSPred)

##### PARA OBTENER UNA TIME SERIES DE OTRO DIAGNOSTICO DEBEN TENER LOS EGRESOS EN BRUTO DE TODOS LOS AÑOS en "datasets\\EgresosBruto"
#Con en bruto me refiero a tal y como vienen descargados de https://reportesdeis.minsal.cl/Egresos2001_2016/egresos_2003/egresos.asp
#Los egresos en bruto no los subo al github porque pesan mucho

#para analizar la time series de neumonia o aborto espontaneo simplemente lean los archivos:
#"datasets\\time series\\AbortoEspontaneoTS.csv"
#"datasets\\time series\\NeumoniaTS.csv"


#neumonia
#codDiagnostico <- c("J120","J121","J122" , "J128","J129","J13","J04" , "J150", "J151", "J152" ,"J153","J154", "J155" ,"J156", "J157","J158","J159","J160","J168","J170","J171","J172","J173","J178","J180","J181","J182","J188","J189")

#aborto espontaneo
codDiagnostico <- c("O030","O031","O032","O033","O034","O035","O036","O037","O038","O039") 

#Quintero
codigoEstab <- c(107108,107434)

#Quintero
codComuna <- 5107

#Puchuncavi
#codComuna <- 5105

source("scripts\\importar.R")

n17 <- egr2017[egr2017$DIAG1 %in% codDiagnostico & egr2017$COMUNA == codComuna,]
n16 <- egr2016[egr2016$DIAG1 %in% codDiagnostico & egr2016$COMUNA == codComuna,]
n15 <- egr2015[egr2015$DIAG1 %in% codDiagnostico & egr2015$COMUNA == codComuna,]
n14 <- egr2014[egr2014$DIAG1 %in% codDiagnostico & egr2014$COMUNA == codComuna,]
n13 <- egr2013[egr2013$DIAG1 %in% codDiagnostico & egr2013$COMUNA == codComuna,]
n12 <- egr2012[egr2012$DIAG1 %in% codDiagnostico & egr2012$COMUNA == codComuna,]
n11 <- egr2011[egr2011$DIAG1 %in% codDiagnostico & egr2011$COMUNA == codComuna,]
n10 <- egr2010[egr2010$DIAG1 %in% codDiagnostico & egr2010$COMUNA == paste("0",codComuna,sep=""),]
n9 <- egr2009[egr2009$DIAG1 %in% codDiagnostico & egr2009$COMUNA == codComuna,]
n8 <- egr2008[egr2008$DIAG1 %in% codDiagnostico & egr2008$COMUNA == codComuna,]
n7 <- egr2007[egr2007$DIAG1 %in% codDiagnostico & egr2007$COMUNA == codComuna,]
n6 <- egr2006[egr2006$DIAG1 %in% codDiagnostico & egr2006$COMUNA == codComuna,]
n5 <- egr2005[egr2005$DIAG1 %in% codDiagnostico & egr2005$COMUNA == codComuna,]
n4 <- egr2004[egr2004$DIAG1 %in% codDiagnostico & egr2004$COMUNA == codComuna,]
n3 <- egr2003[egr2003$DIAG1 %in% codDiagnostico & egr2003$COMUNA == codComuna,]
n2 <- egr2002[egr2002$DIAG1 %in% codDiagnostico & egr2002$COMUNA == codComuna,]
n1 <- egr2001[egr2001$DIAG1 %in% codDiagnostico & egr2001$COMUNA == codComuna,]


##### 2017 #########
Fecha17 <- n17$FECHA_EGR
Fecha17 <- data.table(Fecha17)

frec17 <- Fecha17[,.N,by=Fecha17]
frec17$Fecha17 <- as.Date(frec17$Fecha17)

colnames(frec17)[1] <- "Fecha"

dates17 <- seq.Date(
  as.Date("2017-01-01"),
  as.Date("2017-12-31"),
  "day")


df17 <- data.frame(dates17)

colnames(df17) <- "Fecha"

z17 <- merge(frec17,df17, by="Fecha", all=TRUE)
z17[is.na(z17)] <- 0


z17month <- z17 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))




##### 2016 #########
Fecha16 <- n16$FECHA_EGR
Fecha16 <- data.table(Fecha16)

frec16 <- Fecha16[,.N,by=Fecha16]
frec16$Fecha16 <- as.Date(frec16$Fecha16)

colnames(frec16)[1] <- "Fecha"

dates16 <- seq.Date(
  as.Date("2016-01-01"),
  as.Date("2016-12-31"),
  "day")


df16 <- data.frame(dates16)

colnames(df16) <- "Fecha"

z16 <- merge(frec16,df16, by="Fecha", all=TRUE)
z16[is.na(z16)] <- 0


z16month <- z16 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))


##### 2015 #########
Fecha15 <- n15$FECHA_EGR
Fecha15 <- data.table(Fecha15)

frec15 <- Fecha15[,.N,by=Fecha15]
frec15$Fecha15 <- as.Date(frec15$Fecha15)

colnames(frec15)[1] <- "Fecha"

dates15 <- seq.Date(
  as.Date("2015-01-01"),
  as.Date("2015-12-31"),
  "day")


df15 <- data.frame(dates15)

colnames(df15) <- "Fecha"

z15 <- merge(frec15,df15, by="Fecha", all=TRUE)
z15[is.na(z15)] <- 0


z15month <- z15 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))


##### 2014 #########
Fecha14 <- n14$FECHA_EGR
Fecha14 <- data.table(Fecha14)

frec14 <- Fecha14[,.N,by=Fecha14]
frec14$Fecha14 <- as.Date(frec14$Fecha14)

colnames(frec14)[1] <- "Fecha"

dates14 <- seq.Date(
  as.Date("2014-01-01"),
  as.Date("2014-12-31"),
  "day")


df14 <- data.frame(dates14)

colnames(df14) <- "Fecha"

z14 <- merge(frec14,df14, by="Fecha", all=TRUE)
z14[is.na(z14)] <- 0


z14month <- z14 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2013 #########
Fecha13 <- n13$FECHA_EGR
Fecha13 <- data.table(Fecha13)

frec13 <- Fecha13[,.N,by=Fecha13]
frec13$Fecha13 <- as.Date(frec13$Fecha13)

colnames(frec13)[1] <- "Fecha"

dates13 <- seq.Date(
  as.Date("2013-01-01"),
  as.Date("2013-12-31"),
  "day")


df13 <- data.frame(dates13)

colnames(df13) <- "Fecha"

z13 <- merge(frec13,df13, by="Fecha", all=TRUE)
z13[is.na(z13)] <- 0


z13month <- z13 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2012 #########
Fecha12 <- n12$FECHA_EGR
Fecha12 <- data.table(Fecha12)

frec12 <- Fecha12[,.N,by=Fecha12]
frec12$Fecha12 <- as.Date(frec12$Fecha12)

colnames(frec12)[1] <- "Fecha"

dates12 <- seq.Date(
  as.Date("2012-01-01"),
  as.Date("2012-12-31"),
  "day")


df12 <- data.frame(dates12)

colnames(df12) <- "Fecha"

z12 <- merge(frec12,df12, by="Fecha", all=TRUE)
z12[is.na(z12)] <- 0


z12month <- z12 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2011 #########
Fecha11 <- n11$FECHA_EGR
Fecha11 <- data.table(Fecha11)

frec11 <- Fecha11[,.N,by=Fecha11]
frec11$Fecha11 <- as.Date(frec11$Fecha11)

colnames(frec11)[1] <- "Fecha"

dates11 <- seq.Date(
  as.Date("2011-01-01"),
  as.Date("2011-12-31"),
  "day")


df11 <- data.frame(dates11)

colnames(df11) <- "Fecha"

z11 <- merge(frec11,df11, by="Fecha", all=TRUE)
z11[is.na(z11)] <- 0


z11month <- z11 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2010 #########
Fecha10 <- n10$FECHA_EGR
Fecha10 <- data.table(Fecha10)

frec10 <- Fecha10[,.N,by=Fecha10]
frec10$Fecha10 <- as.Date(frec10$Fecha10)

colnames(frec10)[1] <- "Fecha"

dates10 <- seq.Date(
  as.Date("2010-01-01"),
  as.Date("2010-12-31"),
  "day")


df10 <- data.frame(dates10)

colnames(df10) <- "Fecha"

z10 <- merge(frec10,df10, by="Fecha", all=TRUE)
z10[is.na(z10)] <- 0


z10month <- z10 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2009 #########
Fecha09 <- n9$FECHA_EGR
Fecha09 <- data.table(Fecha09)

frec09 <- Fecha09[,.N,by=Fecha09]
frec09$Fecha09 <- as.Date(frec09$Fecha09)

colnames(frec09)[1] <- "Fecha"

dates09 <- seq.Date(
  as.Date("2009-01-01"),
  as.Date("2009-12-31"),
  "day")


df09 <- data.frame(dates09)

colnames(df09) <- "Fecha"

z09 <- merge(frec09,df09, by="Fecha", all=TRUE)
z09[is.na(z09)] <- 0


z09month <- z09 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2008 #########
Fecha08 <- n8$FECHA_EGR
Fecha08 <- data.table(Fecha08)

frec08 <- Fecha08[,.N,by=Fecha08]
frec08$Fecha08 <- as.Date(frec08$Fecha08)

colnames(frec08)[1] <- "Fecha"

dates08 <- seq.Date(
  as.Date("2008-01-01"),
  as.Date("2008-12-31"),
  "day")


df08 <- data.frame(dates08)

colnames(df08) <- "Fecha"

z08 <- merge(frec08,df08, by="Fecha", all=TRUE)
z08[is.na(z08)] <- 0


z08month <- z08 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2007 #########
Fecha07 <- n7$FEC_EGR
Fecha07 <- data.table(Fecha07)

frec07 <- Fecha07[,.N,by=Fecha07]
frec07$Fecha07 <- as.Date(frec07$Fecha07, tryFormats = "%d-%m-%Y")

colnames(frec07)[1] <- "Fecha"

dates07 <- seq.Date(
  as.Date("2007-01-01"),
  as.Date("2007-12-31"),
  "day")


df07 <- data.frame(dates07)

colnames(df07) <- "Fecha"

z07 <- merge(frec07,df07, by="Fecha", all=TRUE)
z07[is.na(z07)] <- 0


z07month <- z07 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2006 #########
Fecha06 <- n6$FECHA_EGR
Fecha06 <- data.table(Fecha06)

frec06 <- Fecha06[,.N,by=Fecha06]
frec06$Fecha06 <- as.Date(frec06$Fecha06,tryFormats = "%d-%m-%Y")

colnames(frec06)[1] <- "Fecha"

dates06 <- seq.Date(
  as.Date("2006-01-01"),
  as.Date("2006-12-31"),
  "day")


df06 <- data.frame(dates06)

colnames(df06) <- "Fecha"

z06 <- merge(frec06,df06, by="Fecha", all=TRUE)
z06[is.na(z06)] <- 0


z06month <- z06 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2005 #########
Fecha05 <- n5$FECHA_EGR
Fecha05 <- data.table(Fecha05)

frec05 <- Fecha05[,.N,by=Fecha05]
frec05$Fecha05 <- as.Date(frec05$Fecha05)

colnames(frec05)[1] <- "Fecha"

dates05 <- seq.Date(
  as.Date("2005-01-01"),
  as.Date("2005-12-31"),
  "day")


df05 <- data.frame(dates05)

colnames(df05) <- "Fecha"

z05 <- merge(frec05,df05, by="Fecha", all=TRUE)
z05[is.na(z05)] <- 0


z05month <- z05 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2004 #########
Fecha04 <- n4$FECHA_EG
Fecha04 <- data.table(Fecha04)

frec04 <- Fecha04[,.N,by=Fecha04]
frec04$Fecha04 <- as.Date(frec04$Fecha04, tryFormats = "%d-%m-%Y")

colnames(frec04)[1] <- "Fecha"

dates04 <- seq.Date(
  as.Date("2004-01-01"),
  as.Date("2004-12-31"),
  "day")


df04 <- data.frame(dates04)

colnames(df04) <- "Fecha"

z04 <- merge(frec04,df04, by="Fecha", all=TRUE)
z04[is.na(z04)] <- 0


z04month <- z04 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2003 #########
Fecha03 <- n3$FECH_EGRE
Fecha03 <- data.table(Fecha03)

frec03 <- Fecha03[,.N,by=Fecha03]
frec03$Fecha03 <- as.Date(frec03$Fecha03)

colnames(frec03)[1] <- "Fecha"

dates03 <- seq.Date(
  as.Date("2003-01-01"),
  as.Date("2003-12-31"),
  "day")


df03 <- data.frame(dates03)

colnames(df03) <- "Fecha"

z03 <- merge(frec03,df03, by="Fecha", all=TRUE)
z03[is.na(z03)] <- 0


z03month <- z03 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2002 #########
Fecha02 <- n2$EGRESO
Fecha02 <- data.table(Fecha02)

frec02 <- Fecha02[,.N,by=Fecha02]
frec02$Fecha02 <- as.Date(frec02$Fecha02,tryFormats = "%d-%m-%Y")

colnames(frec02)[1] <- "Fecha"

dates02 <- seq.Date(
  as.Date("2002-01-01"),
  as.Date("2002-12-31"),
  "day")


df02 <- data.frame(dates02)

colnames(df02) <- "Fecha"

z02 <- merge(frec02,df02, by="Fecha", all=TRUE)
z02[is.na(z02)] <- 0


z02month <- z02 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

##### 2001 #########
Fecha01 <- n1$FECHA_EGRE
Fecha01 <- data.table(Fecha01)

frec01 <- Fecha01[,.N,by=Fecha01]
frec01$Fecha01 <- as.Date(frec01$Fecha01)

colnames(frec01)[1] <- "Fecha"

dates01 <- seq.Date(
  as.Date("2001-01-01"),
  as.Date("2001-12-31"),
  "day")


df01 <- data.frame(dates01)

colnames(df01) <- "Fecha"

z01 <- merge(frec01,df01, by="Fecha", all=TRUE)
z01[is.na(z01)] <- 0


z01month <- z01 %>% group_by(month=floor_date(Fecha, "month")) %>%
  summarize(amount=sum(N))

zmonthTotal <- rbind(z01month,z02month,z03month,z04month,z05month,z06month,z07month,z08month,z09month,z10month,z11month,z12month,z13month,z14month,z15month,z16month,z17month)

#write.csv(zmonthTotal, file ="datasets\\time series\\AbortoEspontaneoTS.csv") #para guardar la TS que hayan hecho

nTS <- ts(zmonthTotal$amount,start=c(2001,1), end=c(2017,1),frequency=12)

plot.ts(nTS)



### de aqui para abajo es codigo para modelar algun forecast

diff1 <- diff(nTS,differences=1)
plot.ts(diff1)

diff2 <- diff(nTS,differences=2)
plot.ts(diff2)

acf(diff1,lag.max=20)
pacf(diff1,lag.max=20)

## training con todos los datos

model <- auto.arima(nTS)
Box.test(model$residuals,lag=20,type="Ljung-Box")
model_forecast <- forecast(model,h=50)
model_forecast
plot(model_forecast)
accuracy(model)


##training con algunos de los datos, el resto se usa para testear

train <- window(
  nTS,
  end=c(2013,1))
test <- window(
  nTS,
  start=c(2013,2))
## fit a model on training data
aaFit <- auto.arima(
  train)
## forcast training model over
## the testing period
aaPred <- forecast(
  aaFit,
  h=length(test))
plot(aaPred)


accuracy(aaPred,test)


plotarimapred(nTS,aaFit,xlim=c(2001,2017))
