################### QUINTERO SO2 ########################################

QSO2estacionsur <- read.csv("datasets\\contaminacion\\quin\\mensual\\esurMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
#QMP10estacionsur <- read.csv("datasets\\contaminacion\\quin\\estacionsurMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QSO2loncura <- read.csv("datasets\\contaminacion\\quin\\mensual\\loncuraMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
#QMP10loncura <- read.csv("datasets\\contaminacion\\quin\\loncuraMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QSO2vallealegre <- read.csv("datasets\\contaminacion\\quin\\mensual\\vallealegreMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
#QMP10villaalegre <- read.csv("datasets\\contaminacion\\quin\\villaalegreMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QSO2centro<- read.csv("datasets\\contaminacion\\quin\\mensual\\centroMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")

QSO2estacionquintero<- read.csv("datasets\\contaminacion\\quin\\mensual\\equinteroMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")



data0 <- QSO2estacionsur[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("1993/02/01"), as.Date("2018/10/01"), by = "month")
data0$estacionSur <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- QSO2loncura[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("2012/02/01"), as.Date("2018/10/01"), by = "month")
data1$loncura <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]

data2 <- QSO2vallealegre[,c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")
data2$Fecha <- seq(as.Date("1993/01/01"), as.Date("2018/10/01"), by = "month")
data2$villaAlegre <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data2 <- data2[,c(1,5)]

data3 <- QSO2centro[,c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")
data3$Fecha <- seq(as.Date("2012/07/01"), as.Date("2018/10/01"), by = "month")
data3$centro <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data3 <- data3[,c(1,5)]

data4 <- QSO2estacionquintero[,c(1,3:5)]
colnames(data4) <- c("Fecha","Validado","Preliminar","NoValidado")
data4$Fecha <- seq(as.Date("2008/05/01"), as.Date("2018/10/01"), by = "month")
data4$estacionQuintero <- rowMeans(data4[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data4 <- data4[,c(1,5)]


QSO2 <- merge(data4,merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T),by="Fecha",all=T)

QSO2$mean <- rowMeans(QSO2[, 2:6], na.rm=TRUE)

tsQSO2 <- ts(QSO2$mean, start=c(1993,1), end=c(2018,10),frequency=12)

################### PUCHUNCAVI SO2 ########################################


PSO2estacionpuch <- read.csv("datasets\\contaminacion\\puch\\mensual\\epuchuncaviMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10estacionpuch <- read.csv("datasets\\contaminacion\\puch\\mensual\\epuchuncaviMensualMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2lagreda <- read.csv("datasets\\contaminacion\\puch\\mensual\\lagredaMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10lagreda <- read.csv("datasets\\contaminacion\\puch\\mensual\\lagredaMensualMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2maitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10maitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2ventanas<- read.csv("datasets\\contaminacion\\puch\\mensual\\ventanasMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10ventanas<- read.csv("datasets\\contaminacion\\puch\\mensual\\ventanasMensualMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2campiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10campiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")



data0 <- PSO2estacionpuch[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("1993/01/01"), as.Date("2018/10/01"), by = "month")
data0$estPuchuncavi <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- PSO2lagreda[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("1993/01/01"), as.Date("2018/10/01"), by = "month")
data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]

data2 <- PSO2maitenes[,c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")
data2$Fecha <- seq(as.Date("1993/04/01"), as.Date("2018/10/01"), by = "month")
data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data2 <- data2[,c(1,5)]

data3 <- PSO2ventanas[,c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")
data3$Fecha <- seq(as.Date("2013/01/01"), as.Date("2018/10/01"), by = "month")
data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data3 <- data3[,c(1,5)]

data4 <- PSO2campiche[,c(1,3:5)]
colnames(data4) <- c("Fecha","Validado","Preliminar","NoValidado")
data4$Fecha <- seq(as.Date("2013/01/01"), as.Date("2017/03/01"), by = "month")
data4$campiche <- rowMeans(data4[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data4 <- data4[,c(1,5)]


PSO2 <- merge(data4,merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T),by="Fecha",all=T)

PSO2$mean <- rowMeans(PSO2[, 2:6], na.rm=TRUE)

tsPSO2 <- ts(PSO2$mean, start=c(1993,1), end=c(2018,10),frequency=12)



################### PUCHUNCAVI MP10 ########################################


data0 <- PMP10estacionpuch[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("1993/02/01"), as.Date("2016/11/01"), by = "month")
data0$estPuchuncavi <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- PMP10lagreda[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("1993/01/01"), as.Date("2018/10/01"), by = "month")
data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]

data2 <- PMP10maitenes[,c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")
data2$Fecha <- seq(as.Date("1994/02/01"), as.Date("2018/10/01"), by = "month")
data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data2 <- data2[,c(1,5)]

data3 <- PMP10ventanas[,c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")
data3$Fecha <- seq(as.Date("2013/01/01"), as.Date("2018/10/01"), by = "month")
data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data3 <- data3[,c(1,5)]

data4 <- PMP10campiche[,c(1,3:5)]
colnames(data4) <- c("Fecha","Validado","Preliminar","NoValidado")
data4 <- data4[62:104,]
data4$Fecha <- seq(as.Date("2005/06/01"), as.Date("2008/12/01"), by = "month")
data4$campiche <- rowMeans(data4[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data4 <- data4[,c(1,5)]


PMP10 <- merge(data4,merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T),by="Fecha",all=T)

PMP10$mean <- rowMeans(PMP10[, 2:6], na.rm=TRUE)

tsPMP10 <- ts(PMP10$mean, start=c(1993,1), end=c(2018,10),frequency=12)

plot.ts(ts(PMP10))

################### PUCUNCAVI NO ########################################


PNOestacionpuch <- read.csv("datasets\\contaminacion\\puch\\mensual\\epuchuncaviMensualNO.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNO2estacionpuch <- read.csv("datasets\\contaminacion\\puch\\mensual\\epuchuncaviMensualNO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNOlagreda <- read.csv("datasets\\contaminacion\\puch\\mensual\\lagredaMensualNO.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNO2lagreda <- read.csv("datasets\\contaminacion\\puch\\mensual\\lagredaMensualNO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNOmaitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualNO.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNO2maitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualNO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNOventanas<- read.csv("datasets\\contaminacion\\puch\\mensual\\ventanasMensualNO.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNO2ventanas<- read.csv("datasets\\contaminacion\\puch\\mensual\\ventanasMensualNO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNOcampiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualNO.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PNO2campiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualNO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")



data0 <- PNOestacionpuch[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data0$estPuchuncavi <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- PNOlagreda[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]

data2 <- PNOmaitenes[,c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")
data2$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data2 <- data2[,c(1,5)]

data3 <- PNOventanas[,c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")
data3$Fecha <- seq(as.Date("2013/05/01"), as.Date("2018/10/01"), by = "month")
data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data3 <- data3[,c(1,5)]

data4 <- PNOcampiche[,c(1,3:5)]
colnames(data4) <- c("Fecha","Validado","Preliminar","NoValidado")
data4$Fecha <- seq(as.Date("2000/06/01"), as.Date("2017/03/01"), by = "month")
data4$campiche <- rowMeans(data4[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data4 <- data4[,c(1,5)]


PNO <- merge(data4,merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T),by="Fecha",all=T)

PNO$mean <- rowMeans(PNO[, 2:6], na.rm=TRUE)

tsPNO <- ts(PNO$mean, start=c(1993,1), end=c(2018,10),frequency=12)

plot.ts(ts(PNO))

################### PUCHUNCAVI NO2 ########################################


data0 <- PNO2estacionpuch[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data0$estPuchuncavi <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- PNO2lagreda[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]

data2 <- PNO2maitenes[,c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")
data2$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data2 <- data2[,c(1,5)]

data3 <- PNO2ventanas[,c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")
data3$Fecha <- seq(as.Date("2013/01/01"), as.Date("2018/10/01"), by = "month")
data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data3 <- data3[,c(1,5)]

data4 <- PNO2campiche[,c(1,3:5)]
colnames(data4) <- c("Fecha","Validado","Preliminar","NoValidado")
data4$Fecha <- seq(as.Date("2000/06/01"), as.Date("2017/03/01"), by = "month")
data4$campiche <- rowMeans(data4[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data4 <- data4[,c(1,5)]


PNO2 <- merge(data4,merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T),by="Fecha",all=T)

PNO2$mean <- rowMeans(PNO2[, 2:6], na.rm=TRUE)

tsPNO2 <- ts(PNO2$mean, start=c(1993,1), end=c(2018,10),frequency=12)

plot.ts(ts(PNO2))

################### PUCUNCAVI HCT ########################################


POzonoestacionpuch <- read.csv("datasets\\contaminacion\\puch\\mensual\\epuchuncaviMensualOzono.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
POzonolagreda <- read.csv("datasets\\contaminacion\\puch\\mensual\\lagredaMensualOzono.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PHCTmaitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualHCT.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
POzonomaitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualOzono.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
POzonoventanas<- read.csv("datasets\\contaminacion\\puch\\mensual\\ventanasMensualOzono.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PHCTcampiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualHCT.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
POzonocampiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualOzono.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")



data0 <- PHCTmaitenes[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("2010/01/01"), as.Date("2018/10/01"), by = "month")
data0$maitenes <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- PHCTcampiche[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("2000/06/01"), as.Date("2017/03/01"), by = "month")
data1$campiche <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]


PHCT <- merge(data0,data1,by="Fecha",all=T)

PHCT$mean <- rowMeans(PHCT[, 2:3], na.rm=TRUE)

tsPHCT <- ts(PHCT$mean, start=c(1993,1), end=c(2018,10),frequency=12)

plot.ts(ts(PHCT))

################### PUCHUNCAVI Ozono ########################################


data0 <- POzonoestacionpuch[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("2010/02/01"), as.Date("2018/10/01"), by = "month")
data0$estPuchuncavi <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- POzonolagreda[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("2010/02/01"), as.Date("2018/10/01"), by = "month")
data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]

data2 <- POzonomaitenes[,c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")
data2$Fecha <- seq(as.Date("2009/09/01"), as.Date("2018/10/01"), by = "month")
data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data2 <- data2[,c(1,5)]

data3 <- POzonoventanas[,c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")
data3$Fecha <- seq(as.Date("2013/01/01"), as.Date("2018/10/01"), by = "month")
data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data3 <- data3[,c(1,5)]

data4 <- POzonocampiche[,c(1,3:5)]
colnames(data4) <- c("Fecha","Validado","Preliminar","NoValidado")
data4$Fecha <- seq(as.Date("2000/06/01"), as.Date("2017/03/01"), by = "month")
data4$campiche <- rowMeans(data4[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data4 <- data4[,c(1,5)]


POzono <- merge(data4,merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T),by="Fecha",all=T)

POzono$mean <- rowMeans(POzono[, 2:6], na.rm=TRUE)

tsPOzono <- ts(POzono$mean, start=c(1993,1), end=c(2018,10),frequency=12)

plot.ts(ts(POzono))

################### PUCUNCAVI HCNM ########################################


PHCNMmaitenes <- read.csv("datasets\\contaminacion\\puch\\mensual\\losmaitenesMensualHCNM.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PHCNMcampiche<- read.csv("datasets\\contaminacion\\puch\\mensual\\campicheMensualHCNM.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")


data0 <- PHCNMmaitenes[,c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$Fecha <- seq(as.Date("2010/04/01"), as.Date("2018/10/01"), by = "month")
data0$maitenes <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data0 <- data0[,c(1,5)]

data1 <- PHCNMcampiche[,c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")
data1$Fecha <- seq(as.Date("2000/06/01"), as.Date("2014/07/01"), by = "month")
data1$campiche <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)
data1 <- data1[,c(1,5)]


PHCNM <- merge(data0,data1,by="Fecha",all=T)

PHCNM$mean <- rowMeans(PHCNM[, 2:3], na.rm=TRUE)

tsPHCNM <- ts(PHCNM$mean, start=c(1993,1), end=c(2018,10),frequency=12)

plot.ts(ts(PHCNM))