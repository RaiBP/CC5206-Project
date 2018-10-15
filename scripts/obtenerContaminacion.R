QSO2estacionsur <- read.csv("datasets\\contaminacion\\quin\\estacionsurSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QMP10estacionsur <- read.csv("datasets\\contaminacion\\quin\\estacionsurMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QSO2loncura <- read.csv("datasets\\contaminacion\\quin\\loncuraSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QMP10loncura <- read.csv("datasets\\contaminacion\\quin\\loncuraMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QSO2villaalegre <- read.csv("datasets\\contaminacion\\quin\\villaalegreSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QMP10villaalegre <- read.csv("datasets\\contaminacion\\quin\\villaalegreMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
QSO2centro<- read.csv("datasets\\contaminacion\\quin\\centroSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")

PSO2estacionpuch <- read.csv("datasets\\contaminacion\\puch\\estacionpuchSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10estacionpuch <- read.csv("datasets\\contaminacion\\puch\\estacionpuchMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2lagreda <- read.csv("datasets\\contaminacion\\puch\\lagredaSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10lagreda <- read.csv("datasets\\contaminacion\\puch\\lagredaMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2maitenes <- read.csv("datasets\\contaminacion\\puch\\maitenesSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10maitenes <- read.csv("datasets\\contaminacion\\puch\\maitenesMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PSO2ventanas<- read.csv("datasets\\contaminacion\\puch\\ventanasSO2.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")
PMP10ventanas<- read.csv("datasets\\contaminacion\\puch\\ventanasMP10.csv", header=TRUE,sep=";", na.strings=c("",NA),dec=",")



################### QUINTERO SO2 ########################################

data0 <- QSO2estacionsur[c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$estacionSur <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data0$Fecha)){
  data0[i,1] <- 1993 + i
}

data0 <- data0[,c(1,5)]

data1 <- QSO2loncura[c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")

data1$loncura <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data1$Fecha)){
  data1[i,1] <- 2012 + i
}

data1 <- data1[,c(1,5)]

data2 <- QSO2villaalegre[c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")

data2$villaAlegre <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data2$Fecha)){
  data2[i,1] <- 1992 + i
}

data2 <- data2[,c(1,5)]

data3 <- QSO2centro[c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")

data3$centro <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data3$Fecha)){
  data3[i,1] <- 2012 + i
}

data3 <- data3[,c(1,5)]


QSO2 <- merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T)

QSO2$mean <- rowMeans(QSO2[, 2:5], na.rm=TRUE)

library("matrixStats")

QSO2$std <- rowSds(as.matrix(QSO2[, 2:5]), na.rm=TRUE)

ggplot(data=QSO2, aes(x=Fecha, y=mean, group=1))+
  geom_line(linetype = "dashed")+
  geom_point()

  #geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.1)


################### QUINTERO MP10 ########################################

data0 <- QMP10estacionsur[c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$estacionSur <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data0$Fecha)){
  data0[i,1] <- 1994 + i
}

data0 <- data0[,c(1,5)]

data1 <- QMP10loncura[c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")

data1$loncura <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data1$Fecha)){
  data1[i,1] <- 2011 + i
}

data1 <- data1[,c(1,5)]

data2 <- QMP10villaalegre[c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")

data2$villaAlegre <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data2$Fecha)){
  data2[i,1] <- 1992 + i
}

data2 <- data2[,c(1,5)]


QMP10 <- merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T)

QMP10$mean <- rowMeans(QMP10[, 2:4], na.rm=TRUE)


QMP10$std <- rowSds(as.matrix(QMP10[, 2:4]), na.rm=TRUE)

ggplot(data=QMP10, aes(x=Fecha, y=mean, group=1)) + 
  geom_line(linetype = "dashed") + 
  geom_point() 

  #geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.1)

################### PUCHUNCAVÍ SO2 ########################################

data0 <- PSO2estacionpuch[c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$estacionPuch <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data0$Fecha)){
  data0[i,1] <- 1992 + i
}

data0 <- data0[,c(1,5)]

data1 <- PSO2lagreda[c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")

data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data1$Fecha)){
  data1[i,1] <- 1992 + i
}

data1 <- data1[,c(1,5)]

data2 <- PSO2maitenes[c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")

data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data2$Fecha)){
  data2[i,1] <- 1993 + i
}

data2 <- data2[,c(1,5)]

data3 <- PSO2ventanas[c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")

data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data3$Fecha)){
  data3[i,1] <- 2012 + i
}

data3 <- data3[,c(1,5)]


PSO2 <- merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T)

PSO2$mean <- rowMeans(PSO2[, 2:5], na.rm=TRUE)

library("matrixStats")

PSO2$std <- rowSds(as.matrix(PSO2[, 2:5]), na.rm=TRUE)

ggplot(data=PSO2, aes(x=Fecha, y=mean, group=1))+
  geom_line(linetype = "dashed")+
  geom_point()
  #geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.1)


################### PUCHUNCAVÍ MP10 ########################################

data0 <- PMP10estacionpuch[c(1,3:5)]
colnames(data0) <- c("Fecha","Validado","Preliminar","NoValidado")
data0$estacionPuch <- rowMeans(data0[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data0$Fecha)){
  data0[i,1] <- 1993 + i
}

data0 <- data0[,c(1,5)]

data1 <- PMP10lagreda[c(1,3:5)]
colnames(data1) <- c("Fecha","Validado","Preliminar","NoValidado")

data1$lagreda <- rowMeans(data1[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data1$Fecha)){
  data1[i,1] <- 1992 + i
}

data1 <- data1[,c(1,5)]

data2 <- PMP10maitenes[c(1,3:5)]
colnames(data2) <- c("Fecha","Validado","Preliminar","NoValidado")

data2$maitenes <- rowMeans(data2[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data2$Fecha)){
  data2[i,1] <- 1994 + i
}

data2 <- data2[,c(1,5)]

data3 <- PMP10ventanas[c(1,3:5)]
colnames(data3) <- c("Fecha","Validado","Preliminar","NoValidado")

data3$ventanas <- rowMeans(data3[, c("Validado", "Preliminar","NoValidado")], na.rm=TRUE)

for (i in 1:length(data3$Fecha)){
  data3[i,1] <- 2012 + i
}

data3 <- data3[,c(1,5)]


PMP10 <- merge(data3,merge(merge(data0,data1,by="Fecha",all=T),data2,by="Fecha",all=T),by="Fecha",all=T)

PMP10$mean <- rowMeans(PMP10[, 2:5], na.rm=TRUE)

library("matrixStats")

PMP10$std <- rowSds(as.matrix(PMP10[, 2:5]), na.rm=TRUE)

ggplot(data=PMP10, aes(x=Fecha, y=mean, group=1))+
  geom_line(linetype = "dashed")+
  geom_point()+
  ggtitle("Concentración de SO2 en el aire de Quinteros y Puchuncaví") + 
  xlab("Año") + ylab("Concentración de SO2 en atmósfera (ppb)")