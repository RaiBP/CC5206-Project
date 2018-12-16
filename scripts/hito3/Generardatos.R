library("plyr")
source("scripts\\hito2\\obtenerContaminacionMensual.R")


### Cambiar dataset leido si se quiere evaluar otro egreso
test<-read.csv("datasets\\time series\\PNeumoniaTS.csv")
test$egreso[test$amount<=3]<-"Bajo"
test$egreso[test$amount<=6 & test$amount>3]<-"Medio"
test$egreso[test$amount>6]<-"Alto"
egresos<-data.frame(test$month,test$egreso)
names(egresos)<-c("Fecha","Egresos")
egresos$Fecha<-as.Date(egresos$Fecha,"%Y-%m-%d")

PNOsinNA$Fecha<-PNO[complete.cases(PNO$mean),]$Fecha
PNOsinNA$PNOm<-PNO[complete.cases(PNO$mean),]$mean
PNOsinNA<-data.frame(PNOsinNA)


PNO2sinNA$Fecha<-PNO2[complete.cases(PNO2$mean),]$Fecha
PNO2sinNA$PNO2m<-PNO2[complete.cases(PNO2$mean),]$mean
PNO2sinNA<-data.frame(PNO2sinNA)

PHCNMsinNA$Fecha<-PHCNM[complete.cases(PHCNM$mean),]$Fecha
PHCNMsinNA$PHCNMm<-PHCNM[complete.cases(PHCNM$mean),]$mean
PHCNMsinNA<-data.frame(PHCNMsinNA)

PHCTsinNA$Fecha<-PHCT[complete.cases(PHCT$mean),]$Fecha
PHCTsinNA$PHCTm<-PHCT[complete.cases(PHCT$mean),]$mean
PHCTsinNA<-data.frame(PHCTsinNA)

PMP10sinNA$Fecha<-PMP10[complete.cases(PMP10$mean),]$Fecha
PMP10sinNA$PMP10m<-PMP10[complete.cases(PMP10$mean),]$mean
PMP10sinNA<-data.frame(PMP10sinNA)

POzonosinNA$Fecha<-POzono[complete.cases(POzono$mean),]$Fecha
POzonosinNA$POzonom<-POzono[complete.cases(POzono$mean),]$mean
POzonosinNA<-data.frame(POzonosinNA)

PSO2sinNA$Fecha<-PSO2[complete.cases(PSO2$mean),]$Fecha
PSO2sinNA$PSO2m<-PSO2[complete.cases(PSO2$mean),]$mean
PSO2sinNA<-data.frame(PSO2sinNA)

contaminantes<-merge(PNOsinNA,PNO2sinNA,by="Fecha")
contaminantes<-merge(contaminantes,PHCTsinNA,by="Fecha")
contaminantes<-merge(contaminantes,PHCNMsinNA,by="Fecha")
contaminantes<-merge(contaminantes,PMP10sinNA,by="Fecha")
contaminantes<-merge(contaminantes,POzonosinNA,by="Fecha")
contaminantes<-merge(contaminantes,PSO2sinNA,by="Fecha")

final<-merge(egresos,contaminantes,by="Fecha")

write.csv(final, file = "datasets\\datahito3.csv")
