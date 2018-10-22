library(readxl)
library(reshape2)
library(ggplot2)

### ESTE DATASET (incidenciaPorComuna.csv) NO ESTA EN EL GIT PORQUE ES MUY GRANDE, DESCARGAR DEL DRIVE #######
inc<-read.csv("datasets\\incidenciaPorComuna.csv", header = T)
egr<-read.csv("datasets\\incidenciaPorComuna.csv", header = T)
incN<-read.csv("datasets\\incidenciaN.csv", header = T)

codigoQuin<-5107
codigoPuch<-5105
codigoPucon <- 9115 
incQ<-inc[inc$COMUNA==codigoQuin,]
inc0 <-inc[inc$COMUNA==codigoPuch,]
incQ[is.na(incQ)]<-0
inc0[is.na(inc0)]<-0
incN[is.na(incN)]<-0


####### Códigos de diagnosticos(cambiar para ver otra) ##########

codDiagnostico <- c(    "O030","O031","O032","O033","O034","O035","O036","O037","O038","O039")
############ FIN calculo de codigo diagnosticos #############


abquin<-incQ[incQ$id10 %in% codDiagnostico,]
abquin <- abquin[,c(3:5,23:length(abquin))]

sumaInc <- colSums(abquin[,4:19])

abquin[nrow(abquin) + 1,] = c(NA,NA,NA,sumaInc)

abquin<-melt(abquin[nrow(abquin),], id=c("id10", "COMUNA","dec10"))

ab0<-inc0[inc0$id10 %in% codDiagnostico,]
ab0 <- ab0[,c(3:5,23:length(ab0))]

sumaInc0 <- colSums(ab0[,4:19])

ab0[nrow(ab0) + 1,] = c(NA,NA,NA,sumaInc0)
ab0<-melt(ab0[nrow(ab0),], id=c("id10", "COMUNA","dec10"))


abN<-incN[incN$id10 %in% codDiagnostico,]
abN <- abN[,2:ncol(abN)]

sumaIncN <- colSums(abN[,2:ncol(abN)])

abN[nrow(abN) + 1,] = c(NA,sumaIncN)
abN<-melt(abN[nrow(abN),], id=c("id10"))


Fecha<-seq(2002,2017,1)

abquin<-data.frame(abquin,Fecha)
abquin<- abquin[,5:6]
ab0<-data.frame(ab0,Fecha)
ab0<- ab0[,5:6]
abN<-data.frame(abN,Fecha)
abN<- abN[,3:4]

d <- merge(ab0,abquin,by="Fecha",all=T)

colnames(d)[2:3] <- c("Incidencia0","IncidenciaQ")


d <- merge(abN,d,by="Fecha",all=T)

colnames(d)[2] <- c("IncidenciaN")

ggplot(d, aes(x = Fecha))+ 
geom_line(aes(y = IncidenciaQ*1000, colour = "Quintero"),size=2, na.rm=TRUE)+ 
  geom_point(aes(y = IncidenciaQ*1000, colour = "Quintero"),size=4, na.rm=TRUE)+
  geom_line(aes(y = Incidencia0*1000, colour = "Puchuncaví"),size=2, na.rm=TRUE)+
  geom_point(aes(y = Incidencia0*1000, colour = "Puchuncaví"),size=4, na.rm=TRUE)+
  geom_line(aes(y = IncidenciaN*1000, colour = "Nacional"),size=2, na.rm=TRUE)+ 
  geom_point(aes(y = IncidenciaN*1000, colour = "Nacional"),size=4, na.rm=TRUE)+
  labs(y = "Incidencia por cada mil personas",
       x = "Año",
       colour = "Comuna", size=25)+ 
  ggtitle("Incidencia de aborto espontáneo de Quintero, Puchuncaví y a nivel nacional (sin contar Quintero ni Puchuncaví)")+
  scale_x_continuous(breaks = d$Fecha)+ 
  theme(legend.position = c(0.8, 0.9))