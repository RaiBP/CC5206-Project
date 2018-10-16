library(readxl)
library("reshape")
library(ggplot2)

### ESTE DATASET (incidenciaPorComuna.csv) NO ESTA EN EL GIT PORQUE ES MUY GRANDE, DESCARGAR DEL DRIVE #######
inc<-read.csv("datasets\\incidenciaPorComuna.csv", header = T)

codigoQuin<-5107
codigoPucon <- 9115 
incQ<-inc[inc$COMUNA==codigoQuin,]
inc0 <-inc[inc$COMUNA==codigoPucon,]
incQ[is.na(incQ)]<-0
inc0[is.na(inc0)]<-0


####### Códigos de diagnosticos(cambiar para ver otra) ##########

sindrNefr <- c("N000")
for (i in 1:9){
  n <- paste("N00",i,sep="")
  sindrNefr <- c(sindrNefr,n)
}

for (i in 10:19){
  n <- paste("N0",i,sep="")
  sindrNefr <- c(sindrNefr,n)
}

for (i in 30:39){
  n <- paste("N0",i,sep="")
  sindrNefr <- c(sindrNefr,n)
}

for (i in 50:59){
  n <- paste("N0",i,sep="")
  sindrNefr <- c(sindrNefr,n)
}
############ FIN calculo de codigo diagnosticos #############


abquin<-incQ[incQ$id10 %in% sindrNefr,]
abquin <- abquin[,c(3:5,23:length(abquin))]

sumaInc <- colSums(abquin[,4:19])

abquin[nrow(abquin) + 1,] = c(NA,NA,NA,sumaInc)

abquin<-melt(abquin[nrow(abquin),], id=c("id10", "COMUNA","dec10"))

ab0<-inc0[inc0$id10 %in% sindrNefr,]
ab0 <- ab0[,c(3:5,23:length(ab0))]

sumaInc0 <- colSums(ab0[,4:19])

ab0[nrow(ab0) + 1,] = c(NA,NA,NA,sumaInc0)
ab0<-melt(ab0[nrow(ab0),], id=c("id10", "COMUNA","dec10"))

Fecha<-seq(2002,2017,1)

abquin<-data.frame(abquin,Fecha)
abquin<- abquin[,4:6]
ab0<-data.frame(ab0,Fecha)
ab0<- ab0[,4:6]

d <- merge(abquin[,2:3],SO2[,c(1,3)],all=T)


colnames(d)[2:3] <- c("IncidenciaQ","SO2")

d <- merge(ab0[,2:3],d,by="Fecha",all=T)

colnames(d)[2] <- c("Incidencia0")

ggplot(d[10:nrow(d)-1,], aes(x = Fecha))+ 
  geom_bar(aes(y = SO2), stat = "identity")+ 
  geom_line(aes(y = IncidenciaQ*75000, colour = "Quintero"),size=2)+ 
  geom_line(aes(y = Incidencia0*75000, colour = "Pucón"),size=2)+ 
  scale_y_continuous(sec.axis = sec_axis(~./75, name = "Incidencia por cada mil personas"))+ 
  labs(y = "Concentración de SO2 en atmósfera en Quintero (ppb)",
              x = "Año",
              colour = "Comuna", size=25)+ 
  ggtitle("Incidencia de síndrome nefrítico de Quintero y Pucón (eje derecho) y concentración atmosférica de SO2 en Quintero (eje izquierdo)")+
  scale_x_continuous(breaks = d$Fecha)+ 
  theme(legend.position = c(0.8, 0.9))


