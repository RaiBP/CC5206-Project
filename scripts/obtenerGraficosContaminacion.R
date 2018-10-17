#### PRIMERO CORRER obtenerContaminacion.R ######
library(ggplot2)
library(reshape2)

source("scripts\\obtenerContaminacion.R")

QSO2 <- read.csv("datasets\\contaminacion\\QSO2.csv", header=TRUE)
QMP10 <-  read.csv("datasets\\contaminacion\\QMP10.csv", header=TRUE)
PSO2 <- read.csv("datasets\\contaminacion\\PSO2.csv", header=TRUE)
PMP10 <-  read.csv("datasets\\contaminacion\\PMP10.csv", header=TRUE)

SO2 <- data.frame(PSO2[,c("Fecha","mean")],QSO2[,"mean"])
colnames(SO2) <- c("Fecha","Puchuncavi","Quintero")

SO2long <- melt(SO2, id="Fecha")

colnames(SO2long)[2] <- "Comuna"

ggplot(data=SO2long,
       aes(x=Fecha, y=value, colour=Comuna)) +
  geom_line(size=2)+
  geom_point(size=4)+
  ggtitle("Concentración de SO2 en la atmósfera, Quintero y Puchuncaví") + 
  xlab("Año") + ylab("Concentración de SO2 en atmósfera (ppb)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_continuous(breaks = SO2$Fecha)


MP10 <- data.frame(PMP10[,c("Fecha","mean")],QMP10[,"mean"])
colnames(MP10) <- c("Fecha","Puchuncavi","Quintero")

MP10long <- melt(MP10, id="Fecha")

colnames(MP10long)[2] <- "Comuna"

ggplot(data=MP10long,
       aes(x=Fecha, y=value, colour=Comuna)) +
  geom_line(size=2)+
  geom_point(size=4)+
  ggtitle("Concentración de MP10 en la atmósfera, Quintero y Puchuncaví") + 
  xlab("Año") + ylab("Concentración de MP10 en atmósfera (μg/m3N)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_continuous(breaks = MP10$Fecha)