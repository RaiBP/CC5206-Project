library(readxl)
library("reshape")
library(ggplot2)
egrtot<-read.csv("datasets\\egr2001_2017.csv", header = T)
#datapersonas<-read_xlsx("datasets\\base_2002a2020_v3.xlsx")
#View(datapersonas[datapersonas$nombre_comuna=="Quintero",c("Comuna")])
codigoQuin<-5107
egrquin<-egrtot[egrtot$COMUNA==codigoQuin,]
egrquin[is.na(egrquin)]<-0
#Codigo de enfermedad (cambiar para ver otra)
abquin<-egrquin[egrquin$id10=="O030",]
abquin<-melt(abquin, id=c("X","id10", "COMUNA","dec10"))
anho<-seq(2001,2017,1)
abquin<-data.frame(abquin,anho)

ggplot(abquin, aes(x=anho, y=value)) +
  geom_line()+
  geom_point()+
  ggtitle("Aborto espontaneo por infeccion genital y pelviana") + 
  xlab("Año") + ylab("Egresos")