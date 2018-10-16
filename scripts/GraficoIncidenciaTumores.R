source("scripts\\incidenciaNacional.R")
source("scripts\\incidenciaQuintero.R")
source("scripts\\incidenciaPuchuncavi.R")
library("reshape")
library("ggplot2")

DIAG1 <- paste("C00", c(0:9), sep="")
DIAG2 <- paste("C0", c(10:99), sep="")
DIAG3 <- paste("C", c(100:999), sep="")
DIAG <- c(DIAG1, DIAG2, DIAG3)

quintero <- with(incidenciaQuintero, incidenciaQuintero[id10 %in% DIAG,])
puchuncavi <- with(incidenciaPuchuncavi, incidenciaPuchuncavi[id10 %in% DIAG,])
nacional <- with(incidenciaNacional, incidenciaNacional[id10 %in% DIAG,])

quintero <- melt(quintero, id=c("id10"))
quintero <- aggregate(value ~ variable, quintero, FUN = sum)
puchuncavi <- melt(puchuncavi, id=c("id10"))
puchuncavi <- aggregate(value ~ variable, puchuncavi, FUN = sum)
nacional <- melt(nacional, id=c("id10"))
nacional <- aggregate(value ~ variable, nacional, FUN = sum)

anho<-seq(2002,2017,1)

quintero <- data.frame("anho" = anho, "incidenciaQuintero" = quintero$value)
puchuncavi <- data.frame("anho" = anho, "incidenciaPuchuncavi" = puchuncavi$value)
nacional <- data.frame("anho" = anho, "incidenciaNacional" = nacional$value)


grafico <- merge(quintero, puchuncavi)
grafico <- merge(grafico, nacional)


ggplot(grafico, aes(x = anho)) +
  geom_line(aes(y=incidenciaNacional, colour = "Nacional")) + 
  geom_line(aes(y=incidenciaQuintero*140, colour = "Quintero")) + 
  geom_line(aes(y=incidenciaPuchuncavi*140, colour = "Puchuncavi")) + 
  scale_y_continuous(sec.axis = sec_axis(~./140, name = "Incidencias Comunales")) + 
  labs(y = "Incidencia Nacional",
       x = "Año",
       colour = "Comuna")+ 
  ggtitle("Incidencia de Tumores Malignos (egresos con el codigo CXXX)")
