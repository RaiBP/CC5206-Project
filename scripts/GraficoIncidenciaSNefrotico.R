source("scripts\\incidenciaNacional.R")
source("scripts\\incidenciaQuintero.R")
source("scripts\\incidenciaPuchuncavi.R")
library("reshape")
library("ggplot2")

DIAG <- paste("N04", c(0:9), sep="")

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
  geom_line(aes(y=incidenciaQuintero*90, colour = "Quintero")) + 
  geom_line(aes(y=incidenciaPuchuncavi*90, colour = "Puchuncavi")) + 
  scale_y_continuous(sec.axis = sec_axis(~./90, name = "Incidencias Comunales")) + 
  labs(y = "Incidencia Nacional",
       x = "Año",
       colour = "Comuna")+ 
  ggtitle("Incidencia de Sindrome Nefrótico")
