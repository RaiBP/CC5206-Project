# source("scripts\\incidenciaNacional.R")

QUINTERO<-5107

incidenciaQuintero <- with(incidencia, incidencia[COMUNA==QUINTERO,])
temp <- data.frame("id10" = unique(incidenciaQuintero[,"id10"]))
temp <- data.frame("id10" = temp[order(temp$id10), ])

for (year in 2002:2017){
  temp2 <- c(incidenciaQuintero[,paste("Nrel", year, sep="")])
  temp2 <- data.frame("id10" = incidenciaQuintero[,"id10"], "N" = temp2)
  temp2 <- aggregate( N ~ id10, temp2, FUN = sum)
  temp$N = temp2$N
  names(temp)[names(temp)=="N"] <- paste("N", year, sep="")
}

incidenciaQuintero <- temp
write.csv(incidenciaQuintero, file = "datasets\\incidenciaQuintero.csv")