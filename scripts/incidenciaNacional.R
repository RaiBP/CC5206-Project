incidencia <- read.csv("datasets\\incidenciaPorComuna.csv")
print("Importado")
incidenciaNacional <- data.frame("id10" = unique(incidencia[,"id10"]))
incidenciaNacional <- data.frame("id10" = incidenciaNacional[order(incidenciaNacional$id10), ])
incidenciaCorregido <- with(incidencia,incidencia[!(COMUNA == 5107 | COMUNA == 5105),])

for (year in 2002:2017){
  temp <- c(incidenciaCorregido[,paste("Nrel", year, sep="")])
  temp <- data.frame("id10" = incidenciaCorregido[,"id10"], "N" = temp)
  temp <- aggregate( N ~ id10, temp, FUN = sum)
  incidenciaNacional$N = temp$N
  names(incidenciaNacional)[names(incidenciaNacional)=="N"] <- paste("N", year, sep="")
}

write.csv(incidenciaNacional, file = "datasets\\incidenciaNacional.csv")
print("Incidencia Nacional generado")