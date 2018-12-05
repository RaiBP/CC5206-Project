# Crear datasets a usar
source("scripts/Preeliminares/importar.R")
source("scripts/Preeliminares/reducir.R")
source("scripts/Preeliminares/fecha_egr.R")

print("Generando datasets:")
for (year in 2001:2017){
  print(paste(">Para el año:", year, "...",sep=" "))
  egresos <- paste("egr", year, sep="")
  file <- paste("datasets/Reducidos/egr",year,".csv", sep="")
  write.csv(reducir(get(egresos), codDiag), file=file)
  print(paste(">Data set generado en ", file, sep=""))
}

print("Completado :)")