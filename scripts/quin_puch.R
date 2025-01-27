# Crear datasets a usar
source("importar.R")
source("diagF.R")
# Que necesitamos
# Comunas
quin = 5107 # Quintero
puch = 5105 # Puchuncavi
# Las que necesiten extras


# Para Quintero
print("Generando datasets de Quintero:")
loop = F
for (year in 2001:2017){
  print(paste("Para el a�o:", year, "...",sep=" "))
  egresos = paste("egr", year, sep="")
  temp = diagF(get(egresos), codDiag, quin) #temporal
  file = paste("datasets/Quintero_anual/quin",year,".csv", sep="_")
  write.csv(temp, file=file)
  print(paste("Data set generado en ", file, sep=""))
  if (loop){
    temp$anio <- year
    listaTocomple = rbind(listaTocomple, temp)
  }
  else {
    temp$anio <- year
    listaTocomple = temp
  }
  loop = T
}
print("Dataset de todo Quintero")
write.csv(listaTocomple, file = "datasets/Comunal/quintero.csv")
print("Data set generado en datasets/Comunal/quintero.csv")

# Para Puchuncavi
print("Generando datasets de Puchuncavi:")
loop = F
for (year in 2001:2017){
  print(paste("Para el a�o:", year, "...",sep=" "))
  egresos = paste("egr", year, sep="")
  temp = diagF(get(egresos), codDiag, puch) #temporal
  file = paste("datasets/Puchuncavi_anual/puch",year,".csv", sep="_")
  write.csv(temp, file=file)
  print(paste("Data set generado en ", file, sep=""))
  if (loop){
    temp$anio <- year
    listaTocomple2 = rbind(listaTocomple2, temp)
  }
  else {
    temp$anio <- year
    listaTocomple2 = temp
  }
  loop = T
}
print("Dataset de todo Puchuncavi")
write.csv(listaTocomple, file = "datasets/Comunal/puchuncavi.csv")
print("Data set generado en datasets/Comunal/quintero.csv")

listaTocomple$comuna <- "Quintero"
listaTocomple2$comuna <- "Puchuncavi"
listaFinal = rbind(listaTocomple, listaTocomple2)

print("Dataset Quintero/Puchuncavi")
write.csv(listaFinal, file = "datasets/Comunal/quin_puch.csv")
print("Data set generado en datasets/Comunal/quin_puch.csv")