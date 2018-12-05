# Importar datasets
library("readxl")
library("foreign")

# Importar ingresos
print("Importando datasets de ingresos")
print("Esto puede tardar...")
for (year in 2001:2017){
  assign( paste("egr",year,sep='') ,
         read.csv( paste("datasets/Reducidos/egr",year
                         ,".csv",sep=''), header=TRUE,
                   row.names = 1
                   )
         )
  print( paste(year-2000,"de 17...", sep=" ") )
}
print("Completado :)")