# Datasets:
source("scripts/Association Rules/importarReducidos.R")

# Filtrar Parto único espontáneo y cesárea

filtro <- c('O809', 'O800', 'O829', 'O838')

for (year in 2001:2017){
  temp <- paste('egr', year, sep='')
  temp_data <- get(temp)
  temp_data <- temp_data[!temp_data$id10 %in% filtro,]
  assign(temp, temp_data)
}

# Quintero como ejemplo
codComuna <- 5107
comuna <- "Quintero"

# Agrupar todas las enfermedades

folder <- 'datasets/Association Rules/Egresos por fecha/'

for (year in 2001:2017){
  temp <- paste('egr', year, sep='')
  temp_data <- get(temp)
  temp_data <- temp_data[temp_data$COMUNA == 5107,]
  temp_data <- aggregate(N~FECHA_EGR, temp_data, FUN = sum)
  temp <- paste(comuna, year, sep = '')
  file_name <- paste(folder, temp, sep = '')
  assign(temp, temp_data)
  write.csv(temp_data, file=file_name)
}