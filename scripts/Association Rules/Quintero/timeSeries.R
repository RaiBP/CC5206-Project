library(data.table)
library(lubridate)
library(dplyr)

# Datasets:
source("scripts/Association Rules/Quintero/generar.R")

# errores
Quintero2001[Quintero2001$FECHA_EGR == '2000-01-01', 1] <- '2001-01-01'
Quintero2001 <- aggregate(N ~ FECHA_EGR, Quintero2001, FUN = sum)
Quintero2002$FECHA_EGR <- as.Date(Quintero2002$FECHA_EGR,tryFormats = "%d-%m-%Y")
Quintero2002$FECHA_EGR <- as.factor(Quintero2002$FECHA_EGR)
Quintero2004$FECHA_EGR <- as.Date(Quintero2004$FECHA_EGR, tryFormats = "%d-%m-%Y")
Quintero2004$FECHA_EGR <- as.factor(Quintero2004$FECHA_EGR)
Quintero2006$FECHA_EGR <- as.Date(Quintero2006$FECHA_EGR,tryFormats = "%d-%m-%Y")
Quintero2006$FECHA_EGR <- as.factor(Quintero2006$FECHA_EGR)

# Comenzar
zmonthTotal <- data.frame(month = c(), amount = c())

for (year in 2001:2017){
  print(paste("Vamos en", year))
  dates <- seq.Date(
    as.Date(paste(year, "01-01", sep = '-')),
    as.Date(paste(year, "12-31", sep = '-')),
    "day")
  temp <- paste('Quintero', year, sep='')
  data_temp <- get(temp)
  data_temp$FECHA_EGR <- as.Date(data_temp$FECHA_EGR)
  dates <- data.frame(dates)
  colnames(dates) <- "FECHA_EGR"
  z <- merge(data_temp,dates, by="FECHA_EGR", all=TRUE)
  z <- na.omit(z)
  z[is.na(z)] <- 0
  zmonth <- z %>% group_by(month=floor_date(FECHA_EGR, "month")) %>%
    summarize(amount=sum(N))
  zmonthTotal <- rbind(zmonthTotal, zmonth)
}

file_name <- paste(folder, "QuinteroMensual", sep='')
write.csv(zmonthTotal, file = file_name)