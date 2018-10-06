egr2017 <- read.csv("Egr_2017.csv",header=TRUE,sep=";")

install.packages("readxl")
library("readxl")

estab <- read_excel("Base_Establecimientos_ChileDEIS_MINSAL2015-02-04-2015.xlsx")

diag2017 <- egr2017$DIAG1

install.packages("data.table")
library(data.table)

diag2017 <- data.table(diag2017)

diagFrec <- diag2017[,.N,by=diag2017]
diagFrec <- diagFrec[order(diagFrec$N,decreasing=TRUE), ]

codDiag <- read.csv("CSV_CIE10.csv",header=TRUE,sep=",")

names(diagFrec)[names(diagFrec)=="diag2017"] <- "id10"

diagFrec <- merge(diagFrec, codDiag, by="id10") #diagnosticos y sus frecuencias en todo Chile

quintero17 <- with(egr2017,egr2017[COMUNA==5107,])

diagQ17 <- quintero17$DIAG1
diagQ17 <- data.table(diagQ17)
frecQ17 <- diagQ17[,.N,by=diagQ17]
names(frecQ17)[names(frecQ17)=="diagQ17"] <- "id10"
frecQ17 <- merge(frecQ17, codDiag, by="id10") #diagnosticos y sus frecuencias en pacientes de Quinteros

puchuncavi17 <- with(egr2017,egr2017[COMUNA==5105,])

diagP17 <- puchuncavi17$DIAG1
diagP17 <- data.table(diagP17)
frecP17 <- diagP17[,.N,by=diagP17]
names(frecP17)[names(frecP17)=="diagP17"] <- "id10"
frecP17 <- merge(frecP17, codDiag, by="id10") #diagnosticos y sus frecuencias en pacientes de Puchuncavi