library(data.table)
reducir <- function(egreso, codDiag){
  egreso$N <- 1
  
  reduc <- aggregate(N ~ DIAG1 + ESTAB + COMUNA + FECHA_EGR, egreso, FUN = sum)
  
  names(reduc)[names(reduc)=="DIAG1"] <- "id10"
  
  reduc <- merge(reduc, codDiag, by="id10")
}