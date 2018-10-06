library(data.table)
diagF <- function(egresos, codDiag, comuna){
  diag <- egresos$DIAG1
  diag <- data.table(diag)
  
  diagFrec <- diag[,.N,by=diag]
  diagFrec <- diagFrec[order(diagFrec$N,decreasing=TRUE), ]
  
  names(diagFrec)[names(diagFrec)=="diag"] <- "id10"
  
  diagFrec <- merge(diagFrec, codDiag, by="id10")
  
  comunaTabla <- with(egresos, egresos[COMUNA==comuna,])
  
  diag <- comunaTabla$DIAG1
  diag <- data.table(diag)
  frec <- diag[,.N,by=diag]
  names(frec)[names(frec)=="diag"] <- "id10"
  merge(frec, codDiag, by="id10")
}