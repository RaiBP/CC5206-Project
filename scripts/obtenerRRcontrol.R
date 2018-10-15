RR <- read.csv("datasets\\incidencia.csv", header=TRUE)
RR <- RR[,1:18]

A <- function(x,y) x / y

isEmpty <- function(x) {
  return(length(x)==0)
}

for (j in 1:16){
  year <- 2001 + j
  RRQyear <-  paste("RRQ", year, sep="")
  RRPyear <-  paste("RRP", year, sep="")
  RR[,RRQyear] <- NA
  RR[,RRPyear] <- NA
}

pucon <- 9115 
teno <- 7308
yerbasbuenas <- 7408

for (d in diagnosticos){
  for (j in 1:16){
    year <- 2001 + j
    print(paste(year, ": ",d,sep=""))
    Nrel <- paste("Nrel",year, sep="")
    incN <-  paste("incN",year, sep="")
    RRQyear <-  paste("RRQ", year, sep="")
    RRPyear <-  paste("RRP", year, sep="")
    if (isEmpty(egr[egr$COMUNA==5107 & egr$id10==d,match(Nrel,colnames(egr))])){
      RR[RR$"id10"==d,RRQyear] <- 0
    }
    else if (isEmpty(egr[egr$COMUNA==teno & egr$id10==d, match(Nrel,colnames(egr))])){
      RR[RR$"id10"==d,RRQyear] <- NA
    }
    
    else {
      RR[RR$"id10"==d,RRQyear] <- sapply(egr[egr$COMUNA==5107 & egr$id10==d,match(Nrel,colnames(egr))], A, egr[egr$COMUNA==teno & egr$id10==d, match(Nrel,colnames(egr))])
    }
    
    if (isEmpty(egr[egr$COMUNA==5105 & egr$id10==d,match(Nrel,colnames(egr))])){
      RR[RR$"id10"==d,RRPyear] <- 0
    }
    else if (isEmpty(egr[egr$COMUNA==yerbasbuenas & egr$id10==d, match(Nrel,colnames(egr))])){
      RR[RR$"id10"==d,RRQyear] <- NA
    }
    
    else{
      RR[RR$"id10"==d,RRPyear] <- sapply(egr[egr$COMUNA==5105 & egr$id10==d,match(Nrel,colnames(egr))], A, egr[egr$COMUNA==yerbasbuenas & egr$id10==d,match(Nrel,colnames(egr))])
    }
  }
}

# write.csv(RR, file = "datasets\\RR.csv")

is.na(RR) <- sapply(RR,is.infinite)

meanP <- rowMeans(RR[,seq(20,50,2)],na.rm=TRUE)
meanQ <- rowMeans(RR[,seq(19,49,2)],na.rm=TRUE)
RR <- data.frame(RR,meanQ)
RR <- data.frame(RR,meanP)


codDiag <- read.csv("datasets\\codigosDiagnosticos.csv",header=TRUE)
codDiag <- codDiag[,2:3]
RR <- merge(RR, codDiag, by="id10")

ggplot(RR[order(RR$meanQ, decreasing = T),][1:30,], aes(x=reorder(dec10, meanQ), y=meanQ)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Diagnósticos con mayor riesgo relativo nacional en Quintero") + 
  xlab("Diagnóstico") + ylab("Riesgo relativo c/r a incidencia Pucón, promediado de 2002 a 2017")


