egrSinQP <- filter(egr,!(egr$COMUNA == 5107 | egr$COMUNA == 5105))
incidencia <- as.data.frame(unique(egr$id10))
colnames(incidencia) <- c("id10")
diagnosticos <- incidencia$id10


########## calcular promedio de incidencias ################
# for (j in 1:16){
#   year <- 2001 + j
#   incN <-  paste("incN",year, sep="")
#   incidencia[,incN] <- NA
# }
# 
# for (d in diagnosticos){
#   incPromedios <- colMeans(egrSinQP[egrSinQP$id10==d,22:37])
#   for (j in 1:16){
#     print(paste(year, ": ",d,sep=""))
#     year <- 2001 + j
#     Nyear <- paste("N",year, sep="")
#     ayear <- paste("a",year, sep="")
#     Nrel <-  paste("Nrel",year, sep="")
#     incN <-  paste("incN",year, sep="")
#     incidencia[incidencia$"id10"==d,incN] = incPromedios[j]
#   }
# }
#######################################################

A <- function(x,y) x / y

isEmpty <- function(x) {
    return(length(x)==0)
}

for (j in 1:16){
  year <- 2001 + j
  RRQyear <-  paste("RRQ", year, sep="")
  RRPyear <-  paste("RRP", year, sep="")
  incidencia[,RRQyear] <- NA
  incidencia[,RRPyear] <- NA
}

poblSinQP <-  filter(pobl,!(pobl$Comuna == 5107 | pobl$Comuna == 5105))

poblacionTotal <- colSums(poblSinQP[,8:23])


s <- 1


for (d in diagnosticos){
  incidenciaNacional <- colSums(egrSinQP[egrSinQP$id10==d,6:21])
  for (j in 1:16){
    incidenciaNacional[j] <- sapply(incidenciaNacional[j], A, poblacionTotal[j])
    Nyear <- paste("N",year, sep="")
    year <- 2001 + j
    print(paste(s,". ",year, ": ",d,sep=""))
    s <- s+1
    Nrel <- paste("Nrel",year, sep="")
    incN <-  paste("incN",year, sep="")
    RRQyear <-  paste("RRQ", year, sep="")
    RRPyear <-  paste("RRP", year, sep="")
    if (isEmpty(egr[egr$COMUNA==5107 & egr$id10==d,match(Nrel,colnames(egr))])){
      incidencia[incidencia$"id10"==d,RRQyear] <- 0
    }
    else{
      incidencia[incidencia$"id10"==d,RRQyear] <- sapply(egr[egr$COMUNA==5107 & egr$id10==d,match(Nrel,colnames(egr))], A, incidenciaNacional[j])
    }
    if (isEmpty(egr[egr$COMUNA==5105 & egr$id10==d,match(Nrel,colnames(egr))])){
      incidencia[incidencia$"id10"==d,RRPyear] <- 0
    }
    else{
      incidencia[incidencia$"id10"==d,RRPyear] <- sapply(egr[egr$COMUNA==5105 & egr$id10==d,match(Nrel,colnames(egr))], A, incidenciaNacional[j])
    }
  }
}

library(reshape)



# write.csv(incidencia, file = "datasets\\RRmejorado.csv")

incidencia <- read.csv("datasets\\RRnacionalFinal.csv", header=TRUE)
is.na(incidencia) <- sapply(incidencia,is.infinite)
# 
# meanP <- rowMeans(incidencia[,seq(2,32,2)],na.rm=TRUE)
# meanQ <- rowMeans(incidencia[,seq(3,33,2)],na.rm=TRUE)
# incidencia <- data.frame(incidencia,meanQ)
# incidencia <- data.frame(incidencia,meanP)
# 
# 
# codDiag <- read.csv("datasets\\codigosDiagnosticos.csv",header=TRUE)
# codDiag <- codDiag[,2:3]
# incidencia <- merge(incidencia, codDiag, by="id10")
# mean <- rowMeans(incidencia[,c("meanQ","meanP")])
# incidencia <- data.frame(incidencia,mean)
# colnames(incidencia)[37] <- "mean"

diagInteres <- intersect(incidencia[order(incidencia$meanQ, decreasing = T),][1:300,"id10"],incidencia[order(incidencia$meanP, decreasing = T),][1:300,"id10"])

incidenciaInt <- incidencia[incidencia$id10 %in% diagInteres,]

ggplot(incidencia[order(incidencia$meanQ, decreasing = T),][1:30,], aes(x=reorder(dec10, meanQ), y=meanQ)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Diagnósticos con mayor riesgo relativo nacional en Quintero") + 
  xlab("Diagnóstico") + ylab("Riesgo relativo c/r a incidencia nacional, promediado de 2002 a 2017")


ggplot(incidencia[order(incidencia$meanP, decreasing = T),][1:30,], aes(x=reorder(dec10, meanP), y=meanP)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Diagnósticos con mayor riesgo relativo nacional en Puchuncaví") + 
  xlab("Diagnóstico") + ylab("Riesgo relativo c/r a incidencia nacional, promediado de 2002 a 2017")

ggplot(melt(incidenciaInt[,c("meanQ","meanP","dec10","mean")], id.vars=c("mean","dec10")), aes(x=reorder(dec10, mean), y=value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill="Comuna") +
  scale_fill_discrete(labels = c("Quintero","Puchuncaví")) +
  ggtitle("Diagnósticos con mayor riesgo relativo nacional") + 
  xlab("Diagnóstico") + ylab("Riesgo relativo c/r a incidencia nacional, promediado de 2002 a 2017")



ggplot(melt(incidencia[order(incidencia$mean, decreasing = T),][1:60,][,c("meanQ","meanP","dec10","mean")], id.vars=c("mean","dec10")), aes(x=reorder(dec10, mean), y=value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill="Comuna") +
  scale_fill_discrete(labels = c("Quintero","Puchuncaví")) +
  ggtitle("Diagnósticos con mayor riesgo relativo nacional") + 
  xlab("Diagnóstico") + ylab("Riesgo relativo c/r a incidencia nacional, promediado de 2002 a 2017")
