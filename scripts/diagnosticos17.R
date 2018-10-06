egr17 <- read.csv("datasets\\Egr_2017.csv",header=TRUE,sep=";")


tab <- table(egr17$DIAG1,egr17$COMUNA)
tab <- as.data.frame.matrix(tab)
tab <- tab[,1:346]

# barplot(tab, beside=TRUE, legend.text=rownames(tab), ylab="absolute frequency")


#install.packages("gdata")
library(gdata)

#instalar ActivePerl antes de correr la sgte linea: https://www.activestate.com/activeperl
#se puede usar read_excel de la librería "readxl" en su defecto

censo <- read.xls("datasets\\base_2002a2020_v3.xlsx", perl = "C:\\Perl64\\bin\\perl.exe", sheet=1) 
censo17 <- data.frame(censo$Comuna, censo$a2017)
censo17 <- aggregate(censo17$censo.a2017 ~ censo17$censo.Comuna, censo17, FUN=sum)


A <- function(x,y) x / y

#divido cada caso por la poblacion total de la comuna correspondiente
for (i in 1:346){
  codCom <- names(tab)[i]
  nhab <- censo17[censo17$`censo17$censo.Comuna`==codCom,2]
  tab[,i] = sapply(tab[,i], A, nhab)
}

meanNacional<-rowMeans(tab)
tab <- data.frame(tab,meanNacional)

B <- function(x) x*1000

#multiplico por 1000 cada valor para obtener los casos por cada mil habitantes
tab[,1:348] = sapply(tab[,1:348], B)

#install.packages("readxl")
library("readxl")

estab <- read_excel("datasets\\Base_Establecimientos_ChileDEIS_MINSAL2015-02-04-2015.xlsx")

source('obtenerFrecuencias.R')

tab$RelQ <- (tab$X5107 / tab$meanNacional) #diferencia relativa Quintero
tab$RelP <- (tab$X5105 / tab$meanNacional) #diferencia relativa Puchuncavi

tabQP17 <- data.frame(CodigoDiag=rownames(tab),CasosP=tab$X5105,CasosQ=tab$X5107,PromedioNacional=tab$meanNacional,RelQ=tab$RelQ,RelP=tab$RelP)

names(codDiag) <- c("CodigoDiag","Diagnostico")
tabQP17 <- merge(tabQP17,codDiag,by="CodigoDiag")

library(ggplot2)

ggplot(tabQP17[order(tabQP17$RelQ, decreasing = T),][1:30,], aes(x=reorder(Diagnostico, RelQ), y=RelQ)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Porcentajes relativos de diagnósticos, Quintero 2017") + 
  xlab("Diagnóstico") + ylab("Porcentaje relativo sobre promedio nacional")

ggplot(tabQP17[order(tabQP17$RelP, decreasing = T),][1:30,], aes(x=reorder(Diagnostico, RelP), y=RelP)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Porcentajes relativos de diagnósticos, Puchuncaví 2017") + 
  xlab("Diagnóstico") + ylab("Porcentaje relativo sobre promedio nacional")