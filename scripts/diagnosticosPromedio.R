library(gdata)
library(ggplot2)

QUINTERO <- 5107
PUCHUNCAVI <- 5105

egr <- read.csv("datasets\\egr2001_2017.csv")
egr[is.na(egr)] <- 0

egr$Nprom <- 0

for (year in 2002:2017){
  col = paste("N", year, sep = "")
  egr$Nprom <- egr$Nprom + egr[,col]
}

egr$Nprom <- egr$Nprom / (2017 - 2002 + 1)

egr <- data.frame("Comuna" = egr$COMUNA, "id10" = egr$id10, "Diag" = egr$dec10, "Nprom" = egr$Nprom)

datasumper <- read.csv("datasets\\datasumper.csv")

nacional <- aggregate(Nprom ~ id10, egr, FUN = sum)

datasumper$prom <- 0

for (year in 2002:2017){
  col = paste("a", year, sep = "")
  datasumper$prom <- datasumper$prom + datasumper[,col]
}

datasumper$prom <- datasumper$prom / (2017 - 2002 + 1)

pobl <- data.frame("Comuna" = datasumper$Comuna, "poblProm" = datasumper$prom)

parametroNacional <- data.frame("id10" = nacional$id10, "RelN" = nacional$Nprom)

parametroNacional$RelN <- parametroNacional$RelN / (sum(pobl$poblProm))

# Quintero
enQuintero <- with(egr, egr[Comuna == QUINTERO, ])
enQuintero$Nprom <- enQuintero$Nprom / (with(pobl, pobl[Comuna == QUINTERO, ])$poblProm)
colnames(enQuintero)[colnames(enQuintero)=="Nprom"] <- "Rel"
enQuintero <- merge(enQuintero, parametroNacional, by="id10")
enQuintero$Rel <- enQuintero$Rel/enQuintero$RelN

# Puchuncavi
enPuchuncavi <- with(egr, egr[Comuna == PUCHUNCAVI, ])
enPuchuncavi$Nprom <- enPuchuncavi$Nprom / (with(pobl, pobl[Comuna == PUCHUNCAVI, ])$poblProm)
colnames(enPuchuncavi)[colnames(enPuchuncavi)=="Nprom"] <- "Rel"
enPuchuncavi <- merge(enPuchuncavi, parametroNacional, by="id10")
enPuchuncavi$Rel <- enPuchuncavi$Rel/enPuchuncavi$RelN

ggplot(enQuintero[order(enQuintero$Rel, decreasing = T),][1:30,], aes(x=reorder(Diag, Rel), y=Rel)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Porcentajes relativos de diagn?sticos, Quintero promedio 2002 a 2017") + 
  xlab("Diagn?stico") + ylab("Porcentaje relativo sobre promedio nacional")

ggplot(enPuchuncavi[order(enPuchuncavi$Rel, decreasing = T),][1:30,], aes(x=reorder(Diag, Rel), y=Rel)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Porcentajes relativos de diagn?sticos, Puchuncavi promedio 2002 a 2017") + 
  xlab("Diagn?stico") + ylab("Porcentaje relativo sobre promedio nacional")
