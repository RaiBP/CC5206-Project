library(gdata)

library("readxl")

library(data.table)

library("dplyr")


egr <- read.csv("datasets\\egr2001_2017.csv")
egr[is.na(egr)] <- 0


comunas <- pobl$Comuna
comunasSobrantes <- setdiff(unique(egr$COMUNA),comunas)
egr <- filter(egr, !(COMUNA %in% comunasSobrantes))


A <- function(x,y) x / y


for (year in 2002:2017){
  Nyear <- paste("N",year, sep="")
  ayear <- paste("a",year, sep="")
  Nrel <-  paste("Nrel",year, sep="")
  egr[Nrel] <- NA 
  s <- 1
  for (i in comunas){
    nhab <- pobl[pobl$Comuna==i, ayear]
    egr[egr$COMUNA==i,Nrel] <- sapply(egr[egr$COMUNA==i,Nyear], A, nhab)
    print(paste(s, ". ", year,": ",i, sep=""))
    s <- s+1
  }
}