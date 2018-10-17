library(dplyr)
egr <- read.csv("datasets\\egr2001_2017.csv")
pobl <- read.csv("datasets\\datasumper.csv")
egr[is.na(egr)] <- 0
egrSinQP <- filter(egr,!(egr$COMUNA == 5107 | egr$COMUNA == 5105))
incidenciaN <- as.data.frame(unique(egr$id10))
colnames(incidenciaN) <- c("id10")
diagnosticos <- incidenciaN$id10

for (j in 1:16){
  year <- 2001 + j
  incNyear <-  paste("incN", year, sep="")
  incidenciaN[,incNyear] <- NA
}

A <- function(x,y) x / y

isEmpty <- function(x) {
  return(length(x)==0)
}

poblSinQP <-  filter(pobl,!(pobl$Comuna == 5107 | pobl$Comuna == 5105))

poblacionTotal <- colSums(poblSinQP[,8:23])
s<-1
for (d in diagnosticos){
  incidenciaNacional <- colSums(egrSinQP[egrSinQP$id10==d,6:21])
  for (j in 1:16){
    year <- 2001 + j
    print(paste(s,". ",year, ": ",d,sep=""))
    s <- s+1
    incNyear <-  paste("incN", year, sep="")
    incidenciaN[incidenciaN$id10==d,incNyear] <- sapply(incidenciaNacional[j], A, poblacionTotal[j])
    }
  }

write.csv(incidenciaN,"datasets\\incidenciaN.csv")