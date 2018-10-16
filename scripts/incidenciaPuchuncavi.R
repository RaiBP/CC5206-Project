# source("scripts\\incidenciaNacional.R")

PUCHUNCAVI<-5105

incidenciaPuchuncavi <- with(incidencia, incidencia[COMUNA==PUCHUNCAVI,])
temp <- data.frame("id10" = unique(incidenciaPuchuncavi[,"id10"]))
temp <- data.frame("id10" = temp[order(temp$id10), ])

for (year in 2002:2017){
  temp2 <- c(incidenciaPuchuncavi[,paste("Nrel", year, sep="")])
  temp2 <- data.frame("id10" = incidenciaPuchuncavi[,"id10"], "N" = temp2)
  temp2 <- aggregate( N ~ id10, temp2, FUN = sum)
  temp$N = temp2$N
  names(temp)[names(temp)=="N"] <- paste("N", year, sep="")
}

incidenciaPuchuncavi <- temp
write.csv(incidenciaPuchuncavi, file = "datasets\\incidenciaPuchuncavi.csv")
