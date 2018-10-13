
allegr <- read.csv("datasets\\egr2001.csv",header=T)
allegr<- allegr[ , c("id10","COMUNA","dec10","N2001")]
for (year in 2002:2017){
  file <- paste("datasets\\egr",year,".csv", sep="")
  temp <- read.csv(file,header=T)
  anio <- paste("N",year,sep = "")
  temp <- temp[,c("id10","COMUNA","dec10",anio)]
  allegr <- merge(allegr,temp,by=c("id10","COMUNA","dec10"),all=T)
}
write.csv(allegr, file = "datasets\\egr2001_2017.csv")
