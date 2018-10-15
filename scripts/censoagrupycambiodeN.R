library(readxl)
dataper<-read_xlsx("datasets\\base_2002a2020_v3.xlsx")
print("Dataper")
datasumper<-aggregate(cbind(a2002,a2003,a2004,a2005,a2006,a2007,a2008,a2009,a2010,a2011,a2012,a2013,a2014,a2015,a2016,a2017,a2018,a2019,a2020) ~ Region + nombre_region+provincia+Nombre_provincia+Comuna+nombre_comuna,dataper,FUN=sum)
print("Agregado")
dif<-datasumper[datasumper$nombre_comuna=="Puchuncaví",c("a2017")]
datasumper$diferencia<-abs(datasumper$a2017-dif)
write.csv(datasumper, file = "datasets\\datasumper.csv")

for (year in 2001:2017){
  file <- paste("datasets\\egr",year,".csv", sep="")
  temp <- read.csv(file,header=T)
  colnames(temp)[colnames(temp)=="N"] <- paste("N",year,sep="")
  write.csv(temp,file)
  print(paste("Va en el a?o",year, sep=" "))
}