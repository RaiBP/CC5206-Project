library(readxl)
datapersonas<-read_xlsx("datasets\\base_2002a2020_v3.xlsx")
#print("Dataper")
colnames(dataper)[colnames(dataper)=="Sexo (1=hombres;2=mujeres)"] <- "Sexo"
datasumper20<-aggregate(cbind(a2002,a2003,a2004,a2005,a2006,a2007,a2008,
                            a2009,a2010,a2011,a2012,a2013,a2014,a2015,
                            a2016,a2017,a2018,a2019,a2020) ~ Region + 
                        nombre_region+provincia+Nombre_provincia+Comuna+
                        nombre_comuna+Sexo,dataper[dataper$edad<=20,],FUN=sum)
datasumper20$rango<-20
#print("Agregado")
#write.csv(datasumper, file = "datasets\\datasumper.csv")
datasumper40<-aggregate(cbind(a2002,a2003,a2004,a2005,a2006,a2007,a2008,
                              a2009,a2010,a2011,a2012,a2013,a2014,a2015,
                              a2016,a2017,a2018,a2019,a2020) ~ Region + 
                          nombre_region+provincia+Nombre_provincia+Comuna+
                          nombre_comuna+Sexo,dataper[dataper$edad<=40 & dataper$edad>20,],FUN=sum)
datasumper40$rango<-40

datasumper60<-aggregate(cbind(a2002,a2003,a2004,a2005,a2006,a2007,a2008,
                              a2009,a2010,a2011,a2012,a2013,a2014,a2015,
                              a2016,a2017,a2018,a2019,a2020) ~ Region + 
                          nombre_region+provincia+Nombre_provincia+Comuna+
                          nombre_comuna+Sexo,dataper[dataper$edad<=60 & dataper$edad>40,],FUN=sum)
datasumper60$rango<-60
datasumper80<-aggregate(cbind(a2002,a2003,a2004,a2005,a2006,a2007,a2008,
                              a2009,a2010,a2011,a2012,a2013,a2014,a2015,
                              a2016,a2017,a2018,a2019,a2020) ~ Region + 
                          nombre_region+provincia+Nombre_provincia+Comuna+
                          nombre_comuna+Sexo,dataper[dataper$edad<=80 & dataper$edad>60,],FUN=sum)
datasumper80$rango<-80

datasumperrango<- rbind(datasumper20,datasumper40,datasumper60,datasumper80)