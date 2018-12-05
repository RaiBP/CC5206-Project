# Necesitaba que todos los atributos de fecha egreso se llamen igual

estandar = "FECHA_EGR"
names(egr2001)[names(egr2001)=="FECHA_EGRE"] <- estandar
names(egr2002)[names(egr2002)=="EGRESO"] <- estandar
names(egr2003)[names(egr2003)=="FECH_EGRE"] <- estandar
names(egr2004)[names(egr2004)=="FECHA_EG"] <- estandar
# 2005-2006 bien
names(egr2007)[names(egr2007)=="FEC_EGR"] <- estandar
# 2008-2017 bien

# Otras cosillas:
names(egr2005)[names(egr2005)=="SER_RES"] <- "ESTAB"
