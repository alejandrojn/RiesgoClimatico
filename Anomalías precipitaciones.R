##########Anomal�as de las precip�taciones por municipios############ 
##########en el dpto del Atl�ntico 1980-2019#############
###########�ndice de vulnerabilidad para las desviaciones de las####### 
###########precipitaciones y de las frecuancias en las anomal�as#######
install.packages("openxlsx")
library(openxlsx)
library(tidyverse)
library(lubridate)
install.packages("ggpubr")
library(ggpubr)
file.choose()
#####Atl�ntico, d�a pluviom�trico 1980-2000##########
Precipitaciones<-read.csv("G:\\Doctorado\\Investigaci�n\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atl�ntico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones diarias 1980-2000.csv")
#####Atl�ntico, D�a pluviom�trico 2001-2019#########
Precipitaciones2<-read.csv("G:\\Doctorado\\Investigaci�n\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atl�ntico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones 2000-2020.csv")
#####Seleccionando las vcariables de inter�s 1980-2000#######
P80<-subset(Precipitaciones,select=c(CodigoEstacion, Municipio, Fecha, Valor))
#####Seleccionando las vcariables de inter�s 2001-2019##########
P20<-subset(Precipitaciones2, select = c(CodigoEstacion, Municipio, Fecha, Valor))
#####Acoplando las dataframe 1980-2000 y 2001-2019############
P8020<-rbind(P80,P20)
#######creando una variable con las fechas sin horas###########
P8020$Fecha<-as.Date(P8020$Fecha)
########creando una variable con los meses#####
P8020$mes<-month(P8020$Fecha,label = FALSE)
########creando una variable con los ños########
P8020$A�o<-year(P8020$Fecha)

###########Precipitaciones promedio mensuales, desviaciones############ 
###########y totales pluviom�tricos 1980-2001######################
P8020m<-P8020 %>% 
  group_by(Municipio, A�o, mes)%>%
  summarise(promedio=mean(Valor, na.rm = TRUE), desvia=sd(Valor, na.rm = TRUE),
            total=sum(Valor, na.rm = TRUE))%>%
  ungroup

#############Promedio mensual multianual para cada municipio#############
P8020m<-P8020m%>% 
  group_by(Municipio, mes)%>%
  mutate(prom8020=mean(total, na.rm = TRUE))%>%ungroup

#################Valor porcentual de las anomal�as###############
P8020m$desprom<-P8020m$total/P8020m$prom8020

#########Umbrales de normalidad para las anomal�as entre 60-120% 
##########seg�n especificaciones del Ideam###############
P8020m$catnor[P8020m$desprom<0.6]=1
P8020m$catnor[P8020m$desprom>0.6&P8020m$desprom<1.2]=2
P8020m$catnor[P8020m$desprom>1.2]=3

##########Desviaci�n del promedio mensual de precipitaciones 1980-2019######
P8020sd<-P8020m%>% 
  group_by(Municipio)%>%
  summarise(desviacion=sd(promedio, na.rm = TRUE))%>%ungroup

max(P8020sd$desviacion)
min(P8020sd$desviacion)

########Calculando el �ndice de vulnerabilidad para las desviaciones#######
######## del promedio total anual periodo 1980-2019##############################
P8020sd$Iv<-(P8020sd$desviacion-2.278352)/(3.103442-2.278352)

###########Frecuencia de las anomal�as, d�ficit, normal y exceso##########
Tab1<-table(P8020m$Municipio, P8020m$catnor)
view(Tab1)
FA<-data.frame(Tab1)
max(FA$Freq)
min(FA$Freq)
names(FA)[names(FA)=="Var1"]<-"Municipio"
names(FA)[names(FA)=="Var2"]<-"Anomal�a"

######�ndice de vulnerabilidad para la frecuencia de las anomal�as########
######de las precipitaciones 1980-2019#######################
FA$Iv<-(FA$Freq-116)/(182-116)













##########################Borradores#################################



#######Ecuaci�n para la estandarizaci�n de las variables############ 
#######para cada componente#############################

Iv<-function(Ia, Imax, Imin){(Ia-Imin)/(Imax-Imin)}

#######Convertir un data frame a un archivo en excel###############
write.xlsx(Tab1, "frecuencia_anomalia.xlsx")
write.xlsx(P8020sd, "desviaciones_anomalias.xlsx")



histcdc<-P8020m %>%filter(mes==1)
summary(histcdc$total)
histcdc<-histcdc%>%mutate(Iv=desvia/85.200)
summary(P8020m$total)
P8020m<-P8020m%>%mutate(Iv=desvia/567)