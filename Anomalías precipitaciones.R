##########Anomalías de las precipítaciones por municipios############ 
##########en el dpto del Atlántico 1980-2019#############
###########Índice de vulnerabilidad para las desviaciones de las####### 
###########precipitaciones y de las frecuancias en las anomalías#######
install.packages("openxlsx")
library(openxlsx)
library(tidyverse)
library(lubridate)
install.packages("ggpubr")
library(ggpubr)
file.choose()
#####Atlántico, día pluviométrico 1980-2000##########
Precipitaciones<-read.csv("G:\\Doctorado\\Investigación\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atlántico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones diarias 1980-2000.csv")
#####Atlántico, Día pluviométrico 2001-2019#########
Precipitaciones2<-read.csv("G:\\Doctorado\\Investigación\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atlántico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones 2000-2020.csv")
#####Seleccionando las vcariables de interés 1980-2000#######
P80<-subset(Precipitaciones,select=c(CodigoEstacion, Municipio, Fecha, Valor))
#####Seleccionando las vcariables de interés 2001-2019##########
P20<-subset(Precipitaciones2, select = c(CodigoEstacion, Municipio, Fecha, Valor))
#####Acoplando las dataframe 1980-2000 y 2001-2019############
P8020<-rbind(P80,P20)
#######creando una variable con las fechas sin horas###########
P8020$Fecha<-as.Date(P8020$Fecha)
########creando una variable con los meses#####
P8020$mes<-month(P8020$Fecha,label = FALSE)
########creando una variable con los Ã±os########
P8020$Año<-year(P8020$Fecha)

###########Precipitaciones promedio mensuales, desviaciones############ 
###########y totales pluviométricos 1980-2001######################
P8020m<-P8020 %>% 
  group_by(Municipio, Año, mes)%>%
  summarise(promedio=mean(Valor, na.rm = TRUE), desvia=sd(Valor, na.rm = TRUE),
            total=sum(Valor, na.rm = TRUE))%>%
  ungroup

#############Promedio mensual multianual para cada municipio#############
P8020m<-P8020m%>% 
  group_by(Municipio, mes)%>%
  mutate(prom8020=mean(total, na.rm = TRUE))%>%ungroup

#################Valor porcentual de las anomalías###############
P8020m$desprom<-P8020m$total/P8020m$prom8020

#########Umbrales de normalidad para las anomalías entre 60-120% 
##########según especificaciones del Ideam###############
P8020m$catnor[P8020m$desprom<0.6]=1
P8020m$catnor[P8020m$desprom>0.6&P8020m$desprom<1.2]=2
P8020m$catnor[P8020m$desprom>1.2]=3

##########Desviación del promedio mensual de precipitaciones 1980-2019######
P8020sd<-P8020m%>% 
  group_by(Municipio)%>%
  summarise(desviacion=sd(promedio, na.rm = TRUE))%>%ungroup

max(P8020sd$desviacion)
min(P8020sd$desviacion)

########Calculando el índice de vulnerabilidad para las desviaciones#######
######## del promedio total anual periodo 1980-2019##############################
P8020sd$Iv<-(P8020sd$desviacion-2.278352)/(3.103442-2.278352)

###########Frecuencia de las anomalías, déficit, normal y exceso##########
Tab1<-table(P8020m$Municipio, P8020m$catnor)
view(Tab1)
FA<-data.frame(Tab1)
max(FA$Freq)
min(FA$Freq)
names(FA)[names(FA)=="Var1"]<-"Municipio"
names(FA)[names(FA)=="Var2"]<-"Anomalía"

######Índice de vulnerabilidad para la frecuencia de las anomalías########
######de las precipitaciones 1980-2019#######################
FA$Iv<-(FA$Freq-116)/(182-116)













##########################Borradores#################################



#######Ecuación para la estandarización de las variables############ 
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