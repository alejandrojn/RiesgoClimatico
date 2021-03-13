install.packages("ggpubr")
##Paquetes##
library(tidyverse)
library(lubridate)
library(pastecs)
library(e1071)
library(ggpubr)
file.choose()

##############Datos Ideam, Variables hidrometerológicas del########### 
###########departamento del Atlántico##################

###########Atlántico, día pluviométrico 1980-2000###########
Precipitaciones<-read.csv("G:\\Doctorado\\Investigación\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atlántico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones diarias 1980-2000.csv")
P80<-subset(Precipitaciones,select=c(CodigoEstacion, Municipio, Fecha, Valor))

#############Atlántico, Día pluviométrico 2000-2020#########
Precipitaciones2<-read.csv("G:\\Doctorado\\Investigación\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atlántico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones 2000-2020.csv")
P20<-subset(Precipitaciones2, select = c(CodigoEstacion, Municipio, Fecha, Valor))

########Integrando los dataframe 1980-2000 y 2001-2020############
P8020<-rbind(P80,P20)

##########creando una variable con las fechas sin horas##########
P8020$Fecha<-as.Date(P8020$Fecha)

##############creando una variable con los meses#############
P8020$mes<-month(P8020$Fecha,label=FALSE)

##############creando una variable con los aÃ±os##############
P8020$Año<-year(P8020$Fecha)

#############Precipitaciones promedio mensuales, desviaciones########### 
#############y totales pluviométricos 1980-2019##############
  P8020m<-P8020 %>% 
  group_by(Municipio, Año, mes)%>%
  summarise(Promedio=mean(Valor, na.rm = TRUE), Desvia=sd(Valor, na.rm = TRUE),
            Total=sum(Valor, na.rm = TRUE))%>%
            ungroup
############Filtrando a Campo de la Cruz y gráficando la frecuencia####### 
###########de la serie pluviométrica mensual de todo el peridodo#########  
  histcdc1<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==1)
  Pre_ene1<-ggplot()+geom_density(data=histcdc1,aes(x=Total)) 
  histcdc2<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==2)
  Pre_feb1<-ggplot()+geom_density(data = histcdc2,aes(x=Total))
  histcdc3<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==3)
  Pre_mar1<-ggplot()+geom_density(data=histcdc3,aes(x=Total)) 
  histcdc4<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==4)
  Pre_abr1<-ggplot()+geom_density(data=histcdc4,aes(x=Total)) 
  histcdc5<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==5)
  Pre_may1<-ggplot()+geom_density(data=histcdc5,aes(x=Total)) 
  histcdc6<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==6)
  Pre_jun1<-ggplot()+geom_density(data=histcdc6,aes(x=Total)) 
  histcdc7<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==7)
  Pre_jul1<-ggplot()+geom_density(data=histcdc7,aes(x=Total)) 
  histcdc8<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==8)
  Pre_ago1<-ggplot()+geom_density(data=histcdc8,aes(x=Total)) 
  histcdc9<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==9)
  Pre_sep1<-ggplot()+geom_density(data=histcdc9,aes(x=Total)) 
  histcdc10<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==10)
  Pre_oct1<-ggplot()+geom_density(data=histcdc10,aes(x=Total)) 
  histcdc11<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==11)
  Pre_nov1<-ggplot()+geom_density(data=histcdc11,aes(x=Total)) 
  histcdc12<-P8020m %>%filter(Municipio=="Campo De La Cruz" & mes==12)
  Pre_dic1<-ggplot()+geom_density(data=histcdc12,aes(x=Total)) 
  
  Graf_pre1<-ggarrange(Pre_ene1, Pre_feb1, Pre_mar1, Pre_abr1, Pre_may1, 
                  Pre_jun1, Pre_jul1, Pre_ago1, Pre_sep1, Pre_oct1, 
                   Pre_nov1, Pre_dic1, ncol=4,nrow=3,
                   labels=c("Enero", "Febrero", "Marzo", "Abril", "Mayo",
                    "Junio", "Julio", "Agosto", "Septiembre", "Octubre", 
                    "Noviembre", "Diciembre"))
plot(Graf_pre1)
view(Pre_oct1)
  



