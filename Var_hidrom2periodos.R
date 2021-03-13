##Variables hidrometerológicas del Atlántico dividido en dos periodos
#########1980-2000, 2001-2019#############
##Paquetes##
install.packages("e1071")
install.packages("ggpubr")
install.packages("pastecs") 
library(pastecs)
library(e1071)
library(tidyverse)
library(lubridate)
library(ggpubr)
file.choose()


###########Atlántico, día pluviométrico 1980-2000###################
Precipitaciones<-read.csv("G:\\Doctorado\\Investigación\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atlántico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones diarias 1980-2000.csv")
P80<-subset(Precipitaciones,select=c(CodigoEstacion, Municipio, Fecha, Valor))
##creando una variable con las fechas sin horas##
P80$Fecha<-as.Date(P80$Fecha)
##creando una variable con los meses##
P80$mes<-month(P80$Fecha,label=FALSE)
##creando una variable con los aÃ±os##
P80$Año<-year(P80$Fecha)
##Precipitaciones promedio mensuales, desviaciones 
##y totales pluviométricos 1980-2001##
P80<-P80 %>% 
  group_by(Municipio, Año, mes)%>%
  summarise(Promedio=mean(Valor, na.rm = TRUE), Desvia=sd(Valor, na.rm = TRUE),
            Total=sum(Valor, na.rm = TRUE))%>%
            ungroup

########Precipitaciones acumuladas mensual promedio, desviaciones####### 
###########y mediana 1980-2000####################

histcdc1<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==1)
Pre_ene1<-ggplot()+geom_density(data=histcdc1,aes(x=Total)) 
histcdc2<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==2)
Pre_feb1<-ggplot()+geom_density(data = histcdc2,aes(x=Total))
histcdc3<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==3)
Pre_mar1<-ggplot()+geom_density(data=histcdc3,aes(x=Total)) 
histcdc4<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==4)
Pre_abr1<-ggplot()+geom_density(data=histcdc4,aes(x=Total)) 
histcdc5<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==5)
Pre_may1<-ggplot()+geom_density(data=histcdc5,aes(x=Total)) 
histcdc6<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==6)
Pre_jun1<-ggplot()+geom_density(data=histcdc6,aes(x=Total)) 
histcdc7<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==7)
Pre_jul1<-ggplot()+geom_density(data=histcdc7,aes(x=Total)) 
histcdc8<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==8)
Pre_ago1<-ggplot()+geom_density(data=histcdc8,aes(x=Total)) 
histcdc9<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==9)
Pre_sep1<-ggplot()+geom_density(data=histcdc9,aes(x=Total)) 
histcdc10<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==10)
Pre_oct1<-ggplot()+geom_density(data=histcdc10,aes(x=Total)) 
histcdc11<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==11)
Pre_nov1<-ggplot()+geom_density(data=histcdc11,aes(x=Total)) 
histcdc12<-P80 %>%filter(Municipio=="Campo De La Cruz" & mes==12)
Pre_dic1<-ggplot()+geom_density(data=histcdc12,aes(x=Total)) 

Graf_pre1<-ggarrange(Pre_ene1, Pre_feb1, Pre_mar1, Pre_abr1, Pre_may1, 
                    Pre_jun1, Pre_jul1, Pre_ago1, Pre_sep1, Pre_oct1, 
                    Pre_nov1, Pre_dic1, ncol=4,nrow=3,
                    labels=c("Enero", "Febrero", "Marzo", "Abril", "Mayo",
                    "Junio", "Julio", "Agosto", "Septiembre", "Octubre", 
                    "Noviembre", "Diciembre"))
plot(Graf_pre1)


###############Atlántico, Día pluviométrico 2000-2020###################
Precipitaciones2<-read.csv("G:\\Doctorado\\Investigación\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atlántico\\Data\\Raw Datos\\Clima\\Precipitaciones Atlantico\\Precipitaciones 2000-2020.csv")
P20<-subset(Precipitaciones2,select=c(CodigoEstacion, Municipio, Fecha, Valor))
##creando una variable con las fechas sin horas##
P20$Fecha<-as.Date(Precipitaciones2$Fecha)
##creando una variable con los meses##
P20$Mes<-month(Precipitaciones2$Fecha,label=FALSE)
##creando una variable con los aÃ±os##
P20$Año<-year(Precipitaciones2$Fecha)

##Precipitaciones promedio mensuales, desviaciones 
##y totales pluviométricos 2000-2019##
P20<-P20 %>% 
  group_by(Municipio, Año, Mes)%>%
  summarise(Promedio=mean(Valor, na.rm = TRUE), 
             Desvia=sd(Valor, na.rm = TRUE), 
             Total=sum(Valor, na.rm = TRUE))%>%
              ungroup

##Histograma del mes de enero en Campo de la Cruz 2001-2019##
histcdc1<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==1)
Pre_ene<-ggplot()+geom_density(data=histcdc1,aes(x=Total)) 
histcdc2<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==2)
Pre_feb<-ggplot()+geom_density(data = histcdc2,aes(x=Total))
histcdc3<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==3)
Pre_mar<-ggplot()+geom_density(data=histcdc3,aes(x=Total)) 
histcdc4<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==4)
Pre_abr<-ggplot()+geom_density(data=histcdc4,aes(x=Total)) 
histcdc5<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==5)
Pre_may<-ggplot()+geom_density(data=histcdc5,aes(x=Total)) 
histcdc6<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==6)
Pre_jun<-ggplot()+geom_density(data=histcdc6,aes(x=Total)) 
histcdc7<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==7)
Pre_jul<-ggplot()+geom_density(data=histcdc7,aes(x=Total)) 
histcdc8<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==8)
Pre_ago<-ggplot()+geom_density(data=histcdc8,aes(x=Total)) 
histcdc9<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==9)
Pre_sep<-ggplot()+geom_density(data=histcdc9,aes(x=Total)) 
histcdc10<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==10)
Pre_oct<-ggplot()+geom_density(data=histcdc10,aes(x=Total)) 
histcdc11<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==11)
Pre_nov<-ggplot()+geom_density(data=histcdc11,aes(x=Total)) 
histcdc12<-P20 %>%filter(Municipio=="Campo De La Cruz" & Mes==12)
Pre_dic<-ggplot()+geom_density(data=histcdc12,aes(x=Total)) 

Graf_pre<-ggarrange(Pre_ene, Pre_feb, Pre_mar, Pre_abr, Pre_may, Pre_jun, 
                    Pre_jul, Pre_ago, Pre_sep, Pre_oct, Pre_nov, Pre_dic,
                    ncol=4,nrow=3,labels=c("Enero", "Febrero", "Marzo", "Abril",
                    "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", 
                    "Noviembre", "Diciembre"))

plot(Graf_pre)














###################################Borradores######################


save(Preci_prome2, file = "Precipitaciones_promediomes_desvia_totalmes 2001-2019.rda")

Preci_Acum<-Preci_prome %>% group_by(mes, Municipio) %>% 
  summarise(Prom_mes=mean(Promedio, na.rm = TRUE), Prom_Total=mean(Total, na.rm = TRUE), 
            Desvia=sd(Total, na.rm = TRUE), Mediana_total=median(Total, na.rm = TRUE)) %>% 
  ungroup
save(Preci_Acum, file = "Precipitaciones_promemens_prometotal_desvia_total_medianatotal 1980-2000.rda")

histcdc<-Preci_prome2 %>%filter(Municipio=="Campo De La Cruz")
Pre<-ggplot()+geom_density(data=histcdc,aes(x=Total)) 

plot(Pre)
summary(histcdc$Total)
(histcdc$Total)
stat.desc(histcdc$Total)
histcdc<-Preci_prome2 %>%filter(Municipio=="Sabanalarga")
Pre<-ggplot()+geom_density(data=histcdc,aes(x=Total)) 
stat.desc(histcdc$Total)
plot(Pre)
skewness(histcdc$Total)

histcdc<-Preci_prome2 %>%filter(Municipio=="Campo De La Cruz" & Mes)
ggplot()+geom_density(data=histcdc,aes(x=Total)) 
save(histcdc, file = "Histograma de Campo de la Cruz mayo 2001-2019.rda")
ggplot()+geom_jitter(data=histcdc,aes(x=Año, y=Total, color=F))



Graf_pre<-ggarrange(Pre_en, Pre_feb, Pre_mar, Pre_abr, 
                    ncol=2,nrow=2,labels=c("Enero", "Febrero", "Marzo", "Abril"))
plot(Graf_pre)

###########################################Descarte#############################################
##Precipitaciones acumuladas mensual promedio, desviaciones 
##y mediana 2001-2019##
Preci_Acum2<-Preci_prome2 %>% group_by(Mes, Municipio) %>% 
  summarise(Prom_mes=mean(Promedio, na.rm = TRUE), 
            Prom_Total=mean(Total, na.rm = TRUE), 
            Desvia=sd(Total, na.rm = TRUE), 
            Mediana_total=median(Total, na.rm = TRUE)) %>% 
  ungroup
save(Preci_Acum2, file = "Precipitaciones_promemesanual_prometotal_desvitotal_medianatotal 2001-2019.rda")


histcdc<-P8020m %>%filter(mes==1)  
histcdc<-histcdc%>%mutate(Iv= Desvia/85.200)
summary(P8020m$Total)
P8020m2<-P8020m%>%mutate(Iv=Desvia/567)
save(Preci_prome, file = "Precipitaciones_promemesanual_desvia_totalmesanual 1980-2000.rda")

##Precipitaciones acumuladas mensual promedio, desviaciones 
##y mediana 1980-2019##
Preci_Acum<-Preci_prome %>% group_by(mes, Municipio) %>% 
  summarise(Prom_mes=mean(Promedio, na.rm = TRUE), Prom_Total=mean(Total, na.rm = TRUE), 
            Desvia=sd(Total, na.rm = TRUE), Mediana_total=median(Total, na.rm = TRUE)) %>% 
  ungroup
save(Preci_Acum, file = "Precipitaciones_promemens_prometotal_desvia_total_medianatotal 1980-2000.rda")
