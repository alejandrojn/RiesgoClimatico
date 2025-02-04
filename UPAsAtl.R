##########Datos del Censo Nacional Agropecuario 2014#####################
##########Categor�a de �rea de los usos del suelo y del acceso###########
##########a las fuentes de agua para la actividad ganadera###############

##Paquetes##
library(tidyverse)
library(lubridate)
library(pastecs)
library(e1071)
library(ggpubr)
library(openxlsx)
file.choose()

########Participaci�n del area cultivada por categor�as para cada UPA#####
########en el Atl�ntico###################################################
##1- Cargando la base de datos, caracterización de las Unidades Productivas Agropecuarias Depto Atlántico##
UPAs <- read.csv("G:\\Doctorado\\Investigaci�n\\Borradores y presentaciones\\CNA2014\\Microdatos\\Atl�ntico\\Data\\Raw Datos\\CNA\\CNA2014_ENCABEZADO_08 UPA.csv")

##2- Reduciendo el dataframe##
Tab1<-data.frame(UPAs$P_MUNIC, UPAs$P_S5PAUTOS, UPAs$P_S12P150A,
        UPAs$P_S11P130_SP1, 
        UPAs$P_S12P142, UPAs$P_S12P143, UPAs$P_S12P144, 
        UPAs$P_S12P145, UPAs$P_S12P146, UPAs$P_S12P147, 
        UPAs$P_S12P148, UPAs$P_S12P149, UPAs$P_S11P124_SP1,
        UPAs$P_S11P124_SP2, UPAs$P_S11P124_SP3, 
        UPAs$P_S11P124_SP4, UPAs$P_S11P124_SP5, 
        UPAs$P_S11P124_SP6, UPAs$P_S11P124_SP7,
        UPAs$P_S11P124_SP8, UPAs$P_S11P124_SP8,
        UPAs$P_S11P124_SP9, UPAs$P_S11P124_SP10,
        UPAs$P_S11P124_SP11, UPAs$P_S11P125_SP1, 
        UPAs$P_S11P125_SP2, UPAs$P_S11P125_SP3,
        UPAs$P_S11P125_SP4, UPAs$P_S11P125_SP5,
        UPAs$P_S11P125_SP6, UPAs$P_S11P125_SP7,
        UPAs$P_S11P125_SP8, UPAs$P_S11P125_SP9,
        UPAs$P_S11P125_SP10, UPAs$P_S11P125_SP11,
        UPAs$P_S11P125_SP12, UPAs$P_S11P126_SP1,
        UPAs$P_S11P126_SP2, UPAs$P_S11P126_SP3, 
        UPAs$P_S11P126_SP4, UPAs$P_S11P126_SP5,
        UPAs$P_S11P126_SP6, UPAs$P_S11P126_SP7,
        UPAs$P_S11P126_SP8, UPAs$P_S11P126_SP9,
        UPAs$P_S7P79_SP1, UPAs$P_S7P79_SP2, 
        UPAs$P_S7P85B, UPAs$P_S6P67, UPAs$P_S6P68,
        UPAs$P_S12P150A, UPAs$P_S7P83A, UPAs$P_S7P83B,
        UPAs$P_S7P83C, UPAs$P_S7P83D, UPAs$P_S7P83E,
        UPAs$P_S7P83F, UPAs$P_S7P84A, UPAs$P_S7P84B,
        UPAs$P_S7P84C, UPAs$P_S7P84D, UPAs$P_S7P84E,
        UPAs$P_S7P84F, UPAs$P_S6P52_SPA02, UPAs$P_S6P52_SPA04,
        UPAs$P_S6P52_SPC20, UPAs$P_S6P52_SPC21, UPAs$P_S6P52_SPC22,
        UPAs$P_S6P52_SP99, UPAs$P_S11P127_SP1, UPAs$P_S11P127_SP2,
        UPAs$P_S11P127_SP3, UPAs$P_S11P127_SP4, UPAs$P_S11P127_SP5,
        UPAs$P_S11P127_SP6, UPAs$P_S11P127_SP7, UPAs$P_S11P127_SP8,
        UPAs$P_S11P127_SP9, UPAs$P_S11P127_SP10, UPAs$P_S11P127_SP11,
        UPAs$P_S6P76_SP1, UPAs$P_S6P76_SP2, UPAs$P_S6P76_SP3, UPAs$P_S6P76_SP4,
        UPAs$P_S6P76_SP5, UPAs$P_S6P76_SP6, UPAs$P_S6P76_SP7, UPAs$P_S6P76_SP8,
        UPAs$P_S9P117, UPAs$P_S11P136, UPAs$P_S11P136A, UPAs$P_S10P121, UPAs$P_S12P147, UPAs$P_S11P133_SP1,
        UPAs$P_S11P133_SP2, UPAs$P_S11P133_SP3, UPAs$P_S11P133_SP4, UPAs$P_S11P133_SP5,
        UPAs$P_S11P133_SP6, UPAs$P_S11P133_SP7, UPAs$P_S11P133_SP8,
        UPAs$P_S11P133_SP9, UPAs$P_S11P133_SP10, UPAs$SNH, UPAs$SNM,
        UPAs$SN9, UPAs$P_S11P134_SP1, UPAs$P_S11P134_SP2, UPAs$P_S11P134_SP3,
        UPAs$P_S11P134_SP4, UPAs$P_S11P134_SP5, UPAs$P_S11P134_SP6,
        UPAs$P_S11P134_SP7, UPAs$P_S11P135, UPAs$P_S11P135A_SP1,
        UPAs$P_S11P135A_SP2, UPAs$P_S11P135A_SP3, UPAs$P_S11P135A_SP4,
        UPAs$P_S11P135A_SP5, UPAs$P_S11P135A_SP6, UPAs$P_S11P135A_SP7,
        UPAs$P_S11P135A_SP8, UPAs$P_S11P135A_SP9, UPAs$P_S11P135A_SP10,
        UPAs$P_S11P135A_SP11)

#################3- Filtrando el dataframe por codigos municipales######## 
#################Depto Atlantico######################
Tab2<-Tab1 %>% filter(UPAs.P_MUNIC %in% c("8137", 
                      "8141", "8436", "8606", "8638", "8675", "8770"))


###################Calculando el porcentaje de participaci�n########### 
###################de las UPAs por Municipio###################
table(Tab2A$Municipio)/length(Tab2A$Municipio)

################Filtrando por areas de cultivo################## 
Tab2A<-data.frame(Tab2$UPAs.P_MUNIC, Tab2$UPAs.P_S5PAUTOS, 
                  Tab2$UPAs.P_S12P142, Tab2$UPAs.P_S12P143, 
                  Tab2$UPAs.P_S12P145, Tab2$UPAs.P_S12P146, 
                  Tab2$UPAs.P_S12P149)

#############Renombrando los c�digos por Municipio#############
Tab2A$Tab2.UPAs.P_MUNIC<-factor(Tab2A$Tab2.UPAs.P_MUNIC, levels = c("8137", "8141", 
              "8436", "8606", "8638", "8675", "8770"), labels = c("Campo de la Cruz",
              "Candelaria", "Manat�", "Repelon", "Sabanalarga", "Santa Lucia", 
              "Suan"))

###################Renombrando las variables####################
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_MUNIC"]<-"Municipio"
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_S5PAUTOS"]<-"Area_UPA"
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_S12P149"]<-"Cuencas"
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_S12P146"]<-"Bosques"
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_S12P145"]<-"Rastrojos"
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_S12P143"]<-"Barbecho"
names(Tab2A)[names(Tab2A)=="Tab2.UPAs.P_S12P142"]<-"Area_Siem"

##############Calculando las areas totales por categor�as##############
Tab2A$TotalArea <- rowSums(Tab2A[ ,4:7])

############Calculando la participaci�n por categor�as###############
Tab2A <- Tab2A %>% mutate(PartTotArea=TotalArea/Area_UPA)
Tab2A <- Tab2A %>% mutate(PartArea=Area_Siem/Area_UPA)
Tab2A <- Tab2A %>% mutate(PartBarb=Barbecho/Area_UPA)
Tab2A <- Tab2A %>% mutate(PartRast=Rastrojos/Area_UPA)
Tab2A <- Tab2A %>% mutate(PartBosq=Bosques/Area_UPA)
Tab2A <- Tab2A %>% mutate(PartCuen=Cuencas/Area_UPA)

#####Participaci�n del uso por categor�as de las fuentes de aguas######### 
#####para cada UPA del depto del Atl�ntico##############

################Filtrando las fuentes de agua por categor�a############
Tab3 <- data.frame(Tab2$UPAs.P_MUNIC, Tab2$UPAs.P_S11P124_SP1, Tab2$UPAs.P_S11P124_SP2, 
                    Tab2$UPAs.P_S11P124_SP3, Tab2$UPAs.P_S11P124_SP4, 
                   Tab2$UPAs.P_S11P124_SP5)

#############Renombrando los c�digos por Municipio#############
Tab3$Tab2.UPAs.P_MUNIC <-factor(Tab3$Tab2.UPAs.P_MUNIC, levels = c("8137", 
                       "8141", "8436", "8606", "8638", "8675", "8770"), 
                       labels = c("Campo de la Cruz","Candelaria", 
                     "Manat�", "Repelon", "Sabanalarga", "Santa Lucia",
                     "Suan"))
###################Renombrando las variables####################
names(Tab3)[names(Tab3)=="Tab2.UPAs.P_S11P124_SP1"] <- "Lluvia"
names(Tab3)[names(Tab3)=="Tab2.UPAs.P_S11P124_SP2"] <- "Rio_queb"
names(Tab3)[names(Tab3)=="Tab2.UPAs.P_S11P124_SP3"] <- "Lago"
names(Tab3)[names(Tab3)=="Tab2.UPAs.P_S11P124_SP4"] <- "Cienaga"
names(Tab3)[names(Tab3)=="Tab2.UPAs.P_S11P124_SP5"] <- "Embalse"
names(Tab3)[names(Tab3)=="Tab2.UPAs.P_MUNIC"] <- "Municipio"

##########Cambiando los datos nulos por ceros###########
Tab3$Lluvia<- ifelse(is.na(Tab3$Lluvia),0,1)
Tab3$Rio_queb<- ifelse(is.na(Tab3$Rio_queb),0,1)
Tab3$Lago<- ifelse(is.na(Tab3$Lago),0,1)
Tab3$Cienaga<- ifelse(is.na(Tab3$Cienaga),0,1)
Tab3$Embalse<- ifelse(is.na(Tab3$Embalse),0,1)

#########Sumando las filas########################
Tab3$TotFuentes<-rowSums(Tab3[ ,3:6])

########Grado de sensibilidad, fuentes de agua para la actividad#######
########ganadera###################################
Tab3$GS<-(1-Tab3$TotFuentes/4)















#################################Borradores########################################


###Sumando datos nulos##
sapply(Tab3, function(x) sum(is.na(x)))
##Eliminando datos nulos###
Tab3 <- Tab3[!is.na(Tab3$Rio_queb) ,]

#####Cambiando los datos nulos por ceros######
Tab3$Lluvia<- factor(Tab3$Lluvia, levels = c("1", "NA"), 
                     labels = c("1", "0")) 

#####Matriz de 5x5 con las fuentes de agua################
x=matrix(c(Tab3$Lluvia, Tab3$Rio_queb, Tab3$Lago, Tab3$Ci�naga,
           Tab3$Embalse), ncol = 5, nrow = 5)

colnames(x) = c("Lluvia", "Rio_queb", "Lago", "Cienaga", "Embalse")
row.names(x)= c("Lluvia", "Rio_queb", "Lago", "Cienaga", "Embalse")


###Organizando las variables de las fuentes de agua#######

##4- Organizando todas las upas doble propósito##
Tab2D<-Tab2 %>% arrange(UPAs.P_S7P79_SP1==0)
##5- Organizando por código, área y leche depto Atlántico##
Tab2L<-Tab2D %>% arrange(UPAs.P_S7P79_SP2==0)

Tab3 <-data.frame(Tab2$P_S11P124_SP1, Tab2$P_S11P124_SP2, Tab2$P_S11P124_SP3,
                   Tab2$P_S11P124_SP4, Tab2$P_S11P124_SP5) 



##5- Eliminando la columna de leche##
Tab_cod_dob<-Tabr2f2[ ,-c(4)]
##6- Eliminando las filas con cero de doble##
Tab_Co_A_Do<-Tab_cod_dob[-c(2810:8388), ]

##8- Eliminando la columna de leche##
Tab_cod_Le<-Tabr2f2[ ,-c(3)]
##9- Eliminando las filas con cero de leche##
Tab_Co_A_Le<-Tab_cod_Le2[-c(122:8388), ]

##CNA2014 Córdoba##
##1- Cargando la base de datos, caracterización de las Unidades Productivas Agropecuarias Depto Córdoba##
UPAs23 <- read.csv("Data/Raw Datos/CNA2014_Encabezado_23 UPA.csv")
##2- Reduciendo el dataframe##
TablarC<-data.frame(UPAs23$P_MUNIC, UPAs23$P_S5PAUTOS, UPAs23$P_S7P79_SP1, UPAs23$P_S7P79_SP2)
##3- Filtrando el dataframe por códigos municipales de interés Atlántico##
TablarCf<-TablarC %>% filter(UPAs23.P_MUNIC %in% c("23168", "23001", "23555", "23586", "23686"))
##4- Organizando todas las upas doble propósito##
TablarCf2<-TablarCf %>% arrange(UPAs23.P_S7P79_SP1==0)
##5- Eliminando la columna de leche##
TablarCf3<-TablarCf2[ ,-c(4)]
##6- Eliminando las filas con cero de doble##
Tab_Co_A_Do_C<- TablarCf3[-c(4383:23062), ]
##7- Eliminando columna de doble y organizando las Upas=1##
TablarCF2<- TablarCf %>% arrange(UPAs23.P_S7P79_SP2==0)
Tab_Co_A_Le_C<-TablarCF2[ ,-c(3)]
##8- Eliminando las filas con ceros##
Tab_Co_A_Le_C2<-Tab_Co_A_Le_C[-c(90:23062), ]
##9- Tomando vectores de doble propósito y leche en Córdoba##
Tabla_Do_C<-table(Tab_Co_A_Do_C$UPAs23.P_MUNIC, Tab_Co_A_Do_C$UPAs23.P_S7P79_SP1)
Tabla_Le_C<-table(Tab_Co_A_Le_C2$UPAs23.P_MUNIC, Tab_Co_A_Le_C2$UPAs23.P_S7P79_SP2)



##Explorando la base de datos##
head(UPAs)
View(UPAs)
str(UPAs)
names(UPAs)

##Análisis descriptivo, frecuencias relativas##
table(UPAs$ENCUESTA)
table(UPAs$P_S7P78)
table(UPAs$P_S7P78)
prop.table(table(UPAs$P_S7P78))
table(UPAs$P_S7P79_SP1)
prop.table(table(UPAs$P_S7P79_SP1))
table(UPAs$P_S7P78, UPAs$P_S7P79_SP1)
table(UPAs$P_S7P78)
table(UPAs$P_S7P78, UPAs$P_S7P82)
prop.table(table(UPAs$P_S7P82, UPAs$P_S7P78))
table(UPAs$P_S9P117)
table(Tab1$SNH)
table(Tab1$SNM)
prop.table(table(Tab1$SNH, Tab1$SNM))

##Cruzando dos variables categóricas en forma de vectores##
Tab1<-table(UPAs$P_MUNIC, UPAs$P_S7P79_SP1, UPAs$P_S7P79_SP2)

##Total de observaciones##
margin.table(Tab1a5, 1) #por filas 
margin.table(Tab1a5, 2)#por columnas

##Porcentajes por celdas, rows and columns##
round(prop.table(Tab1a5), 2)##%celdas##
round(prop.table(Tab1a5, 1), 2)##%row
round(prop.table(Tab1a5, 2), 2)##% columns

##Análisis descriptivo variables numéricas##
summary(UPAs$P_S7P78)
summary(UPAs)
summary(Tab1$Area_tot_Upa)
summary(Tab1$SNH)

##Cargando base de datos, caracterización de los cultivos##
Cultivos <- read.csv("Data/Raw Datos/CNA2014_S6CUL_2013_08 cultivos.csv")
View(Cultivos)
str(Cultivos)
names(Cultivos)


##Cargando base de datos, caracterización de árboles frutales y forestales##
Frut_Fores_disp <- read.csv("Data/Raw Datos/CNA2014_S6FV_08 frutales y forestales dispersos.csv")

##Cargando base de datos, caracterización construcciones de uso agropecuario##
Construc_uso_agro <- read.csv("Data/Raw Datos/CNA2014_S10_08 construcciones uso agro.csv")

##Cargando base de datos, caracterización maquinaria y equipos##
Maqui_equipo <- read.csv("Data/Raw Datos/CNA2014_S9_08 maquinaria y equipos.csv")

##Cargando base de datos, caracterización hogares##
Hogares <- read.csv("Data/Raw Datos/CNA2014_S15H_08 Hogares.csv")
table(Hogares$P_S15P177)
tab1 <- prop.table(table(Hogares$P_S15P177))
View(tab1)


##Cargando base de datos, caracterización vivienda##
Vivienda <- read.csv("Data/Raw Datos/CNA2014_S15V_08 Vivienda.csv")
table(Vivienda$P_S15P162)
prop.table(table(Vivienda$P_S15P162))


##Cargando base de datos, caracterización personas##
Personas <- read.csv("Data/Raw Datos/CNA2014_S15P_08 Personas.csv")


##Pendiente##
library(tableone)
CreateTableOne(data = UPAs)
print(UPAs, showAlllevels = T)
##Paquete para borrar columnas##
library(dplyr)
##Manipulación de datos#

## Eliminar columnas##
Tab1 <- UPAs[ ,-c(1, 2, 3, 4)]
Tab1 <- Tab1[ ,-c(1, 2)]
Tab1 <- Tab1[ ,-c(2)]
Tab1 <- Tab1[ ,-c(4)]
Tab1 <- Tab1[ ,-c(7)]
Tab1 <- Tab1[ ,-c(11)]
Tab1 <- Tab1[ ,-c(11:14)]

##Nombra variables##
names(Tab1)[names(Tab1)=="P_S3P9"] <- "Cul_autocon"
names(Tab1)[names(Tab1)=="P_S3P11"] <- "Plan_Bos"
names(Tablamun)[names(Tablamun)=="Var2"]<-"Area_Upa"