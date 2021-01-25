###https://www.kaggle.com/sazid28/advertising.csv
##1. Primero cargamos las librerias correspondientes para el proyecto
library(reshape2)
library(ggplot2)
library(dplyr)

##2. Definimos la ruta del dataset
setwd("D:/documentos/Santander bedu/Fase 2/Repositorio/Evidencia/Programación y estadística con R Evidencias/Proyecto")
anuncios<-read.csv("data/Advertising.csv")

##3. Se quiere conocer la información que tenemos en el dataframe cargado, por ello primero se elimina la primer columna que contiene los datos.
head(anuncios)
anuncios<-anuncios[,-1]
anuncios<-rename(anuncios, Ventas=sales, periodico=newspaper)
##4. Para conocer un poco más acerca de los datos haciendo un EDA y observando sus características
str(anuncios); dim(anuncios)
head(anuncios); summary(anuncios)
View(anuncios)

##5. Se procede a realizar unas gráficas que permitan conocer la distribución de los datos, conociendo su media y su desviación estandar
anuncios <- na.omit(anuncios) 

###Medidas de tendencia central

media_tv<-mean(anuncios$TV)
mediana_tv <- median(anuncios$TV)

media_radio<-mean(anuncios$radio)
mediana_radio <- median(anuncios$radio)

media_perio<-mean(anuncios$periodico)
mediana_perio <- median(anuncios$periodico)


###histogramas
ggplot(anuncios, aes(TV)) +
  geom_histogram(binwidth = 5, col="black", fill = "lightgreen") + 
  ggtitle("Inversión en TV") +
  ylab("Frecuencia") +
  xlab("TV") + 
  theme_light()+
  geom_vline(xintercept = media_tv, color = "red", size=2)+
  geom_label(aes(x = 70, y = 8.3, label = paste("La media de los datos esta en: ", media_tv)), fill = "green")



ggplot(anuncios, aes(radio)) +
  geom_histogram(binwidth = 5, col="black", fill = "lightgreen") + 
  ggtitle("Inversión en TV") +
  ylab("Frecuencia") +
  xlab("TV") + 
  theme_light()+
  geom_vline(xintercept = media_radio, color = "red", size=2)+
  geom_label(aes(x = 40, y = 24, label = paste("La media de los datos esta en: ", media_radio)), fill = "green")

ggplot(anuncios, aes(periodico)) +
  geom_histogram(binwidth = 5, col="black", fill = "lightgreen") + 
  ggtitle("Inversión en TV") +
  ylab("Frecuencia") +
  xlab("TV") + 
  theme_light()+
  geom_vline(xintercept = media_perio, color = "red", size=2)+
  geom_label(aes(x = 90, y = 24, label = paste("La media de los datos esta en: ", media_perio)), fill = "green")

##Antes de pensar en el modelado, es interesante observar como se distribuyen los datos en una grafica con respecto a las ventas

anuncios.m1 = melt(anuncios, id.vars = c("Ventas"),
             measure.vars = c("TV", "radio", "periodico"))

ggplot(anuncios.m1,aes(x=value,y=Ventas, color=variable))+ geom_point() 

ggplot(anuncios.m1,aes(x=value,y=Ventas, color=variable)) + geom_point()+
  labs(x='Inversión',y='Ventas',colour='Medio de comunicación')+
  facet_wrap("variable")

##6. Y para conocer si exiten algunos outliers, se realiza un boxplot de cada una de las variables
ggplot(anuncios.m1, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=factor(variable)))+
  labs(fill = "Medio de comunicación")

##7. 

