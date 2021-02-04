###https://www.kaggle.com/sazid28/advertising.csv
##1. Primero cargamos las librerias correspondientes para el proyecto
suppressWarnings(suppressMessages(library(reshape2)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(gridExtra)))

##2. Definimos la ruta del dataset

anuncios<-read.csv("Advertising.csv")

##3. Se quiere conocer la información que tenemos en el dataframe cargado, 
#por ello primero se elimina la primer columna que contiene los datos.

head(anuncios)
anuncios <- anuncios[,-1]
anuncios <- rename(anuncios, Ventas = sales, periodico = newspaper)

##4. Para conocer un poco más acerca de los datos haciendo un EDA y 
#observando sus características
str(anuncios); dim(anuncios)
head(anuncios); summary(anuncios)
View(anuncios)

##5. Se procede a realizar unas gráficas que permitan conocer la 
#distribución de los datos, conociendo su media y su desviación estandar

anuncios <- na.omit(anuncios) #esta de sobra  

###Medidas de tendencia central

media_ventas <- mean(anuncios$Ventas)
mediana_ventas <- median(anuncios$Ventas)

media_tv <- mean(anuncios$TV)
mediana_tv <- median(anuncios$TV)

media_radio <- mean(anuncios$radio)
mediana_radio <- median(anuncios$radio)

media_perio <- mean(anuncios$periodico)
mediana_perio <- median(anuncios$periodico)

##6. Y para conocer si exiten algunos outliers, se realiza un 
#boxplot de cada una de las variables

anuncios.m1 = melt(anuncios, id.vars = c("Ventas"),
                   measure.vars = c("TV", "radio", "periodico"))

anuncios.m1 %>%
  ggplot() +
  aes(x=variable, y=value) + 
  geom_boxplot(aes(fill=factor(variable))) +
  ylab("Presupuesto en Publicidad ($)") +
  xlab("") + 
  labs(fill = "Medio de comunicación")

## 7. histogramas

p1 <- anuncios %>% 
  ggplot() +
  aes(Ventas) + 
  geom_histogram( aes(y=..density..), binwidth = 2,
                  colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Ventas totales") +
  ylab("Frecuencia") +
  xlab("Ventas") + 
  theme_light() +
  geom_vline(xintercept = media_ventas, color = "red", size=0.5) +
  geom_label(aes(x = media_ventas, y = 0.115, 
                 label = media_ventas), fill = "#FF6666")

p2 <- anuncios %>%
  ggplot() +
  aes(TV) +
  geom_histogram( aes(y=..density..), binwidth = 20, 
                  colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Presupuesto en TV") +
  ylab("Frecuencia") +
  xlab("TV") + 
  theme_light() +
  geom_vline(xintercept = media_tv, color = "red", size=0.5) +
  geom_label(aes(x = media_tv, y = 0.0074, 
                 label = media_tv), fill = "#FF6666")

p3 <- anuncios %>%
  ggplot() +
  aes(radio) + 
  geom_histogram( aes(y=..density..), binwidth = 3,
                  colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Presupuesto en Radio") +
  ylab("Frecuencia") +
  xlab("Radio") + 
  theme_light()+
  geom_vline(xintercept = media_radio, color = "red", size=0.5)+
  geom_label(aes(x = media_radio, y = 0.04, 
                 label = media_radio), fill = "#FF6666")

p4 <- anuncios %>%
  ggplot() +
  aes(periodico) +
  geom_histogram( aes(y=..density..), binwidth = 7,
                  colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Presupuesto en Periodico") +
  ylab("Frecuencia") +
  xlab("Periodico") + 
  theme_light()+
  geom_vline(xintercept = media_perio, color = "red", size=0.5)+
  geom_label(aes(x = media_perio, y = 0.031, 
                 label = media_perio), fill = "#FF6666")

#multiplot
grid.arrange(p1, p2, p3, p4, nrow = 2)



##8. Antes de pensar en el modelado, es interesante observar 
#como se distribuyen los datos en una grafica con respecto a las ventas

p5 <- anuncios.m1 %>%
  ggplot()+ 
  aes(x=value,y=Ventas, color=variable) + 
  geom_point() 

p6 <- anuncios.m1 %>%
  ggplot() + 
  aes(x=value,y=Ventas, color=variable) + 
  geom_point()+
  labs(x='Presupuesto',y='Ventas',colour='Medio de comunicación')+
  facet_wrap("variable")

pairs(anuncios)
p5
p6


##9. Correlaciones entre los datos

anun.cor<- round(cor(anuncios), 3)

anun.cor1 <- melt(anun.cor, id.vars = c("Ventas"),
                  measure.vars = c("TV", "radio", "periodico"))

#heatmap de los coeficientes de correlacion

anun.cor1 %>%
  ggplot() +
  aes(x = Var1, y = Var2) +
  geom_tile(aes(fill = value), colour = "black") + 
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Coeficientes de correlación") +
  geom_text(aes(label=value),  vjust=-0.3, size=3.5) + 
  theme_minimal() +
  labs(x='',y='', fill='Coeficiente de Correlación') 

#10. Regresion lineal multiple

# partimos de una prueba de hipotesis donde 
#H0: beta3 = beta5 = beta6 = beta7 = 0
# es decir Ventas = beta0 + beta1*TV + beta2*radio + beta4*periodico + e (Reducido)
# contra
# H1: H0 no es verdad

attach(anuncios)
m1 <- lm(Ventas ~ TV + radio +  periodico)

summary(m1)
round(vcov(m1),8)
confint(m1,level=0.95) #Intervalos de confianza
predict.at=data.frame(
  TV=100, radio=30, periodico=40) #x0=(100,30,40)
predict(m1,newdata=predict.at,
        interval="prediction",level=0.95) #Pred Int for hat y0 given x0
anova(m1)

#como podemos observar, y a partir del heatmap, el coeficiente 
#de regresion para el numero de ventas por periodico 
#no es estadisticamente significativo por lo que ajustamos el modelo sin
#la variable mencionada anteriormente

m2 <- update(m1, ~.-periodico)
summary(m2)
