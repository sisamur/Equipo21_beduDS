###Al revisar el archivo data, se observa que no se tienen datos anteriores a las fechas del 2017, sin embargo, en la direccion https://www.football-data.co.uk/spainm.php
###se encuentran los datos de temporadas anteriores, por lo que se procede primeramente a adquirir los datos correspondientes a las temporadas 15-16 y 16-17
###y realziar el procesamiento correspondiente.

library(dplyr)

setwd("D:documentos/Santander bedu/Fase 2/Repositorio/Evidencia/Programación y estadística con R Evidencias/Sesión 7/Postwork")
T15_16<-read.csv("data/SP1_15-16.csv")
T16_17<-read.csv("data/SP1_16-17.csv")

lista <- list(T15_16, T16_17)
nlista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

nlista[[1]] <- mutate(nlista[[1]], Date = as.Date(Date, "%d/%m/%y"))
nlista[[2]] <- mutate(nlista[[2]], Date = as.Date(Date, "%d/%m/%y"))
data <- do.call(rbind, nlista)



###
install.packages("mongolite")

###Acorde a las instrucciones, se subio el archivo desde el manejador de compass

##Utilizando la libreria mongolite y con la url de donde se alojo la base de datos en el compass se lee la colaccion
library(mongolite)
m <- mongo("match", url = "mongodb+srv://bedu_p7:NEC@cluster0.wogir.mongodb.net/match_games")

###Esta linea la ejecutamos para agregar el conjunto de datos creado anteriormente, coniderando que no se encontraban los años previos al 2017,
###se realiza un insert con los nuevos datos, directos al cliente de mongo, ya no es necesario volver a ejecutar, dado que ya se subieron esos datos,
###a diferencia del primer conjunto de datos, agregados por compass, ahora lo hacemos por medio de un insert
m$insert(data)

## HAcemos un find general y observamos los datos que se leen
alldata <- m$find('{}')
print(alldata)

##Se hace un count para conocer la cantidad de registros que se tienen
Cantidad<-m$count('{}')
paste("La cantidad de registros son:",Cantidad)

#Se solicita en este ejericio conocer el resultado del real madrid de la fecha 20 de Diciembre del 2015
m$find(
  query = '{"Date": "2015-12-20","HomeTeam": "Real Madrid"}'
)

##¿contra quien jugó y cual fue el resultado?
print("El Real Madrid jugó contra el rayo Vallecano, ganando el partido por goleada, anotando 10 goles a favor y recibiendo dos en contra")

##Se cierra la conexión a la base de datos
rm(m)
gc()
