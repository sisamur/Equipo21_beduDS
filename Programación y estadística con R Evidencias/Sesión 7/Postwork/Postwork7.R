#Utilizando el manejador de BDD Mongodb Compass (previamente instalado), deberás de realizar las siguientes acciones:
#
#Alojar el fichero data.csv en una base de datos llamada match_games, nombrando al collection como match
#
#Una vez hecho esto, realizar un count para conocer el número de registros que se tiene en la base
#
#Realiza una consulta utilizando la sintaxis de Mongodb, en la base de datos para conocer el número de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra que equipo jugó, ¿perdió ó fue goleada?
#
#Por último, no olvides cerrar la conexión con la BDD

install.packages("mongolite")

###Acorde a las instrucciones, se subio el archivo desde el manejador de compass

##Utilizando la libreria mongolite y con la url de donde se alojo la base de datos en el compass se lee la colaccion
library(mongolite)
m <- mongo("match", url = "mongodb+srv://bedu_p7:NEC@cluster0.wogir.mongodb.net/match_games")

## HAcemos un find general y observamos los datos que se leen
alldata <- m$find('{}')
print(alldata)

##Se hace un count para conocer la cantidad de registros que se tienen
Cantidad<-m$count('{}')
paste("La cantidad de registros son:",Cantidad)

#No se poseen datos inferiores al 2015 como lo solicita el ejercicio, en este ejemplo a falta de preguntar, se asigna una fecha random
m$find(
  query = '{"Date": "2020-07-02","HomeTeam": "Real Madrid"}'
)

##Se cierra la conexi�n a la base de datos
rm(m)
gc()
