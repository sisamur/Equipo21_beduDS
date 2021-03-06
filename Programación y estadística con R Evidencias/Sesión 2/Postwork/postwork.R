###post work dia 2
#cargamos la librerias
#usamos esta forma de cargar la libreria para evitar mensajes 
suppressWarnings(suppressMessages(library(dplyr)))

#situamos le path para los datos
setwd("/home/luis/Documents/bedu_data_sciences/module2/R/2/postwork/files")

#Importa los datos de soccer de las temporadas 2017/2018, 
#2018/2019 y 2019/2020 de la primera división de la liga 
#española a R, los datos los puedes encontrar en el siguiente 
#enlace: https://www.football-data.co.uk/spainm.php
u.1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u.1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u.1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

download.file(url = u.1718, destfile = "SP1-1718.csv", mode = "wb")
download.file(url = u.1819, destfile = "SP1-1819.csv", mode = "wb")
download.file(url = u.1920, destfile = "SP1-1920.csv", mode = "wb")

#importamos los datos
lista <- lapply(dir(), read.csv) # Guardamos los archivos en lista
lista

#Obten una mejor idea de las características de los data 
#frames al usar las funciones: str, head, View y summary

str(lista)
head(lista)
summary(lista)
View(lista)
names(lista)
#Al ser una lsita con todo los datos, usamos lapply para 
#limpiar la lista con los dataframe

#seleccionamos Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR
#usando lapply
lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
str(lista)

#Asegúrate de que los elementos de las columnas correspondientes 
#de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date 
#y mutate para arreglar las fechas). Con ayuda de la función 
#rbind forma un único data frame que contenga las seis columnas 
#mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).

#antes de la conversion, realizaremos el data frame
d.liga <- do.call(rbind, lista)
str(d.liga)

#procedemos a cambiar del data frame las fecheas de formato: de chr a date
d.liga <- mutate(d.liga, Date = as.Date(Date, "%d/%m/%Y"))
str(d.liga)
head(d.liga)
tail(d.liga)
