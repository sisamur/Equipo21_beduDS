library(dplyr)
library(fbRanks)
#A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020,
#crea el data frame SmallData, que contenga las columnas date, home.team, home.score, away.team y away.score; 
#esto lo puede hacer con ayuda de la función select del paquete dplyr. Luego establece un directorio de trabajo 
#y con ayuda de la función write.csv guarda el data frame como un archivo csv con nombre soccer.csv. Puedes colocar 
#como argumento row.names = FALSE en write.csv.
setwd("D:/documentos/Santander bedu/Fase 2/postwork/sesion5")

a1718<-read.csv("data/SP1_17_18.csv")
a1819<-read.csv("data/SP_18_19.csv")
a1920<-read.csv("data/SP1_19_20.csv")

df.lista = list(a1718, a1819,a1920) 

columnas <- c("Date", "HomeTeam", "AwayTeam","FTHG","FTAG")

filtro <- lapply(df.lista,select, columnas)

SmallData <- do.call(rbind, filtro)
SmallData <- rename(SmallData,date = Date, home.team=HomeTeam, home.score=FTHG, away.team = AwayTeam, away.score=FTAG )
SmallData <- mutate(SmallData, date = as.Date(date, "%d/%m/%y"))
write.csv(x=SmallData, file="data/soccer.csv", row.names = FALSE)

#Con la función create.fbRanks.dataframes del paquete fbRanks importe el archivo soccer.csv a R y al mismo tiempo 
#asignelo a una variable llamada listasoccer. Se creará una lista con los elementos scores y teams que son data frames 
#listos para la función rank.teams. Asigna estos data frames a variables llamadas anotaciones y equipos.

listasoccer<-create.fbRanks.dataframes(scores.file = "data/soccer.csv",team.resolver=NULL,
                                       teams.file=NULL,
                                       date.format="%y-%m-%d",na.remove=TRUE)

anotaciones<-listasoccer$scores
str(anotaciones)

#Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que correspondan a las fechas en las 
#que se jugaron partidos. Crea una variable llamada n que contenga el número de fechas diferentes. Posteriormente, con la 
#función rank.teams y usando como argumentos los data frames anotaciones y equipos, crea un ranking de equipos usando 
#unicamente datos desde la fecha inicial y hasta la penúltima fecha en la que se jugaron partidos, estas fechas las deberá 
#especificar en max.date y min.date. Guarda los resultados con el nombre ranking.

anotaciones <- mutate(anotaciones, date = as.Date(date, "%Y-%m-%d"), home.score=as.numeric(home.score), away.score=as.numeric(away.score))
n<-unique(anotaciones$date)
ranking<-rank.teams(scores=anotaciones,
           max.date=n[length(n)-2], min.date=n[1])

#Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana o el resultado es 
#un empate para los partidos que se jugaron en la última fecha del vector de fechas fecha. Esto lo puedes hacer con ayuda 
#de la función predict y usando como argumentos ranking y fecha[n] que deberá especificar en date.

predict(ranking, min.date = n[length(n)-1], max.date = n[length(n)])

#Referencia de apoyo
#https://cran.r-project.org/web/packages/fbRanks/fbRanks.pdf