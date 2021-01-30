#### postwork 6
suppressWarnings(suppressMessages(library(dplyr)))
#Importa el conjunto de datos match.data.csv a R
data <- read.csv("match.data.csv")
data <- mutate(data, date = as.Date(date, "%Y-%m-%d"))
head(data)
str(data)

# 1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido.

data$sum.score <- data$home.score+data$away.score

head(data)
str(data)
# 2. Obtén el promedio por mes de la suma de goles.

#  meses y años
data$my <- format(data$date,format="%Y-%m")

str(data)
tail(data)
#  obtener la media
mn.mon <- aggregate( sum.score ~ my, data , mean )
mn.mon <- rename(mn.mon, "month.year"= my, "avg.score" = sum.score)
str(mn.mon)

# 3. Crea la serie de tiempo del promedio por mes de 
#la suma de goles hasta diciembre de 2019.
a = which(mn.mon$month.year == "2019-12")

mn.mon <- mn.mon[1:a,]

prom <- ts(mn.mon$avg.score)

# 4. Grafica la serie de tiempo.

ts.plot(prom)
