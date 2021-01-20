#ponemos el directorio
setwd("/home/luis/Documents/bedu_data_sciences/module2/R/1/poswork/")

# Importa los datos de soccer de la temporada 2019/2020 de la 
#primera división de la liga española a R, los datos los puedes 
#encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

SP1 <- read.csv("SP1.csv")

#Del data frame que resulta de importar los datos a R, 
#extrae las columnas que contienen los números de goles 
#anotados por los equipos que jugaron en casa (FTHG) y 
#los goles anotados por los equipos que jugaron como visitante (FTAG)
dim(SP1)[1]
g.lo <- SP1$FTHG
g.lo
g.vis <- SP1$FTAG
g.vis

#Consulta cómo funciona la función table en R al ejecutar 
#en la consola ?table
?table

#creamos las tablas
g.lo.t <- table(g.lo)
g.vis.t <- table(g.vis)

con.table <- table(SP1$FTHG, SP1$FTAG)

#La probabilidad (marginal) de que el equipo que juega en 
#casa anote x goles (x = 0, 1, 2, ...)
p.lo <- round(g.lo.t / dim(SP1)[1] * 100, digits = 2)

for (i in 1:length(p.lo)){
  print(paste("La probabilidad de que el equipo anote en casa", 
              i-1, "gole(s), es de: ", p.lo[i], "%"))
}

#La probabilidad (marginal) de que el equipo que juega como
#visitante anote y goles (y = 0, 1, 2, ...)

p.vis <- round(g.vis.t / dim(SP1)[1] * 100, digits = 2)

for (i in 1:length(p.vis)){
  print(paste("La probabilidad de que el equipo anote de visitante", 
              i-1, "gole(s), es de: ", p.vis[i], "%"))
}

#La probabilidad (conjunta) de que el equipo que juega en casa 
#anote x goles y el equipo que juega como visitante anote y 
#goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

p.con <- round(con.table / dim(SP1)[1] * 100, digits = 2)
names(dimnames(p.con)) <- list("", "La probabilidad de que el equipo que 
   juega en casa y el que juega de visitante 
   anoten goles, se encuentra en la siguiente 
   tabla:")
p.con       
