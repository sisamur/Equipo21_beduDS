#Postwork 4

#Ahora investigarás la dependencia o independencia del número de goles anotados 
#por el equipo de casa y el número de goles anotados por el equipo visitante 
#mediante un procedimiento denominado bootstrap, revisa bibliografía en internet 
#para que tengas nociones de este desarrollo.


#cargamos las librerias
suppressWarnings(suppressMessages(library(dplyr)))


u.1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u.1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u.1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

df.1718 <- read.csv(file = u.1718)
df.1819 <- read.csv(file = u.1819)
df.1920 <- read.csv(file = u.1920)

lista <- list(df.1718, df.1819, df.1920)

lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

d.liga <- do.call(rbind, lista)

d.liga <- mutate(d.liga, Date = as.Date(Date, "%d/%m/%Y"))

#se hacen pequeños para pruebas
#d.liga <- d.liga[d.liga$FTHG < 3,]
#d.liga <- d.liga[d.liga$FTAG < 3,]
#####
#La probabilidad (marginal) de que el equipo que 
#juega en casa (FTHG) anote x goles (x=0,1,2,)

p.casa <- round( (table(d.liga$FTHG) / dim(d.liga)[1]), digits = 3)
p.casa

#La probabilidad (marginal) de que el equipo que 
#juega como visitante (FTAG) anote y goles (y=0,1,2,)

p.vis <- round( (table(d.liga$FTAG) / dim(d.liga)[1]), digits = 3)
p.vis

#La probabilidad (conjunta) de que el equipo que 
#juega en casa anote x goles y el equipo que juega 
#como visitante anote y goles (x=0,1,2,, y=0,1,2,)

#primero realizamos una tabla conjunta (o contingencia)

con.table <- table(d.liga$FTHG, d.liga$FTAG)
con.table
p.con <- round(con.table / dim(d.liga)[1], digits = 3)
p.con

#Ya hemos estimado las probabilidades conjuntas de que el equipo de casa 
#anote X=x goles (x=0,1,... ,8), y el equipo visitante anote Y=y 
#goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al 
#dividir estas probabilidades conjuntas por el producto de las probabilidades 
#marginales correspondientes.

#a partir del producto tensorial obtenemos la matrix de las probabilidades marginales
pt.vc <-  p.casa %*% t(p.vis)

#Obtenemos  la tabla de cocientes
(t.c <- round(p.con / pt.vc, digits = 2))

#Mediante un procedimiento de boostrap, obtén más cocientes similares a los
#obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona 
#en cuáles casos le parece razonable suponer que los cocientes de la tabla en 
#el punto 1, son iguales a 1 (en tal caso tendríamos independencia de las 
#variables aleatorias X y Y).

#creamos una lista con 10000 tablas, obtenidas a partir de un bootstrap
#de diferentes muestras tomadas 

t.co = vector(mode="list", length=10000)

for(i in 1:10000) {
  #tomamos la muestra
  sm = sample(dim(d.liga)[1], size = 666, replace=T)
  dt.n = d.liga[sm,]
  #calculamos las probabilidades goles (casa y visitante)
  p.casa <- round( (table(dt.n$FTHG) / dim(dt.n)[1]), digits = 3)
  p.vis <- round( (table(dt.n$FTAG) / dim(dt.n)[1]), digits = 3)
  #probabilidades conjuntas
  p.con <- round(table(dt.n$FTHG, dt.n$FTAG) / dim(dt.n)[1], digits = 3)
  #probabilidades marginales
  pt.vc <-  p.casa %*% t(p.vis)
  #tabla de cocientes en una vector
  t.co[[i]] <- round(p.con / pt.vc, digits = 3)
  
}

#Creamos un vector para analizar los valores, dependiendo de la relacion 
#a analizar en la tabla de cocientes, por ejemplo tenemos 0 goles en casa
#con 1 gol como visitante
g.casa = "6"
g.visi = "2"

x = c()
for(i in 1:10000){ 
  x1 <- t.co[[i]][g.casa,g.visi]
  x <- c(x, x1)
}
hist(x)
