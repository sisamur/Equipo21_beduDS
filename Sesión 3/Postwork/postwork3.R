#Ahora graficaremos probabilidades (estimadas) marginales y 
#conjuntas para el número de goles que anotan en un partido el 
#equipo de casa o el equipo visitante.

#Con el último data frame obtenido en el postwork de la sesión 2, 
#elabora tablas de frecuencias relativas para estimar las siguientes 
#probabilidades:


#####
#Postwork 2
suppressWarnings(suppressMessages(library(dplyr)))
library(ggplot2)

setwd("/home/luis/Documents/bedu_data_sciences/module2/R/3/postwork")

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

#####
#La probabilidad (marginal) de que el equipo que 
#juega en casa (FTHG) anote x goles (x=0,1,2,)

p.casa <- round( (table(d.liga$FTHG) / dim(d.liga)[1])
                 * 100, digits = 2)
p.casa

#La probabilidad (marginal) de que el equipo que 
#juega como visitante (FTAG) anote y goles (y=0,1,2,)

p.vis <- round( (table(d.liga$FTAG) / dim(d.liga)[1])
                 * 100, digits = 2)
p.vis

#La probabilidad (conjunta) de que el equipo que 
#juega en casa anote x goles y el equipo que juega 
#como visitante anote y goles (x=0,1,2,, y=0,1,2,)

#primero realizamos una tabla conjunta (o contingencia)
con.table <- table(d.liga$FTHG, d.liga$FTAG)
con.table
p.con <- round(con.table / dim(d.liga)[1] * 100, digits = 2)
p.con

#Realiza lo siguiente:
  
#Un gráfico de barras para las probabilidades marginales 
#estimadas del número de goles que anota el equipo de casa

#convertimos en dataframe
df.p.vis <- as.data.frame(p.vis)
colnames(df.p.vis) <- c("Goles", "Probabilidad")
df.p.vis

#Graficamos
df.p.vis %>%
  ggplot() + #tambien se puede realizar usando el aes...
  aes(x = Goles, y = Probabilidad) +
  ggtitle("Probabilidad marginal de goles anotados en casa por equipo") +
  geom_bar(stat="identity", fill="steelblue",  width=0.7) + 
  geom_text(aes(label=Probabilidad),  vjust=-0.3, size=3.5) + 
  theme_minimal()


#Un gráfico de barras para las probabilidades marginales 
#estimadas del número de goles que anota el equipo visitante.

#convertimos en dataframe
df.p.casa <- as.data.frame(p.casa)
colnames(df.p.casa) <- c("Goles", "Probabilidad")
df.p.casa

#Graficamos
df.p.casa %>%
  ggplot() + #tambien se puede realizar usando el aes...
  aes(x = Goles, y = Probabilidad) +
  ggtitle("Probabilidad marginal de goles anotados de visitante por equipo") +
  geom_bar(stat="identity", fill="steelblue",  width=0.7) + 
  geom_text(aes(label=Probabilidad),  vjust=-0.3, size=3.5) + 
  theme_minimal()



#Un HeatMap para las probabilidades conjuntas 
#estimadas de los números de goles que anotan 
#el equipo de casa y el equipo visitante en un partido.

#realizamos el data frame

df.p.con <- as.data.frame(p.con)
colnames(df.p.con) <- c("goles.casa", "goles.visita", "prob")

df.p.con %>%
  ggplot() + #tambien se puede realizar usando el aes...
  aes(x = goles.casa, y = goles.visita) +
  geom_tile(aes(fill = prob), colour = "black") + 
  scale_fill_gradientn(colours = terrain.colors(50)) +
  ggtitle("Probabilidad conjunta de goles anotados") +
  geom_text(aes(label=prob),  vjust=-0.3, size=3.5) + 
  theme_minimal() +
  labs(x='Goles anotads en casa',
       y='Goles anotados de visitante',
       fill='Probabilidad conjuta') 

