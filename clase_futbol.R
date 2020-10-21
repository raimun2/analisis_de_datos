#    si no sabes usar R para hacer analisis de datos, puedes aprender en https://r4ds.had.co.nz/introduction.html

#    lo primero que hago es cargar las librerias que contienen las funciones para graficas y ordenar la data
library(ggplot2)
library(tidyr)

#    aca estoy leyendo una tabla csv que se encuentra publica en internet, y viene separada por ";" y con encabezados
Mundiales = read.table("http://dataminingsoccer.com/de/wp-content/downloads/SoccerDataAllWorldCups.csv", sep=";",header=T)

#    con esta linea de codigo inspeccionamos la data
head(Mundiales) # cada fila en la data representa un partido

#    creamos una variable con los goles por partido
Mundiales$goles_partido = Mundiales$score.A + Mundiales$score.B

#    exploramos el numero de goles por partido con un histograma
hist(Mundiales$goles_partido)

#    veo si las etapas del torneo son consistentes
table(Mundiales$which.stage.of.the.turnament)

#    creamos otra variable para la fase simplificada. esta variable se inicia como "grupos" para todas las fases
Mundiales$fase = "grupos"

#    y luego cambiamos aquellas fases que contengan la palabra "final" (final, semi final y final round)
Mundiales$fase[grep("^final|^semi final|^final round",Mundiales$which.stage.of.the.turnament)] = "final"

#    y finalmente cambiamos aquellas fases que contengan la palabra "third" por 3y4
Mundiales$fase[grep("third",Mundiales$which.stage.of.the.turnament)] = "3y4"

#    agrupamos la informacion, para cada año y fase simplificada, calculamos el promedio de goles por partido
data_fase = aggregate(goles_partido ~ year+fase, Mundiales, mean)

#    creamos grafico de evolucion de los goles por partido en el tiempo, diferenciado por fase simplificada
ggplot(data_fase, aes(x=year, y =goles_partido, col=fase)) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() + 
  xlab("Año del mundial") +
  ylab("# Goles") + 
  ggtitle("Numero de goles por partido segun año y fase")

