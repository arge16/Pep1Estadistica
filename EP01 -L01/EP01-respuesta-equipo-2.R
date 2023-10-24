#     $$$$$$$$\                    $$\                            $$$$$$\  
#    $$  _____|                   \__|                          $$  __$$\ 
#   $$ |      $$$$$$\  $$\   $$\ $$\  $$$$$$\   $$$$$$\        \__/  $$ |
#   $$$$$\   $$  __$$\ $$ |  $$ |$$ |$$  __$$\ $$  __$$\        $$$$$$  |
#   $$  __|  $$ /  $$ |$$ |  $$ |$$ |$$ /  $$ |$$ /  $$ |      $$  ____/ 
#   $$ |     $$ |  $$ |$$ |  $$ |$$ |$$ |  $$ |$$ |  $$ |      $$ |      
#   $$$$$$$$\\$$$$$$$ |\$$$$$$  |$$ |$$$$$$$  |\$$$$$$  |      $$$$$$$$\ 
#   \________|\____$$ | \______/ \__|$$  ____/  \______/       \________|
#                 $$ |              $$ |                                
#                $$ |              $$ |                                
#               \__|              \__|                                

#Equipo 2
#    ¿Cómo diría que es el ingreso de los hombres de la RM? (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)

#integrantes:
# Grupo 2 - Pregunta 2
# Roberto Galleguillos
# Bastian Guerrero
# Franco Salvo

install.packages("modeest")
install.packages("ggplot2")

library("modeest")
library("dplyr")
library("ggplot2")
library("ggpubr")


###LECTURA DE CSV
data <- read.csv("EP01 Datos Casen 2017.csv", sep = ";", header = TRUE)
head(data)



#FILTRADO DE DATOS
datos_filtrados <- data[data$sexo == "Hombre",]



#cALCULO ESTADISTICA
summary(datos_filtrados)

moda <- mfv(datos_filtrados$ytot)
moda

#histograma

g1 <- gghistogram(
  datos_filtrados,
  x = "ytot",
  bins = 50,
  add = "mean",
  xlab = "Salario (CLP)",
  ylab = "frecuencia",
  color = "blue",
  fill = "blue",
  xlim = c(0, 72691664)
)

print(g1)



#boxplot

box <- ggboxplot(
  datos_filtrados[["ytot"]],
  color = "yellow",
  fill = "pink",
  ylab = "Salario (CLP)",
  ylim = c(-50000,2000000)
  )

print(box)







# ¿Cómo diría que es el ingreso de los hombres de la RM? (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)


# Para responder la presemte pregunta, hizo falta trabajar con la variable "sexo" de tipo categórica nominal, junto con la 
# variable "ytot",de tipo numérica discreta. Como medida estadística se utilizó la moda, esto para determinar si se presenta
# un comportamiento unimodal, bimodal o multimodal, en donde se determina que es unimodal, con una moda de 11091. También se 
# requirió realizar dos gráficos, un histograma y un diagrama de caja, con ello se puede presenciar un comportamiento de una 
# asimetría negativa, por parte del histograma,  lo que significa una alta frecuancia en personas con menor ingreso, mientras
# que  por parte del gráfico de caja, se presenta un comportamiento disperso por parte de los ingresos, esto último tiene 
# sentido debido a que los ingresos en Chile se sabe que son bastante desiguales en ciertos casos. 

