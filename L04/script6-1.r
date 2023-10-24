library(ggpubr)
library(tidyverse)
#Generar un vector con un rango de valores para la efecto de medias
efecto <- seq (-2.5 , 2.5 , 0.01)
# Calcular el poder para una prueba t bilareral , para cada tama ñ o9 # del efecto , asumiendo una muestra con desviación estándar igual a 1
# Se Se consideran 4 escenarios e 1. Una muestra de tamafio 6 e 2. Una muestra de tamafio 6 e 3. Una muestra de tamafio 10 e 4. Una muestra de tamafio 10 
# para calcular el poder: y nivel de significacion 0.05. y nivel de significacion 0.01. y nivel de significacion 0.05. y nivel de significacion 0.01. 
n_6_alfa_05 <- pover.t.test(n = 6,
                            delta = efecto,
                            sd = 1, sig.level = 0.05,
                            type = "one.sample",
                            alternative = "two.sided")$power 

n_6_alfa_01 <- pover.t.test(n = 6, delta = efecto,
                            sd = 1, sig.level = 0.01,
                            type = "one.sample",
                            alternative = "two.sided")$power 

n_10_alfa_05 <- pover.t.test(n = 10,
                             delta = efecto,
                             sd = I,
                             sig.level = 0.05,
                             type = "one.sample",
                             alternative = "two.sided")$power 

n_10_alfa_01 <- pover.t.test(n = 10,
                            delta = efecto,
                            sd = 1,
                            sig.level = 0.01, 
                            type = "one.sample", alternative = "tvo.sided")Spower 

# Construir matrix de datos en formato ancho.
 datos <- data.frame(efecto, n_6_alfa_05, n_6_alfa_01, n_10_alfa_05, n_10_alfa_01) 
# Llevar a formato largo. 
datos <- datos 7.>.% pivot_longer(!"efecto", names_to = "fuente", values_to = "poder") 
# Formatear fuente como variable categarica. 
niveles <- c("n_6_alfa_05", "n_6_alfa_01", "n_10_alfa_05", "n_10_alfa_01") 
etiquetas <- c("n=6, alfa=0,05", "n=6, alfa=0,01", "n=10, alfa=0,05", "n=10, alfa=0,01") 
datos[rfuentel] <- factor(datos[("fuente")), levels = niveles, labels = etiquetas) 
# Craficar las curvas de poder. g <- ggplot(datos, aes(efecto, poder, colour = factor(fuente))) g <- g + geom_line() g <- g + labs(colour = "") g <- g + ylab("Poder estadistico") g <- g + xlab("Tamafio del efecto") g <- g + scale_color_manual(values=c("red", "blue", "chartreuse4", "orange")) g <- g + theme_pubr() g <- g + ggtitle("Curvas de poder para prueba t bilateral") g <- g + geom_vline(xintercept = 0, linetype = "dashed") 
print(g) 
