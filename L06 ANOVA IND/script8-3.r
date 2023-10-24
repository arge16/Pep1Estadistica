#ANÁLISIS POST-HOC
#Prueba HSD de Tukey
#La prueba HSD de Tukey es más poderosa que los factores de corrección de Bonferroni y Holm. Se asemeja
#a estas últimas en que también busca diferencias significativas (de hecho, el nombre HSD se debe a las
#siglas inglesas para “diferencia honestamente significativa”) entre los diferentes pares de medias, aunque
#usa un enfoque muy diferente: para ello emplea el estadístico Q.


library(tidyverse)
#	Crear el data frame en formato ancho.
A <- c(23, 19, 25, 23, 20)
B<- c(26, 24, 28, 23, 29) 
C <- c(19, 24, 20, 21, 17) 
datos <- data.frame(A, B, C)

#	Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("A", "B", "C"),
names_to = "algoritmo", values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]]) 
datos[["instancia"]] <- factor(1:nrow(datos))
#	Establecer nivel de significacion (el mismo usado en ANOVA). alfa <- 0.025
 
#	Procedimiento ANOVA.
anova <- aov( tiempo ~ algoritmo , data = datos )
# Prueba HSD de Tukey .
post_hoc <- TukeyHSD(anova ,
" algoritmo ",
ordered = TRUE,
conf.level = 1-alfa)
print(post_hoc)

#En la figura 8.4 podemos apreciar que la columna diff muestra las diferencias de las medias entre grupos,
#obteniéndose resultados idénticos a los teóricos, y la columna p.adj entrega valores p asociados a cada
#diferencia, ajustados para compararlos con el nivel de significación original. Cabe destacar que el único valor
#p menor a este nivel (α = 0, 025) corresponde a la diferencia B-C, siendo esta última la única significativa,
#lo cual una vez más coincide con el resultado del procedimiento manual. También debemos notar que las
#columnas lwr y upr muestran el límite inferior y superior, respectivamente, del intervalo de (1 − α) · 100 %
#confianza para la verdadera diferencia entre las medias de los grupos.