#ANOVA DE UNA VÍA PARA MUESTRAS INDEPENDIENTES
# El método de análisis de varianza, comúnmente
#conocido como ANOVA o AoV (del inglés Analysis of Variance), surge, en esencia, como un método para
#combatir este problema al comparar simultáneamente tres o más medias muestrales.

#Para explicar en detalle el procedimiento ANOVA de una vía para muestras independientes, consideremos
#el siguiente ejemplo: un ingeniero cuenta con tres algoritmos (A, B y C) para resolver un determinado
#problema (en iguales condiciones y para instancias de tamaño fijo, digamos con E elementos) y desea comparar
#su eficiencia. Para cada algoritmo, selecciona una muestra aleatoria independiente de instancias y registra
#el tiempo de ejecución (en milisegundos) para cada una de las instancias de la muestra correspondiente,
#obteniendo las siguientes observaciones:


#La pregunta detrás de ANOVA para este ejemplo es: ¿se diferencian los tiempos medios que requieren los
#algoritmos para resolver todas las posibles instancias del problema de tamaño E? De donde se desprende que:
#H0 : el tiempo de ejecución promedio para instancias de tamaño E es igual para los tres algoritmos.
#HA : el tiempo de ejecución promedio para instancias de tamaño E es diferente para al menos un algoritmo.


# Condiciones:
#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
#iguales.
#2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#4. Si las muestras provienen de más de una población, estas tienen la misma varianza.


library(tidyverse)
library(ggpubr) 
library(ez) 
# Crear el data frame en formato ancho. 
A <- c(23, 19, 25, 23, 20) 
B <- c(26, 24, 28, 23, 29) 
C <- c(19, 24, 20, 21, 17) 
datos <- data.frame(A, B, C) 
# Llevar data frame a formato largo. 
datos <- datos %>% pivot_longer(c("A", "B", "C"), names_to = "algoritmo", 
                                                  values_to = "tiempo") 
datos[["algoritmo"]] <- factor(datos[["algoritmo"]]) 
datos[["instancia"]] <- factor(1:nrow(datos)) 

# Comprobcion de normalidad. 
g <- ggqqplot(datos, x = "tiempo", y = "algoritmo", color = "algoritmo") 
g <- g + facet_wrap(- algoritmo)
 g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title") print(g) 
# Procedimiento ANOVA con aov(). 
cat("Procedimiento ANOVA usando aov\n\n") 
prueba <- aov(tiempo algoritmo, data = datos) 
print(summary(prueba)) 
# Procedimiento ANOVA con ezANOVA(). 
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n") 
prueba2 <- ezANOVA( data = datos, dv = tiempo, between = algoritmo, wid = instancia, return_ = TRUE) 
print(prueba2) 
# Grafico del tamaflo del efecto.
g2 <- ezPlot( data = datos, dv = tiempo, wid = instancia, between = algoritmo, y_lab = "Tiempo promedio de ejecucion [ms]", x = algoritmo )
print(g2)