#ANOVA DE UNA VÍA PARA MUESTRAS INDEPENDIENTES

#En el capítulo 8 conocimos el procedimiento ANOVA de una vía para muestras independientes, que podemos
#entender como una extensión de la prueba t de Student para muestras independientes. De manera similar,
#ahora abordaremos el procedimiento ANOVA de una vía para muestras correlacionadas (también
#llamado ANOVA para medidas repetidas o ANOVA intra-sujetos) que puede asociarse a la prueba
#t con muestras pareadas, pero ahora con tres o más mediciones (o condiciones) en lugar de dos. Para ello
#tomaremos como base la explicación que ofrece Lowry (1999, cap. 15).
#En este caso podemos distinguir entre dos escenarios:
#Diseño con medidas repetidas: a cada sujeto se le toman medidas en las diferentes condiciones, por
#ejemplo, registrar los tiempos de ejecución para una misma instancia de un problema con k algoritmos
#diferentes.
#Diseño con bloques aleatorios: cada bloque contiene diferentes sujetos agrupados según una determinada característica, por ejemplo, registrar tiempos de ejecución usando instancias de grafos diferentes,
#pero similares (como que tengan el mismo número de vértices y aristas), para los k algoritmos.
#El método es el mismo en ambos casos e intenta controlar estadísticamente la variación introducida por
#factores distintos al que se desea estudiar, usando para ello varias mediciones de un sujeto (o grupos de
#sujetos parecidos). Si bien el diseño con bloques aleatorios es común, especialmente en medicina, este apunte
#usa las medidas repetidas en su discusión, ya que son más comunes en el área de la informática.
#Como es habitual, usemos un ejemplo para ver cómo se lleva a cabo el procedimiento ANOVA de una vía
#para muestras correlacionadas. Supongamos que un estudiante de un curso de programación debe comparar
#la eficiencia de cuatro algoritmos de ordenamiento: quicksort, bubblesort, radixsort y mergesort. Para ello, ha
#seleccionado aleatoriamente 6 arreglos de igual tamaño y registrado, para cada uno de ellos, el tiempo de
#ejecución utilizado por cada algoritmo (en milisegundos), como muestra la tabla 9.11


#En este caso, la lógica es muy similar a la que ya conocimos para ANOVA con muestras independientes. Sin
#embargo, existe una diferencia importante al trabajar con muestras correlacionadas: no toda la variabilidad es
#pura e inevitable, sino que una parte de ella se debe a diferencias individuales preexistentes entre los sujetos
#(por ejemplo, un arreglo puede estar ordenado desde el inicio, mientras otro podría estar en orden inverso).
#Recordemos que la pregunta detrás de ANOVA es: ¿se diferencian las medias poblacionales?, por lo que
#nuestras hipótesis son:
#H0: El tiempo de ejecución promedio es igual para los cuatro algoritmos.
#HA: El tiempo de ejecución promedio es diferente para al menos un algoritmo.

#Condiciones:

#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
#iguales.
#2. Las mediciones son independientes al interior de cada grupo.
#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#4. La matriz de varianzas-covarianzas es esférica. Como explica Horn (2008, p. 1), esta condición establece
#que las varianzas entre los diferentes niveles de las medidas repetidas deben ser iguales


library(tidyverse) 
library(ggpubr)
library(ez)
#	Crear el data frame. 
instancia <- factor(1:6) 
Quicksort <- c(23.2 , 22.6 , 23.4 , 23.3 , 21.8 , 23.9)
Bubblesort <- c(31.6 , 29.3 , 30.7 , 30.8 , 29.8 , 30.3)
Radixsort <- c(30.1 , 28.4 , 28.7 , 28.3 , 29.9 , 29.1)
Mergesort <- c(25.0 , 25.7 , 25.7 , 23.7 , 25.5 , 24.7)
datos <- data.frame(instancia, Quicksort, Bubblesort, Radixsort, Mergesort)

#	Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("Quicksort", "Bubblesort", "Radixsort",
                                "Mergesort") ,
                                names_to = "algoritmo", values_to = "tiempo")
datos [["algoritmo"]] <- factor ( datos [["algoritmo" ]])

#	Comprobcion de normalidad.
g <- ggqqplot(datos, x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(- algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text") 
g <- g + rremove("y.ticks") + rremove("y.text") 
g <- g + rremove("axis.title")
print(g)

# Procedimiento ANOVA con ezANOVA ().
cat("Procedimiento ANOVA usando aov\n\n")
prueba <- aov(tiempo~algoritmo + Error(instancia/(algoritmo)),
data = datos)
print(summary(prueba))
#	Procedimiento ANOVA con ezANOVA(). 
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
prueba2 <- ezANOVA(data = datos, dv = tiempo, within = algoritmo, wid = instancia, return_aov = TRUE)
print(summary(prueba2$aov))
cat("\n\nPero ezANOVA entrega rads informacion.\n")
cat("El resultado de la prueba de esfericidad de Mauchly:\n\n") 
print(prueba2[["Mauchly's Test for Sphericity"]])
cat("\n\nY factores de correccion para cuando no se cumple la\n") 
cat("condici6n de esfericidad:\n\n")
print(prueba2$'Sphericity Corrections')

#	Grdfico del tamafio del efecto.
g2 <- ezPlot(data = datos, dv = tiempo, wid = instancia, within = algoritmo, y_lab = "Tiempo promedio de ejecucion [ms]", x = algoritmo)

print(g2)


#Habíamos mencionado que otra ventaja de ezANOVA() es que verifica la condición de esfericidad mediante la
#prueba de esfericidad de Mauchly, cuyo resultado se muestra en la figura 9.4. Podemos apreciar que el valor p
#obtenido en esta prueba es muy alto (p = 0, 555), de lo que se desprende que los datos del ejemplo sí cumplen
#con la condición de esfericidad (hipótesis nula de la prueba de Mauchly).
