#ANÁLISIS POST-HOC
#Prueba de comparación de Scheffé

#Otra alternativa para hacer un análisis post-hoc es la prueba de Scheffé. Al igual que la corrección de
#Bonferroni, este método también es muy conservador al momento de efectuar comparaciones entre pares.
#No obstante, tiene la ventaja de que permite hacer comparaciones adicionales, además de todos los pares de
#grupos: por ejemplo, podríamos preguntar si un grupo es mejor que todos los demás. El ingeniero del ejemplo
#podría, tras encontrar mediante el procedimiento ANOVA que existen diferencias significativas, plantearse
#preguntas del siguiente tipo:
#1. ¿Existe diferencia entre los tiempos de ejecución de los algoritmos A y B?
#2. ¿Es el tiempo promedio de ejecución del algoritmo A distinto al tiempo de ejecución promedio de los
#algoritmos B y C?

library(tidyverse)
library(DescTools)

# Crear el data frame en formato ancho .
A <- c(23 , 19 , 25 , 23 , 20)
B	<- c(26, 24, 28, 23, 29)
C <- c(19, 24, 20, 21, 17) 
datos <- data.frame(A, B, C)
#	Llevar data frame a formato largo.
datos <- datos 7.>% pivot_longer(c("A", "B", "C"),
names_to = "algoritmo", values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]]) 
datos[["instancia"]] <- factor(1:nrow(datos))

#	Establecer nivel de significacion (el mismo usado en ANOVA). 
alfa <- 0.025
#	Procedimiento ANOVA. 
anova <- aov(tiempo	algoritmo, data = datos)
#	Crear matriz de contrastes. 
contrastes <- matrix(c(1, -1, 0,
                       1, 0, -1,
	                   0,	1,	-1,
	                    1,	-0.5,	-0.5,
                        -0.5, 1, -0.5,
                        -0.5, -0.5, 1),
                            nrow=6,
                        byrow = TRUE)
#	Trasponer matriz de contrastes (para que cada contraste sea una columna). 
contrastes <-	t(contrastes)

#	Hacer prueba de Scheffe.
scheffe <- ScheffeTest(x = anova, which = "algoritmo", contrasts = contrastes, conf.level = 1 - alfa)
print(scheffe)
#Un detalle importante a tener en cuenta es que podemos hacer la llamada a ScheffeTest() sin entregar los
#argumentos which y contrasts, en cuyo caso únicamente se contrastan todos los pares, como en las pruebas
#post-hoc precedentes.
