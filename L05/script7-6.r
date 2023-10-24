#PRUEBAS PARA MUESTRAS PEQUEÑAS
# PRUEBA Q DE COCHRAN
#La prueba Q de Cochran es una extensión de la prueba de mcNemar, adecuada cuando la variable de
#respuesta es dicotómica y la variable independiente tiene más de dos observaciones pareadas (cuando ambas
#variables son dicotómicas, esta prueba es equivalente a la de mcNemar). Como tal, debería estar incluida en la
#sección precedente, pero le dedicaremos una sección aparte pues la explicación requiere de algunos conceptos
#importantes que no hemos estudiado aún.
#Veamos esta prueba por medio de un ejemplo. Elsa Capunta, estudiante de un curso de algoritmos, tiene como
#tarea determinar si existe una diferencia significativa en el desempeño de tres metaheurísticas que buscan
#resolver el el problema del vendedor viajero. Para ello, el profesor le ha proporcionado los datos presentados
#en la tabla 7.13, donde la columna instancia identifica cada instancia del problema empleada para evaluar
#las metaheurísticas y las restantes columnas indican si la metaheurística en cuestión encontró (1) o no (0) la
#solución óptima para dicha instancia.

#Las hipótesis asociadas a la prueba de mcNemar son:
#H0: la proporción de “éxitos” es la misma para todos los grupos.
#HA: la proporción de “éxitos” es distinta para al menos un grupo.

# Condiciones:
#La variable de respuesta es dicotómica.
#La variable independiente es categórica.
#Las observaciones son independientes entre sí.
#El tamaño de la muestra es lo suficientemente grande. Se sugiere que n · k ≥ 24, donde n ≥ 4 es el
#número de casos cuyas respuestas no son únicamente éxitos o fracasos (unos o ceros) y k es la cantidad
#de niveles en la variable independiente (NCSS, 2022).

library(tidyverse) 
library(RVAideMemoire) 
library(rcompanion) 
# Crear matriz de datos. 
instancia <- 1:15 
annealing <- c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)  
hormigas <- c(0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1)
genetico <- c(1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1) 
datos <- data.frame(instancia, annealing, hormigas, genetico) 

# Llevar matriz de datos a formato largo. 
datos <- datos %>% pivot_longer(c("annealing", "hormigas", "genetico"), 
                                names_to = "metaheuristica", 
                                values_to = "resultado") 
datos[["instancia"]] <- factor(datos[["instancia"]])
datos[["metaheuristica"]] <- factor(datos[["metaheuristica"]])
# Hacer prueba Q de Cochran. 
prueba <- cochran.qtest(resultado - metaheuristica I instancia, 
                        data = datos, alpha = 0.05) 
print(prueba) 

#Algo importante que debemos recordar: solo haremos un procedimiento post-hoc si la prueba ómnibus rechaza la hipótesis nula en favor de la hipótesis alternativa. Además, el procedimiento post-hoc
#realizado debe considerar el mismo nivel de significación que la prueba ómnibus
# Procedimiento post-hoc con correccion de Bonferroni. 
post_hoc_l <- pairwiseMcnemar(resultado - metaheuristica I instancia, data = datos, method = "bonferroni") 

cat("\nProcedimiento post-hoc con correccion de Bonferroni\n") 
print (post_hoc_1) 
# Procedimiento post-hoc con correccion de Holm. post_hoc_2 <- pairwiseMcnemar(resultado - metaheuristica I instancia, data = datos, method = "holm") 
cat("\nProcedimiento post-hoc con correccion de Holm\n")
print (post_hoc_2) 
