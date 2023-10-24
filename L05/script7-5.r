#PRUEBAS PARA MUESTRAS PEQUEÑAS
#Prueba de mcNemar 
#Esta prueba resulta apropiada cuando una misma característica, con respuesta dicotómica, se mide en dos
#ocasiones diferentes para los mismos sujetos (muestras pareadas) y queremos determinar si se produce o
#no un cambio significativo entre ambas mediciones. Una vez más, podemos registrar las frecuencias en una
#matriz de confusión como la que vimos en 7.8. En ella, para este análisis, las celdas a y d corresponderían
#a instancias en que no hay cambios. La celda b en dicha tabla representa a las instancias que cambian de
#Presente a Ausente y la celda c, a instancias que cambian de Ausente a Presente.
#Las hipótesis asociadas a la prueba de mcNemar son:
#H0: no hay cambios significativos en las respuestas.
#HA: sí hay cambios significativos en las respuestas.

# Construir la tabla de contingencia. 
alumno <- seq(1:25) 
modelo_1 <- c(rep("Correcto", 16), rep("Incorrecto", 9)) 
modelo_2 <- c(rep("Correcto", 9), rep("Incorrecto", 11), rep("Correcto", 5)) 
datos <- data.frame(alumno, modelo_2, modelo_1) 
tabla <- table(modelo_2, modelo_1) 
print(tabla) 
# Aplicar prueba de McNemar. 

prueba <- mcnemar.test(tabla)
print (prueba)

