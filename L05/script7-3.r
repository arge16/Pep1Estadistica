#   Prueba chi-cuadrado de independencia

#Esta prueba permite determinar si dos variables categóricas, de una misma población, son estadísticamente independientes o si, por el contrario, están relacionadas.
#Tomemos en este caso como ejemplo que un micólogo desea determinar si existe relación entre la forma del
#sombrero de los hongos y si estos son o no comestibles. Para ello, tras recolectar una muestra de 8.120 hongos,
#obtiene la tabla de contingencia que se muestra en la tabla 7.61

# Las hipótesis a contrastar son:
#H0: : las variables clase y forma del sombrero son independientes.
#HA : las variables clase y forma del sombrero están relacionadas.
# Crear tabla de contingencia. 
comestible <- c(404, 1948, 32, 228, 1596) 
venenoso <- c(48, 1708, 0, 600, 1556) 
tabla <- as.table(rbind(comestible, venenoso)) 
dimnames(tabla) <- list(tipo = c("comestible", "venenoso"), 
                        sombrero = c("campana", "convexo", "hundido", "nudoso", "plano")) 
print(tabla) 
# Hacer prueba chi-cuadrado de independencia. 
prueba <- chisq.test(tabla) 
cat("\nLa prueba internamente calcula los valores esperados:\n") 
esperados <- round(prueba[["expected"]], 3) 
print(esperados) 
cat("\nResultado de la prueba:\n") print(prueba) 