# Prueba chi-cuadrado de homogeneidad
#Esta prueba resulta adecuada si queremos determinar si dos poblaciones (la variable dicotómica) presentan
#las mismas proporciones en los diferentes niveles de una variable categórica.
#Por ejemplo, supongamos que la Sociedad Científica de Computación (SCC) ha realizado una encuesta a
#300 programadores con más de 3 años de experiencia de todo el país, escogidos al azar, y les ha preguntado
#cuál es su lenguaje de programación favorito. La tabla 7.1 muestra las preferencias para cada lenguaje,
#separadas en programadores (varones) y programadoras (mujeres). ¿Son similares las preferencias de lenguaje
#de programación entre hombres y mujeres?
# Las hipótesis a contrastar son:
# H0: programadores hombres y mujeres tienen las mismas preferencias en lenguaje de programación favorito
# (ambas poblaciones muestras las mismas proporciones para cada lenguaje estudiado).
# HA: programadores hombres y mujeres tienen preferencias distintas en lenguajes de programación favorito.

# Crear tabla de contingencia. 
programadores <- c(42, 56, 51, 27, 24) programadoras <- c(25, 24, 27, 15, 9) 
tabla <- as.table(rbind(programadores, programadoras)) 
dimnames(tabla) <- list(sexo = c("programadores", "programadoras"), lenguajes = c("C", "Java", "Python", "Ruby", "Otro")) 
print(tabla) 
# Hacer prueba chi-cuadrado de homogeneidad. 
prueba <- chisq.test(tabla) print(prueba) 