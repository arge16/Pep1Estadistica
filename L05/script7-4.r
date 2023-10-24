#PRUEBAS PARA MUESTRAS PEQUEÑAS
#Prueba exacta de Fisher 
#La prueba exacta de Fisher es una alternativa a la prueba χ de independencia en el caso de que ambas
#variables sean dicotómicas. Así, las hipótesis a contrastar son:
#Por ejemplo, supongamos que la Sociedad Científica de Computación (SCC) ha realizado una encuesta a
#300 programadores con más de 3 años de experiencia de todo el país, escogidos al azar, y les ha preguntado
#cuál es su lenguaje de programación favorito. La tabla 7.1 muestra las preferencias para cada lenguaje,
#separadas en programadores (varones) y programadoras (mujeres). ¿Son similares las preferencias de lenguaje
#de programación entre hombres y mujeres?

# Las hipótesis a contrastar son:
# H0: las variables son independientes.
# HA: las variables están relacionadas.

# Construir la tabla de contingencia. 
vacuna <- c(rep("Argh", 6), rep("Grrr", 11)) 
resultado <- c(rep("Humano", 12), rep("Vampiro", 5)) 
datos <- data.frame(resultado, vacuna)
 tabla <- xtabs(s., datos) 
 print(tabla) 
# Aplicar prueba exacta de Fisher. 
alfa <- 0.05 
prueba <- fisher.test(tabla, 1-alfa) 
