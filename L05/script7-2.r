#  Prueba chi-cuadrado de bondad de ajuste

#Esta prueba permite comprobar si una distribución de frecuencias observada se asemeja a una
#distribución esperada. Usualmente se emplea para comprobar si una muestra es representativa de la
#población (NIST/SEMATECH, 2013, p. 1.3.5.15).
#Para entender mejor esta idea, supongamos ahora que una gran empresa de desarrollo de software cuenta
#con una nómina de 660 programadores, especialistas en diferentes lenguajes de programación. El gerente
#ha seleccionado un subconjunto de 55 programadores, supuestamente de forma aleatoria, para enviarlos a
#cursos de perfeccionamiento en sus respectivos lenguajes, pero el sindicato lo ha acusado de “seleccionar estas
#personas a conveniencia de los intereses mezquinos de la gerencia, impidiendo que el grupo sea representativo a
#fin de asegurar una mejora en la productividad de toda la empresa”. Ante el inminente riesgo de movilizaciones,
#el gerente necesita demostrar que el grupo seleccionado es una muestra representativa de sus programadores.

# Las hipótesis a contrastar son:
#H0: las proporciones de especialistas en cada lenguaje son las mismas para la nómina y la muestra.
# las proporciones de especialistas en cada lenguaje son diferentes en la nómina que en la muestra.

# Crear tabla de contingencia.
nomina <- c(236, 78, 204, 76, 66) muestra <- c(17, 9, 14, 10, 5) 
tabla <- as.table(rbind(nomina, muestra)) 
dimnames(tabla) <- list(grupo = c("Nomina", "Muestra"), 
                        lenguajes = c("C", "Java", "Python", "Ruby", "Otro")) 
print(tabla) 
# Verificar si se esperan mas de 5 observaciones por cada grupo. 
n_nomina <- sum(nomina) 
n_muestra <- 55 
proporciones <- round(nomina/n_nomina, 3) 
esperados <- round(proporciones * n_muestra, 3) 
print(esperados) 
# Hacer prueba chi-cuadrado de homogeneidad.
prueba <- chisq.test(tabla, correct = FALSE) 
print(prueba) 