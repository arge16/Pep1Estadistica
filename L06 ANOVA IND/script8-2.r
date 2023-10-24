#ANÁLISIS POST-HOC
#Bonferroni y Holm
#Al aplicar el procedimiento ANOVA de una vía para muestras independientes a nuestro ejemplo pudimos
#concluir que existe al menos un algoritmo cuyo tiempo promedio de ejecución es diferente al
#de los demás. Ahora bien, si los algoritmos del ejemplo tienen por objeto resolver un problema crítico, de
#cuya rápida solución depende aumentar la productividad de una empresa o prevenir una situación de mucho
#riesgo, desde luego nos interesaría conocer cuál es el mejor (o el peor) de los algoritmos comparados a fin de
#poder garantizar un menor tiempo de respuesta. En consecuencia, necesitamos contar con algún método que
#permita determinar si los tiempos de ejecución de los algoritmos A y B difieren significativamente, o los de
#B y C, o bien los de A y C. En el contexto general, si tenemos k grupos, la cantidad de comparaciones (N)
#que deberíamos efectuar está dada por la ecuación 8.12

#El script 8.2 muestra la realización de pruebas t para cada par de grupos usando tanto la corrección de
#Bonferroni como la de Holm, obteniéndose los resultados que se muestran en la figura 8.3. Debemos recordar
#que el nivel de significación que se entrega como argumento es el mismo que usamos en el procedimiento
#ANOVA.
library ( tidyverse )
# Crear el data frame en formato ancho .
A <- c(23, 19, 25, 23, 20)
#	<- c(26, 24, 28, 23, 29) C <- c(19, 24, 20, 21, 17) datos <- data.frame(A, B, C)
#	Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("A", "B", "C"),
names_to = "algoritmo", values_to = "tiempo")
datos[["algoritmo"]] <- factor(datos[["algoritmo"]]) datos[["instancia"]] <- factor(1:nrow(datos))
#	Establecer nivel de significacion (el mismo usado en ANOVA). alfa <- 0.025
#	Procedimiento post-hoc de Bonferroni. cat("Procedimiento post-hoc de Bonferroni\n\n")
bonferroni <- pairwise.t.test(datos[["tiempo"]],
datos[["algoritmo"]], p.adj = "bonferroni", pool.sd = TRUE,
paired = FALSE,
conf.level = 1 - alfa)
print(bonferroni)
#	Procedimiento post-hoc de Holm. cat("\n\nProcedimiento post-hoc de Holm\n\n")
holm <- pairwise.t.test(datos[["tiempo"]],
datos[["algoritmo"]],
p.adj = "holm", pool.sd = TRUE, paired = FALSE, conf.level = 1 - alfa)
print ( holm )

#Los valores p obtenidos con ambos métodos son diferentes (un lector atento recordará que la corrección de
#Bonferroni es considerada muy conservadora). Sin embargo, en ambos casos podemos ver que únicamente los
#algoritmos B y C presentan una diferencia significativa al comparar el valor p ajustado que entrega R con el
#nivel de significación (α = 0, 025). Si miramos el gráfico del tamaño del efecto obtenido para el procedimiento
#ANOVA (figura 8.2), podemos concluir entonces con 97,5 % de confianza que el algoritmo C es más rápido
#que el algoritmo B.
