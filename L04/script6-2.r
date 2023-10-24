# Cenerar un vector con un rango para el tamafio de la muestra.
    n <- seq(5, 8000, 5) 
# Definir constantes 
    desv_est <- 6 alfa <- 0.05 tam_efecto <- 0.5 
# Se calcula el poder con que se detecta el tamafio del efecto para 
# cada tamanio de la muestra, asumiendo una prueba bilateral para 
# una sola muestra. 
    poder <- pover.t.test(  n = n,
                            delta = tam_efecto,
                            sd = desv_est, 
                            sig.level = alfa, 
                            type = "two.sample", 
                            alternative = "two.sided")Spower 
# Crear un data frame. 
    datos <- data.frame(n, poder) 
# Craficar la curva de poder. 
    g <- ggplot(datos, aes(n, poder)) 
    g <- g + geom_line(colour = "red") 
    g <- g + ylab("Poder estadistico") 
    g <- g + xlab("Tamafio de la muestra") 
    g <- g + theme_pubr() 
    g <- g + ggtitle("Relacion entre el poder y el tamafio de la muestra") 
    print(g) 
