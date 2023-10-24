library(ggpubr)
library(pvr) 
#  Fijar valores conocidos.
sigma <- 12 alfa <- 0.05 n <- 36 
# Calcular el error estindar. 
SE <- sigma / sqrt(n) 
# Crificar la distribuciOn muestral de la media de las diferencias si a la hipOtesis nula fuera verdadera. 
z <- seq(-6 * SE, 4 * SE, 0.01) 
y <- dnorm(z, mean = media_nula, sd = SE) 

g <- ggplot(data = data.frame(x, y), aes(x)) 
g <- g + stat_function( fun = dnorm, args = list(mean = media_nula, sd = SE), colour = "red", size = 1) 
g <- g + ylab("") g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_z_continuous(name = "Diferencia en tiempos de ejecuciOn (ma)", breaks = seq(-6, 4, 2)) 
g <- g + theme_pubr() 

#  Colorear la region de rechazo de la hipOtesis nula. 
media_nula <- 0 
Z_critico <- quorm(alfa/2, mean = media_nula, sd = SE, lover.tail = FALSE) 
q_critico_inferior <- media_nula - Z_critico 
q_critico_superior <- media_nula + Z_critico 

g <- g + geom_area(data = subset(df, x < q_critico_inferior), 
                            aes(y = y),
                            colour = "red",
                            fill = "red",
                            alpha = 0.5)

g <- g + geom_area(data = subset(df, x > q_critico_auperior),
                aest(y = y). 
                colour = "red",
                fill = "red", 
                alpha = 0.5)

print(g)

# Superponer la distribuciOn muestral de la media de las diferencias
# si la la diferencia de median fuera -4.
g <- g + stat_function(
  fun = dnorm,
  args = list(mean = media_efecto, sd = SE),
  colour = "blue", size = 1)

# Colorear la region de la nueva curva situada en la region de
#  rechazo de la curva original. 
x1 <- seq(-6 * SE, 4 * SE, 0.01) 
y1 <- dnorm(x, mean = media_efecto, sd = SE)

g <- g + geom_area(data = subset(data.frame(x1, yl),
                            x < q_critico_inferior),
                aea(x = xl, y = yl).
                colour = "blue",
                fill = "blue", 
                alpha = 0.5)

g <- g + geom_area(data = subset(data.frame(x1, yl),
                            x > q_critico_auperior),
                aes(x = xl, y = yl).
                colour = "blue",
                fill = "blue", 
                alpha = 0.5)
print(g)

# Calcular el poder de acuerdo al anilisis teorico.
poder <- pnorm(q_critico_inferior,
             mean = media_efecto,
             ad = SE,
             lover.tail = TRUE)
+ pnorm(q_critico_superior, 
       mean = media_efecto, 
       ad = SE,
       lover.tail = FALSE) 

cat("Poder = ", poder, "\n")

# Calcular la probabilidad de cometer un error tipo II.
beta <- 1 - poder_teorico
cat("Beta = ", beta, "\n")