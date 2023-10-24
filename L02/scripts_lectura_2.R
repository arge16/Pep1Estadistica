###################Variables aleatorias discretas###################

#Trabajar con variables aleatorias discretas en R

library(discreteRV)

#Crear una variable aleatoria discreta para representar el dado
# adulterado de la tabla 3.1
resultados <- 1:6
probabilidades <- c(0.25, 0.125, 0.125, 0.125, 0.125, 0.25)
X<-RV(outcomes = resultados, probs = probabilidades)
#Calcular el valor esperado
esperado <- E(X)
cat("El valor esperado es:", esperado, "\n")
#Calcular la varianza
varianza <-V(X)
cat("La varianza es:", varianza, "\n")
#Calcular la desviación estándar
desviacion <- SD(X)
cat("La desviación estándar es:", desviacion, "\n")


#Distribucion Normal

dnorm(x, mu, sigma) #Funcion de densidad de una Distribucion normal, te da la probabilidad puntual de que ocurra x en una distribucion normal de media mu y desv sigma
pnorm(x,mu,sigma,lower.tail) #funcion de distribucion de una distribucion normal, te da la probabilidad acumulada de que ocurra x en una distribucion normal de media mu y desv sigma, cola inferior es decir <=
qnorm(probabilidad_deseada,mu,sigma) #Entrega el valor de X que acumula la probabilidad deseada
rnorm(n,mu,sigma) #Simula una distribucion normal de tamaño n y parametors mu y sigma

#Por ej si te piden prob de que x sea menor a 8 tengo que usar pnorm ya que es la que te entrega la prob acumulada
#Si te piden el valor que te acumula el 30% inferior, tengo que usar qnorm, esta te da los cuantiles, tu le das la prob y le dices que es la infreior y te da el valor que esta como maximo en ese 30% inferior
#X=Valor al que se busca la probabilidad
#mu = media
#sigma = desviacion estandar

#Ejemplo, mu=10, sigma= 7

#P(x=5) Cual es la prob en una distrb normal de media 5 y sd 7 de que x= 5
dnorm(5,10,7)

#P(x<=4) Cual es la prob en una distrb normal de media 5 y sd 7 de que x<= 4
pnorm(4,10,7,lower.tail = TRUE)

#P(x>=4)  Cual es la prob en una distrb normal de media 5 y sd 7 de que x>= 4
pnorm(4,10,7,lower.tail = FALSE)
#No entrega hacia abajo sino que 4 o mayor a 4

#validacion, debeia ser 1
pnorm(4,10,7,lower.tail = TRUE) + pnorm(4,10,7,lower.tail = FALSE)

#P(x<=a) = 0.3, cual es el valor de a
#osea si te dicen, quiero saber el valor de a que hace que se me acumule hacia abajo un 30% de la probabilidad, entonces tengo que usar qnorm
qnorm(0.3,10,7)

#Validacion
pnorm(6.329196,10,7,lower.tail = TRUE)
#deberia ser 0.3

#P(x>=a) = 0.3, cual es el valor de a
qnorm(0.3,10,7,lower.tail = FALSE)

#Validacion
pnorm(13.6708,10,7,lower.tail = FALSE)
#deberia ser 0.7 aprox

#simulacion de n=1000, mu=10, s
x1<-rnorm(1000,10,7)
mean(x1)
sd(x1)
hist(x1)