#####################Importar conjunto de datos#####################
#Cargar conjunto de datos disponible en R
datos<-mtcars
#Importar desde un arcivo de valores separados por coma en formato ingles
datos3 <- read.csv("C:/Users/Usuario/Desktop/curso R/mtcars.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
#Configurar carpeta de trabajo
setwd("C:/Users/Usuario/Desktop/curso R")
#Mostrar las primeras 6 filas del conjunto de datos
head(datos3)    
#Mostrar las últimas 6 filas del conjunto de datos
tail(datos3)

################################Construir un dataframe##############################
#crear un vector de strings y guardarlo en variable nombre
nombre<-c("Juan","Pedro","Maria","Luisa","Ana","Carlos")
#crear vector de fechas y guardarlo en variable fecha_nacimiento
fecha_nacimiento<-as.Date(c("1990-01-01","1991-02-02","1992-03-03","1993-04-04","1994-05-05","1995-06-06"))
#Crear tres vectores de reales entre 1.0 y 7.0 y guardarlos en prueba_i, respectivamente
prueba_1<-c(5.5,3.4,4.5)
prueba_2<-c(6.5,4.4,5.5)
prueba_3<-c(7.5,5.4,6.5)
#construir un dataframe a partir de los vectores anteriores y guardarlo en la variable dataframe
dataframe<-data.frame(nombre,fecha_nacimiento,prueba_1,prueba_2,prueba_3, stringsAsFactors = FALSE)
#Mostrar el dataframe
dataframe
#Mostrar la estructura del dataframe
str(dataframe)
#guardar el dataframe en un archivo de valores separados por coma(formato español)
write.csv2(dataframe, "C:/Inferencia/Ejemplo.csv", row.names = FALSE)

#####################Importar datos desde un archivo de texto#####################
#Importar desde un archivo de texto en formato español
datos2 <- read.table("C:/Users/Usuario/Desktop/curso R/mtcars.txt", header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)
#Mostrar las primeras 6 filas del conjunto de datos
head(datos2)
#Mostrar las últimas 6 filas del conjunto de datos
tail(datos2)
#Mostrar la estructura del dataframe
str(datos2)
#guardar el dataframe en un archivo de valores separados por coma(formato español)
write.csv2(datos2, "C:/Users/Usuario/Desktop/curso R/mtcars.csv", row.names = FALSE)


#####################Modificaciones de una matriz de datos#####################
#Leer un dataframe desde archivo csv
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/mtcars.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
#Eliminar del data frame la columna fecha_nacimiento
datos$fecha_nacimiento<-NULL
#Agregar al data frame la columna edad
datos$edad<-c(20,21,22)
#Crear una nueva observacion
nueva<- data.frame(nombre="Luis", prueba_1=5.5, prueba_2=6.5, prueba_3=7.5, edad=23)
#Agregar la nueva observacion al data frame
datos<-rbind(datos,nueva)
#guardar el dataframe en un archivo de valores separados por coma(formato español)
write.csv2(datos, "C:/Users/Usuario/Desktop/curso R/mtcars.csv", row.names = FALSE)

#####################Modificaciones de una matriz de datos con paquete dplyr#
library(dplyr)
#cargar datafram iris incluido en R
datos<-iris
#Seleccionar observaciones correspondientes a la especie versicolor
versicolor <- datos %>% filter(Species == "versicolor")
#Seleccionar observaciones de la especie versicolor cuyos sepalos tengan longitud o igual o superior a 6cm
largas <- datos %>% filter(Species == "versicolor" & Sepal.Length >= 6)
#Seleccionar la especie y variables relativas a los petalos
petalos <- datos %>% select(Species, starts_with("Petal"))
#Seleccionar variables de ancho y la especie
anchos<-datos %>% select(ends_with("Width"),Species)
#Agregar al conjunto de datos de los petalos una nueva variable con la razon entre el largo y el ancho de estos
petalos<- petalos %>% mutate(Species, Petal.Width, Petal.Ratio = Petal.Length/Petal.Width)
#Ordenar el conjunto de datos de petalos en forma descendente segun la razon de los petalos
petalos<-petalos %>% arrange(desc(Petal.Ratio))
#Ordenar el conjunto de datos de petalos en forma ascendente segun el largo de los petalos
petalos<-petalos %>% arrange(Petal.Length)

#####################Modificaciones de un conjunto de datos mtcars para facilitar su comprension#
library(dplyr)
#cargar datafram mtcars incluido en R
datos<-mtcars
#Renombrar columnas
datos<-datos %>% rename(Rendimiento = mpg, 
                        Cilindros = cyl, 
                        Desplazamiento = disp, 
                        Potencia = hp, 
                        Eje = drat,
                        Peso = wt, 
                        CuartoMilla = qsec,
                        Motor = vs,
                        Transmision = am, 
                        Cambios = gear, 
                        Carburadores = carb)
#Dar formato categorico a las variables motor y Transmision, renombrando sus niveles
datos[["Motor"]]<-factor(datos[["Motor"]], levels = c(0,1), labels = c("V","Recto"))
datos[["Transmision"]]<-factor(datos[["Transmision"]], levels = c(0,1), labels = c("Automatica","Manual"))
#Dar formato ordinal a las variables Cilindrada y Cambios, renombrando sus niveles
datos[["Cilindros"]]<-factor(datos[["Cilindros"]], levels = c(4,6,8), labels = c("4 cilindros","6 cilindros","8 cilindros"), ordered = TRUE)
datos[["Cambios"]]<-factor(datos[["Cambios"]], levels = c(3,4,5), labels = c("3 cambios","4 cambios","5 cambios"), ordered = TRUE)
#Guardar el dataframe en un archivo de valores separados por coma(formato español)
write.csv2(datos, "C:/Users/Usuario/Desktop/curso R/mtcars.csv", row.names = FALSE)



#########################MEDIDAS DE TENDENCIA CENTRAL()##########################
#cargar conjunto de datos
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/mtcars.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
#Calcular la media para la variable Rendimiento.
media<-mean(datos[["Rendimiento"]])
cat("Rendimiento medio: ",media,"\n")
#Calcular la media para la tercera y quinta columas (variables Desplazamiento y Eje)
cat("Mediasd\n")
print(sapply(datos[c(3,5)],mean))
cat("\n")
#Calcular la media para las columnas 3 a 6 (Variables Desplazamiento, Potencia Eje y Peso)
cat("Medias\n")
print(sapply(datos[3:6],mean))
cat("\n")
#Calcular la media para la variable Rendimiento omitiendo valores faltantes
print(mean(datos[["Rendimiento"]],na.rm = TRUE))
#Calcular la mediana para la variable Rendimiento
print(median(datos[["Rendimiento"]]))
#Calcular la moda para la variable Rendimiento
print(table(datos[["Rendimiento"]]))
#Calcular la moda para la variable Rendimiento usando mfv() del paquede modeest
library(modeest)
print(mfv(datos[["Rendimiento"]]))


#########################MEDIDAS DE DISPERSION()#################################
#Importante conocer la variabilidad o dispersion pues asi se puede saber que tan semejantes o diferentes son las observaciones entre si.
#Suelen calcularse en base a la desviacion que es la distancia entre una observacion y la media dle conjunto de datos.
#Calcular la desviacion estandar para la variable Rendimiento ()
print(sd(datos[["Rendimiento"]]))
#Calcular la varianza para la variable Rendimiento
print(var(datos[["Rendimiento"]]))
#Calcular la varianza para las columnas 3 a 6 (Variables Desplazamiento, Potencia Eje y Peso)
print(sapply(datos[3:6],var))
#Calcular el rango para la variable Rendimiento
print(range(datos[["Rendimiento"]]))
#Calcular minimo y maximo para la variable Rendimiento
print(min(datos[["Rendimiento"]]))
print(max(datos[["Rendimiento"]]))


#######################Calculo de cuantiles######################################
#Cargar conjunto de datos
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = FALSE)
#Calculo de cuartiles
cat("Cuartiles\n")
print(quantile(datos[["Rendimiento"]]))
#Calculo de quintiles
cat("Quintiles\n")
print(quantile(datos[["Rendimiento"]], seq(0,1,0.2)))
#Calculo de deciles
cat("Deciles\n")
print(quantile(datos[["Rendimiento"]], seq(0,1,0.1)))
#Calculo de percentiles
cat("Percentiles\n")
print(quantile(datos[["Rendimiento"]], seq(0,1,0.01)))
#Calculo de rango intercuartilico, el IQR es mas ribusta que la desviacion estandar en presencia de valores atipicos, pues no se ve afectado por estos.
#es una mejor medida de dispersion, y la mediana es una mejor medida de tendencia central que la media en presencia de valores atipicos.
cat("Rango intercuartilico\n")
print(IQR(datos[["Rendimiento"]]))
#Calculo de rango intercuartilico usando las funciones quantile() y diff()
cat("Rango intercuartilico\n")
print(diff(quantile(datos[["Rendimiento"]], c(0.25,0.75))))

#########################Uso de la funcion summarise()#############################
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = FALSE)
#Calculo de varias medidas para la variable Potencia
medidas_potencia<- datos %>% summarise(media=mean(Potencia), mediana=median(Potencia), varianza=var(Potencia), desviacion=sd(Potencia), rango=range(Potencia), minimo=min(Potencia), maximo=max(Potencia), IQR=IQR(Potencia))

#############################Estadistica ddescriptiva para datos categoricos#################################
#Frecuencia: cantidad de veces que podemos encontrar cada nivel de la variable en los datos.
#Proporcion, frecuencia relativa, corresponde a frecuencia de un nivel de la variable dividida por el numero total de observaciones.
#Para este tipo de datos la mejor alternativa es la tabla de contingencia, matriz de confusion o tabla de frecuencias.
#Cargar datos   
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = FALSE)
#Crear tabla de contingencia para la variable Cambios (table no muestra los nombre de variables tabuladas)
contingencia<-table(datos[["Cambios"]])
cat("Tabla de contingencia\n")
print(contingencia)
cat("\n")

#Crear tabla de contingencia para la variable Cambios con nombres de la variable tabuladas
contingencia2<-xtabs(~Cambios, data=datos)
cat("Tabla de contingencia\n")
print(contingencia2)
cat("\n")

#Mostrar totales por fila y mostrarlos por separados
totales<-marginsums(contingencia)
cat("Totales por fila\n")
print(totales)
cat("\n")

#Calcular totales por fila y agregarlos a la tabla
con_totales<-addmargins(contingencia,1)
cat("Tabla de contingencia con totales por fila\n")
print(con_totales)
cat("\n")

#Convertir a table de proporciones
proporciones<-prop.table(contingencia)
proporciones<-addmargins(proporciones,1)
cat("Tabla de contingencia con proporciones por fila\n")
print(proporciones)
cat("\n")

#Estadisticas descriptivas para datos agrupados, esto agrupa las observaciones para 3,4 y 5 cambios
# y muestra las medidas de tendencia central y dispersion para cada grupo, asi como el IQR.
datos<-mtcars
resumen<-group_by(datos,gear)%>%summarise(count=n(),mean(mpg), median(mpg),sd(mpg),IQR(mpg), mean(mpg))
print(resumen)

##########################Graficos para variables numericas##############################

#Histograma, util para representar una unica variable numerica y la muestra es grande, podemos decir que este grafico
#muestra una aproximacion de la densidad (o distribucion de frecuencias) para la variable. Para lo que tenemos que 
#dividir el rango de valores posibles en intervalos (generalmente iguales), y luego contar la cantidad de observaciones en cada intervalo.

library(ggpubr)
#Cargar datos
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = TRUE, row.names=1)
#Crear histograma para la variable Rendimiento
g1<-gghistogram(datos,
                x="Rendimiento",
                bins=10,
                add="mean",
                xlab = "Rendimiento [Millas/galon]",
                ylab = "Frecuencia",
                color = "blue",
                fill = "blue")
print(g1)

#Histograma para la variable Potencia
g2<-gghistogram(datos,
              x="Potencia",
              bins = 10,
              add="mean",
              xlab = "Potencia [HP]",
              ylab = "Frecuencia",
              color = "red",
              fill = "yellow") 
print(g2)


#Boxplot, util para representar variable numerica ya que considera 5 estadisticos: minimo, primer cuartil, mediana, tercer cuartil y maximo.
#Ademas facilita la identificacion de valores atipicos. Los extremos inferior y superior del rectangulo o caja de la figura corresponden
#Al primer y tercer cuartil respectivamente, la linea que divide la caja corresponde a la mediana, Asi la caja engloba el 50% central de los datos.
#Y la altura de la caja corresponde al rango intercuartil. Las barras que se extienden por sobre y debajo de la caja se llaman
#bigotes, capturan aquellos datos fuera de la caja central y que esten situados a no mas de 1.5 veces el IQR.
#Cualquier observacion que este mas alla de la caja y los bigotes se repreesnta con un punto, el cual es un outlier.

library(ggpubr)
#Cargar datos
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = TRUE, row.names=1)
g<-ggboxplot(datos[["Potencia"]],
             color = "red",
             fill = "pink",
             ylab = "Potencia [HP]",)

g<- g+rremove("x.ticks")
g<- g+rremove("x.title")
g<- g+rremove("x.text")
print(g)


################################Graficos para variables categoricas##############################

#Grafico de barras, util para representar variable categorica.
#Si queremos representar una unica variable categorica, lo mas adecuado es usar un grafico de barras,
#Pues cada barra es tan larga como la proporcion de valores presentes en cada nivel de la variable.
library(ggpubr)
#Cargar datos
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = TRUE, row.names=1)
#Crear la tabla de frecuencias para la variable Cambios y convertirla a dataframe
contingencia<- as.data.frame(xtabs(~Cambios, data=datos))
#Crear grafico de barras para la variable Cambios
g<-ggbarplot(contingencia,
             x="Cambios",
             y="Freq",
             fill = c("brown","purple","orange"),
             title = "Cantidad de cambios de los automoviles",
              xlab = "Cantidad de cambios",
             ylab = "Frecuencia")
print(g)

#grafico de torta, util para representar variable categorica.
datos<-read.csv2("C:/Users/Usuario/Desktop/curso R/Mtcars.csv", stringsAsFactors = TRUE, row.names=1)
#Crear la tabla de frecuencias para la variable Cambios y convertirla a dataframe
contingencia<- as.data.frame(xtabs(~Cambios, data=datos))

#Crear grafico de barras para la variable Cambios
g<-ggpie(contingencia,
             x="Freq",
             label ="Cambios",
             fill = c("red","yellow","greed"),
             title = "Cantidad de cambios de los automoviles",
            lab.pos = "in")
print(g)
