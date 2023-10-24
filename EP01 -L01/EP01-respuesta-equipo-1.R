library(ggplot2)

# Leer el csv EP01 Datos Casen 2017.csv y crear un data frame con las variables
datos <- read.csv("EP01 Datos Casen 2017.csv", header = TRUE, sep = ";", dec = ",")

# Descripción de los datos:
# folio: identificador del hogar: (Tipo: Categórico Nominal)
# o: número de orden de la persona dentro del hogar (Tipo: Categórico Ordinal) 
# id.vivienda: identificador de la vivienda (Tipo: Categórico Nominal)
# hogar: identificación del hogar en la vivienda (Tipo: Categórico Nominal)
# region: región (Tipo: Categórico Nominal)
# provincia: provincia (Tipo: Categórico Nominal)
# comuna: comuna (Tipo: Categórico Nominal)
# ing.comuna: posición en el ranking histórico del ingreso de la comuna (ascendente) (Tipo: Categórico Ordinal)
# zona: área geográfica (Urbano, Rural) (Tipo: Categórico Nominal)
# sexo: sexo de la persona registrada (Tipo: Categórico Nominal)
# edad: edad de la persona registrada (Tipo: Numérico Discreta)
# ecivil: estado civil de la persona registrada (Tipo: Categórico Nominal)
# ch1: situación ocupacional de la persona registrada (Tipo: Categórico Nominal)
# ytot: ingreso total (Tipo: Numérico Discreta)

# PREGUNTA
# ¿Se encuestaron más o menos la misma cantidad de gente en cada provincia de la RM?
# Respuesta: No, se encuestaron más personas en la provincia de Santiago, aunque se puede notar que en las demás provincias se encuestaron una cantidad similar de personas.

# Discutir y consensuar qué medidas estadísticas (media, mediana, moda, etc.) y qué forma gráfica ayudaría a responder la pregunta asignada.

# Medida estadística seleccionada: Frecuencia y Proporción
# Se utiliza la frecuencia y la proporción ya que se esta trabajando con un tipo de dato cualitativo, en este caso la provincia, y se quiere saber la cantidad de personas encuestadas en cada provincia de la RM.
# Se puede notar de la tabla de frecuencia y proporción que se encuestaron más personas en la provincia de "Santiago" en relación a las demás provincias de la RM.

# Se crean las variables frec y prop, que corresponden a la frecuencia y proporción de las provincias de la RM respectivamente.
frec <- as.numeric(table(datos$provincia))
prop <- as.numeric(table(datos$provincia)/sum(table(datos$provincia)))*100

# Se toman los valores de la columna provincia y se eliminan los valores repetidos
provincia <- unique(datos$provincia)

# Se ordenan las provincias de forma que se ajusten a la tabla de frecuencia
provincia_ordenada <- sort(provincia)

# Se crea una dataframe con las variables provincia_ordenada, frec y prop
tabla_frec <- data.frame(Provincia = provincia_ordenada, Frecuencia = frec, Proporcion = prop)

# Se imprime la tabla de frecuencia y proporción
knitr::kable(tabla_frec)

# Gráfico:
graf2 <- ggplot(datos, aes(x = provincia)) +
  geom_bar() + # Se utiliza geom_bar() ya que se esta trabajando con un tipo de dato cualitativo
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size=3) + # Se utiliza geom_text() para agregar la frecuencia de cada barra
  labs(x = "Provincia", y = "Cantidad de encuestado") + # Se agregan los nombres de los ejes
  theme_bw() 

plot(graf2)

# Se considera que el gráfico adecuado para responder la pregunta es un gráfico de barras, ya que se puede apreciar la cantidad de personas encuestadas en cada provincia de la RM.
# En el gráfico de barras se puede observar una clara diferencia en las alturas de las barras, con una gran altura específicamente en la correspondiente a la provincia "Santiago"
# Por lo tanto se puede concluir del gráfico que no se encuestaron más o menos la misma cantidad de gente en cada provincia de la RM, se encuestarón más personas de la provincia "Santiago"
  