#############################################################################
# TRABAJO R Esther Lluch
library (RCurl) #cargamos los paquetes
data <- read.table ("datos-trabajoR.txt", header = TRUE, sep = "\t") #cargamos los datos de nuestro archivo .txt, ahora se llamará "data" (con tabulaciones)
dim (data) #dimensiones de nuestros datos = 3 columnas, 50 filas de datos
head (data) #inicio del archivo de datos
tail (data) #final del archcivo
summary (data) #nos ofrece un resumen sobre nuestros datos, como la mediana, la media, cuartiles, min/max... 
str (data) #nos proporciona detalles sobre la estructura del dataframe, incluyendo el tipo de datos (variables), las columnas y algunos ejemplos de los valores de estas.
View(data) #directamente vemos una tabla con nuestros datos

# ¿Cuántas variables hay?
# Encontramos "Variable 1", "Variable 2" y "Tratamiento". 3 variables pero con diferencias en sus contenidos. Aunque también se podría considerar como variables solamente "Variable 1" y "Variable 2". 
#Variable 1 y 2 contienen datos numéricos en sus respectivas columnas, en cambio, "Tratamiento" aunque contiene datos numéricos hace referencia a un caracter "tto tipo 1", "tto tipo 2", etc. (Variable categórica) 
# ¿Cuántos tratamientos hay? Hay 5 tipos de tratamiento

#Separamos los datos según sea Tratamiento / Variable 1 / Variable 2
Tratamiento <- data [,1] #datos en la columna 1
Variable1 <- data [,2] #datos en la columna 2
Variable2 <- data [,3] #datos en la columna 3


##BOX PLOT 
boxplot (Variable1~Tratamiento, col = c("lightblue"))
boxplot (Variable2~Tratamiento, col = c("lightyellow"))


##GRAFICO DE DISPERSION con las DOS VARIABLES. Cada tratamiento con un color distinto. 
plot(x=Variable1, y=Variable2, col=Tratamiento) #creamos el scatter plot, con cada grupo de tratamiento de un color diferente

##CREAMOS UNA LEYENDA
legend(x = "bottomright", legend = c("Tto 1", "Tto 2", "Tto 3", "Tto 4", "Tto 5"), 
       fill = c("black", "red", "green", "cyan", "blue"), title = "Tratamientos")


##HISTOGRAMA
hist (Variable1, col = c("lightblue"))
hist (Variable2, col = c("lightyellow"))
#En el eje x = valores posibles de la variable que se está analizando, divididos en intervalos. Cada barra vertical representa a un rango de valores. 
#En el eje y = frecuencia/conteo de observaciones que caen dentro de cada intervalo (cuántas veces aparece un valor dentro de un rango específico).
#Se representa la densidad de la distribución en rangos determinados. 
#También podemos observar la forma de distribución de los datos y si hay valores atípicos.
#Tanto en la V1 como en la V2 sigue una distribución asimétrica, con múltiples picos (modas)

##FACTOR en COLUMNA de TRATAMIENTO
FactorTratamiento <- factor(data$Tratamiento)
FactorTratamiento
#Encontramos 5 niveles de tratamiento (5 grupos)
#Los factores se utilizan para representar variables categóricas o nominales, en este caso grupos de tratamiento.
#Útiles para trabajar con datos que tiene un nº finito y predefinido de categorías o niveles. 



##MEDIA y DESVIACIÓN ESTÁNDAR 
# Usando la función aggregate para calcular la media y desviación estándar
resultados_aggregateV1 <- aggregate(Variable1 ~ FactorTratamiento, data = data, 
                                  FUN = function(x) c(media = mean(x), desviacion_estandar = sd(x)))
resultados_aggregateV1

resultados_aggregateV2 <- aggregate(Variable2 ~ FactorTratamiento, data = data, 
                                  FUN = function(x) c(media = mean(x), desviacion_estandar = sd(x)))
resultados_aggregateV2
#En los resultados podemos ver los datos agrupados según el tipo de tratamiento. Obtenemos la media y la desviación estándar para cada tratamiento.
#Una tabla para la variable 1 y otra tabla para la variable 2.

##¿CUANTOS ELEMENTOS TIENE CADA TRATAMIENTO?
#Cada grupo de tratamiento tiene 10 elementos, (10 datos en la variable 1 y otros 10 datos en la variable 2)
table (FactorTratamiento)

##EXTRACCION DE DATOS
#TRATAMIENTO 1
tto1V1 <- data [1:10,2] #extraccion de datos del tratamiento 1, de la variable 1. Especificamos en su nombre que son los datos del tto 1.
tto1V1


#TRATAMIENTO 4
tto4V1 <- data [31:40,2] #extraccion de datos del tratamiento 4, de la variable 1. Especificamos que son los datos del tto 4. 
tto4V1


##COMPROBAR DISTRIBUCIÓN NORMAL DE LOS DATOS (en este caso de la Variable 1, grupo de tratamiento 1 y grupo de tratamiento 4)
shapiro.test(tto1V1) #comprobamos si los datos se distribuyen de una forma normal en cada grupo de tratamiento
shapiro.test(tto4V1)

#Los datos seguirán una distribución normal si el p.value > 0.05
#Como el resultado de p=value es mayor a 0,05 ambos grupos siguen una distribución normal. 
#tto1V1 p-value = 0.06434     tto4V1 p-value = 0.1564
#Cuanto más pequeño sea el valor p, menos probable es que los datos provengan de una población normal.

##EN FUNCION DEL RESULTADO, ¿QUÉ TEST USARÍAS?
#Utilizamos el T-test (distribución T de Student)
#Técnica estadística utilizada para comparar las medias de dos grupos y determinar si hay evidencia suficiente para afirmar que las diferencias entre medias son estadísticamente significativas. 
t.test (tto1V1,tto4V1)

#Resultados: Valor t: -13.228 Nº de grados de libertad (df) es aproximadamente 9.2425.
#Valor p:extremadamente bajo, p ≈ 2.576e-07, lo que sugiere una diferencia altamente significativa entre los dos grupos.
#Intervalo de confianza (95%) para la diferencia de medias se proporciona como (-54.77165, -38.82835), lo que indica que la diferencia de medias se encuentra dentro de ese rango con un 95% de confianza.

##COMPROBACION HIPOTESIS NULA tto1=tto4
#Dado que el valor p es extremadamente bajo, se rechaza la hipótesis nula (media tto1 = media tto4 en la Variable1) en favor de la hipótesis alternativa.
#Esto significa que hay una diferencia significativa entre las medias de los dos grupos ("significancia estadística")
#En términos de los datos en sí, el grupo "tto1" tiene una media de aproximadamente 4.0, mientras que el grupo "tto4" tiene una media de aproximadamente 50.8.
#Estos valores indican una gran diferencia entre los dos grupos.

##VARIANZAS 
var.test (tto1V1,tto4V1)
#Considerando que Hipotesis Nula = igualdad de varianzas.
#Valor F (uso de prueba F) obtenido es 0.013476
#Valor p es muy bajo, p ≈ 4.595e-07, lo que sugiere que la diferencia entre las varianzas es altamente significativa (varianzas entre ambos grupos son muy diferentes).
#La razón de las varianzas se informa como 0.01347607
#Indica que la varianza del primer grupo (numerador) es mucho más pequeña que la varianza del segundo grupo (denominador).
#El conjunto de estos resultados sugieren que hay evidencia significativa para rechazar hipotesis nula


#También podemos calcular las varianzas por separado y observar los resultados. 
vartto1V1 <- var (tto1V1)
vartto1V1
cat("Varianza del tto 1:", vartto1V1, "\n") #(un poco más bonito)

vartto4V1 <- var (tto4V1)
vartto4V1
cat("Varianza del tto 4:", vartto4V1, "\n")

#Calculamos la diferencia entre varianzas, y coincide con las conclusiones del var.test
diferencia_varianzas <- vartto1V1 - vartto4V1
diferencia_varianzas
cat("DIFERENCIA de VARIANZAS:", diferencia_varianzas, "\n")
#Observamos que las varianzas entre tratamientos son muy diferentes

