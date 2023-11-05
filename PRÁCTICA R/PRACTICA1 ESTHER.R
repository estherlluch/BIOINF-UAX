#############################################################################
#
# PRACTICA 1 Esther Lluch
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
## ENTREGA EL 01 OCTUBRE 23:59
## Se requiere la entrega de este script completado con los códigos más las imágenes y las respuestas a las preguntas
## Adjuntar en la entrega el PDF final y el archivo con los genes
#
##############################################################################

# Instalar RCurl



# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# En la VARIABLE "DATA" están los datos. Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim (data) #mide las dimensiones de los datos, en este caso de (data)
head (data) #vemos la primera parte de los datos
tail (data) #vemos la última parte de los datos


# Hacemos un primer histograma para explorar los datos
hist (data) #crea un histograma de los datos escogidos

# Transformamos los datos con un logaritmo.
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve? Podremos conseguir un histograma con los datos mejor distribuidos y conseguir una imagen más visible
data_log = log2 (data) #para guardar la transformación logarítimica en una variable a parte. 
hist (data_log) #creamos un histograma nuevo con la transformación logarítimica.  OJO! Solo se puede utilizar log para crear imágenes. 

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
boxplot (data_log) #creación de un Box Plot con los datos. 
boxplot (data_log, col=c("blue", "blue", "blue", "orange", "orange", "orange")) #Cambio de los colores de los boxplot, según son WT o KO. Azul=WT y Naranja=KO
boxplot (data_log, col=c("blue", "blue", "blue", "orange", "orange", "orange"), main="GSE5583-boxplots", las=2) #Cambio del título del box plot con "main", y modificación de los nombres de la parte inferior para que estén en vertical por medio de "las"
# ¿Qué es un boxplot? Método estandarizado para representar gráficamente una serie de datos numéricos a través de sus cuartiles. Se representa la mediana y los cuartiles de los datos, incluso los valores atípicos. 


# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación
hc=hclust(as.dist(1-cor(data_log))) #Se crea la variable para realizar un hierarchical clustering
plot(hc, main="Clustering") #Se crea el clustering, representando "hc"
# de los valores de expresión. ¿Es correcta la separación? Si, ya que se separan los diferentes grupos, el WT y el KO


#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado? Una matriz
# Vamos a generar dos grupos, uno de WT 1-3 y otro de KO 4-6
wt <- data [,1:3] #generamos el grupo de WT, utilizamos la coma para escoger solo las columnas, la 1, 2 y 3. OJO! Ponemos "data" y ya no utilizamos data_log
ko <- data [,4:6]
class(wt) #nos indica el tipo de datos que tenemos, en este caso una matriz
head(wt) #muestra los primeras líneas 


# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean=apply(wt, 1, mean) #con "apply" calculamos donde queramos, lo que queramos. En este caso calculamos la media de cada gen, en el grupo WT. El "1" hace referencia a la FILA.
wt.mean #vemos las medias de cada gen que se han calculado
head (wt.mean) #observamos el principio de nuevo 

ko.mean=apply(ko, 1, mean) #calculamos la media de los KO
ko.mean #vemos el resultado 
head (ko.mean)

# ¿Cuál es la media más alta? #observamos cuál es la media más alta. Podemos comparar entre grupos de WT y KO cuál presenta la media más elevada
max (wt.mean) #35375,73
max (ko.mean) #37460,5

# Ahora hacemos un scatter plot (gráfico de dispersión)
#Compara medias del Eje X con el Y. Muestra la relación entre dos variables
plot (ko.mean~wt.mean)
plot (ko.mean~wt.mean, xlab="WT", ylab="KO", main = "GSE5583 - Scatter") #Cambiamos el título del Scatter, y el nombre de los ejes (x,y) por WT y KO


# Añadir una línea diagonal con abline
# Es importante saber que debe estar abierto el plot para ejecutar abline. Podemos poner cualquier color para la línea.
# La b va primero y la a después en el abline (y = ax+b). Dibujaremos una recta diagonal en el plot. 
abline (0,1,col ="red")

abline (h=2, col ="blue") #en este caso, trazaremos una línea horizontal
abline (v=5, col="green") #en este caso, trazaremos una línea vertical

# ¿Eres capaz de añadirle un grid?
grid () #para añadir una cuadrícula en el fondo del plot

# Calculamos la diferencia entre las medias de las condiciones. Recuerda que las medias de cada variable estaban ya calculadas (mean). 
diff.mean = wt.mean - ko.mean #cálculo de la diferencia de las medias 
diff.mean #para comprobar el resultado. 

# Hacemos un histograma de las diferencias de medias
hist (diff.mean) #vemos el histograma de la diferencia de las medias
hist (diff.mean,col = "blue") #cambiamos el color


# Calculamos la significancia estadística con un t-test. 
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test. Como de testimonio. 
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué? Si cambiamos los datos a log estaríamos manipulando los datos. Solo se deben usar log para las gráficas.
# ¿Cuántas valores tiene cada condición? Dentro de cada condición hay 3 valores (3 muestras)
# Ahora comprobamos que hemos hecho TODOS los cálculos. Intentar correr todo el script de arriba abajo sin error, es decir el script funciona. 

pvalue = NULL #para crear la lista
tstat = NULL
for (i in 1 : nrow (data)) { #para cada gen
  x = wt [i,] #gene wt número i. Estamos utilizando data y la variable wt (vector). Coge todas las columnas. 
  y = ko [i,] # gene ko número i. Lo mismo con la variable ko. Guardamos cada condición (WT o KO) en una variable diferente.
  
  #Hacemos el test por cada fila, de cada test sacamos un p-value.
  t = t.test (x,y) #creamos la variable t, hacemos el test con las dos variables. 
  
  #Añadimos el p value a la lista
  pvalue [i] = t$p.value #nos indica los p.value y nos da las estadísticas. 
  
  #Añadimos las estadísticas a la lista
  tstat [i] = t$statistic #con el índice de la fila. 
}

head(pvalue) #comprobamos los valores de p value. Ningún gen presenta una diferencia significativa, ya que los valores son >0,05. 
length(pvalue) #el valor que nos dé será el número de genes que tenemos. 
  


# Hacemos un histograma de los p-values.
hist(pvalue) #no refleja la proporción de la distribución
# ¿Qué pasa si le ponemos con una transformación de -log10?
hist (-log10 (pvalue), col = "pink") #mejor distribución de los datos en el histograma. Ahora la mayoría de los datos están en el cero, lo significativo es una parte muy pequeña. 

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot (diff.mean, -log10 (pvalue), main = "GSE5583 - Volcano") #es otro tipo de gráfico de dispersión, donde analizamos la diferencia de medias en contra del logaritmo. 
#en este caso los valores significativos estarán del 3 para arriba aproximadamente.

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de medias de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2 # diferencia de medias
pvalue_cutoff = 0.01 #ajustamos el pvalue a un valor más bajo, en este caso 0,01
abline (v= -log10 (pvalue_cutoff), col = "blue", lwd = 3) #convertimos los valores de cutoff a log
#vamos a marcar con una línea vertical (azul) para diferenciar los valores significativos. El lwd es el grosor de la línea
abline (h= -log10 (pvalue_cutoff), col = "green", lwd =3) #trazamos línea horizontal (verde). Los datos significativos estarán en la franja. 


# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff #la diferencia de media hemos puesto que será 2. Convertimos en la tabla de diferencias de media a valor absoluto para poder filtrar. 
#Filtramos los datos por la diferencia de medias
dim (data [filter_by_diff.mean, ]) #encontraremos los nombres de los genes que pasen el filtro. 
 
# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff #filtramos por el siguiente filtro, en este caso el pvalue (0.01)
dim (data [filter_by_pvalue, ]) #vemos los datos que han pasado los datos

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios? 426 genes lo cumplen
filter_combined = filter_by_diff.mean & filter_by_pvalue #combinamos los dos filtros que hemos creado
filtered = data [filter_combined,] #creamos la nueva variable que solo nos dará los datos filtrados 
dim (filtered) #vemos la dimensión, la cual coincide con el filtro por pvalue
head (filtered) #comprobamos


# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot (diff.mean, -log10 (pvalue), main = "GSE5583 - Volcano #2") #cambio de nombre
points (diff.mean [filter_combined], -log10 (pvalue[filter_combined]), col = "red") #datos significativos en rojo

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés? Por los valores negativos de los sobreexpresados, y los valores positivos de los reprimidos
plot (diff.mean, -log10 (pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean [filter_combined & diff.mean < 0],
        -log10 (pvalue [filter_combined & diff.mean < 0]), col = "red")
points(diff.mean [filter_combined & diff.mean > 0],
       -log10 (pvalue [filter_combined & diff.mean > 0]), col = "blue")
#parece que está al revés.Los sobreexpresados son los KO que tienen 2 veces más la expresión que el WT, y los reprimidos la mitad del WT.
#en la gráfica, los sobreexpresados están en el lado negativo; y los reprimidos en la parte positiva. 


# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap? 
heatmap (filtered) #creamos el heatmap con los datos ya filtrados
#para agruparlo y colocarlo correctamente, agrupa los WT por un lado y los KO por otro,y entre las dos mitades de arriba y abajo nos distingue entre sobreexpresados y reprimidos 
rowv = as.dendrogram (hclust (as.dist (1-cor (t(filtered)))))
colv = as.dendrogram (hclust (as.dist(1-cor(filtered))))
heatmap (filtered, Rowv = rowv, Colv = colv, cexCol=0.7, labRow =FALSE) #*labRow=FALSE es para quitar el nombre de los genes

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#Podemos instarlos de esta forma: 
if (!requireNamespace("BiocManager"))
install.packages("BiocManager")
BiocManager::install(c("gplots","RColorBrewer"))

# o de la segunda forma
install.packages("gplots")		
install.packages("RColorBrewer")	

#Cargamos los paquetes instalados
library(gplots)

library (RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col = rev(redblue(256)), scale = "row", labRow=FALSE)


# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2 (filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col = rev(redblue(256)), scale = "row", labRow = FALSE) #nuevo heatmap con distribución en colores rojo y azul
dev.off ()
heatmap.2 (filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col = redgreen(75), scale = "row", labRow=FALSE) #colores rojo y verde


# Guardamos los genes diferencialmente expresados y filtrados en un fichero
class (filtered) #vemos de nuevo el tipo de archivo que tenemos
write.table(x=filtered, file = "filteredDT.txt", sep = ",", row.names = FALSE, col.names =TRUE) #creamos el archivo "filteredDT.txt" con nuestros genes filtrados, separados por comas, nombrando las columnas
write.table(x=filtered, file = "filteredDTT.txt", sep = "\t", row.names = FALSE, col.names =TRUE) #otra forma de guardar el archivo, en este caso con tabulaciones para separar los datos
