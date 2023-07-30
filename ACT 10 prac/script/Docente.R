#Integrantes: Vivas Alcivar Luis Segundo

#--------Instalamos las librerias---------------------
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(magrittr)) {install.packages("magrittr")}
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(amap)) {install.packages("amap")}
if(!require(dendextend)) {install.packages("dendextend")}
if(!require(factoextra)) {install.packages("factoextra")}
if(!require(purrr)) {install.packages("purrr")}
if(!require(openxlsx)) {install.packages("openxlsx")}

# Para clustering particional
if(!require(cluster)) {install.packages("cluster")}
if(!require(NbClust)) {install.packages("NbClust")}

# --- Cargar librerías ---
# Utilizamos library("nombre de la librería a cargar")
library("tidyverse")
library("magrittr")
library("dplyr")
library("amap")
library("dendextend")
library("factoextra")
library("purrr")
library("openxlsx")
# Para clustering particional
library("cluster")
library("NbClust")


#Cargamos el dataset
datos_docentes<- read.xlsx("datos/DOCENTES-EVAL-S2-2022 (1).xlsx")
write.xlsx(datos_docentes,"DOCENTES-EVAL-S2-2022 (1).xlsx",
          quote = FALSE, row.names = FALSE)

datos_docentes %>%glimpse()#utilizamos glimpse para ver la estructura de la tabla
#los datos en las columnas deben ser escalados o estandarizados
datos_docentes_escalados<- datos_docentes %>%
mutate_if(is.numeric,scale)
view(datos_docentes_escalados)

#euclidea
distancia_euclidea<-datos_docentes_escalados %>%
select_if(is.numeric)%>%

  #Necesitamos determinar la distancia o similitud entre objetos, por lo que procedemos a calcular
  #la matriz de distancia usando la función dist () que es del paquete amap
  Dist(method = "euclidean",diag= FALSE, upper = TRUE)%>%
  as.matrix()

#manhattan

distanci_manhattan <- datos_docentes_escalados %>%
  select_if(is.numeric) %>%
  Dist(method = "manhattan", diag = FALSE, upper = TRUE) %>%
  as.matrix()

#jerarquico
#se utilizará la función hclust()

hc.simple <- hclust(dist(distancia_euclidea),method = "single")
hc.completo <- hclust(dist(distancia_euclidea),method = "complete")
hc.promedio <- hclust(dist(distancia_euclidea),method = "average")

# Visualizacion del dendograma con la funcion plot()
plot(hc.simple,main = "agrupamiento con criterio simple",
     xlab = "Docentes", ylab = "Distancia", cex=1)

#definimos grupos por corte de altura o numero de grupos

#altura
##Utilizamos la función cutree() para
#indicar el punto de corte en la formación de grupos.
grupo <- cutree(hc.simple, h=6.2)

#generamos el agrupamiento en modo matriz
ob<-datos_docentes_escalados$DOCENTE
agrupamiento <- as_tibble(cbind(ob,grupo))
agrupamiento
#creamos un nuevo corte por numero de grupos e indicamos en k en cuantos grupos queremos el corte
grupo_num <- cutree(hc.simple, k=3)

#generamos el agrupamiento en modo matriz
ob <- datos_docentes_escalados$DOCENTE
agrupamiento <- as_tibble(cbind(ob,grupo))
agrupamiento #Como resultado se puede observar a qué grupo pertenece cada docente

#Usamos la funciones color_branches() y color_labels(), para visualizar mejor los dendrogramas.

#colores de las ramas
hc.simple <- color_branches(hc.simple,k=3)
#colores de las etiquetas (objetos)
hc.simple <- color_labels(hc.simple,k=3)
#tamaño de fuente de las etiquetas
hc.simple <- set(hc.simple,"labels_cex",1)

#plot
plot(hc.simple,main="Agrupamiento con criterio SIMPLE",
     xlab = "docentes", ylab = "Distancia", cex=1)


#evaluaremos el rendimiento de los 3 criterios de enlace con diferentes valores de corte
resultados <- map_df(c(3:10), function(num_grupo){
  hc_simple <- hcut(distancia_euclidea, k=num_grupo, hc_method = "single")$silinfo$avg.width
  hc_completo <- hcut(distancia_euclidea, k=num_grupo, hc_method = "complete")$silinfo$avg.width
  hc_promedio <- hcut(distancia_euclidea, k=num_grupo, hc_method = "average")$silinfo$avg.width
    mean()
  #crear tabla
  tibble(num_grupo=num_grupo, hc_simple=hc_simple, hc_completo=hc_completo,
         hc_promedio=hc_promedio)
})

resultados




#Clustering Particional
#Se recomienda limpiar el ambiente de trabajo si queremos observar de mejor manera
#ya que se esta trabajando en un solo sript

#leemos los datos
datos_docentes_particional<- read.xlsx("datos/DOCENTES-EVAL-S2-2022 (1).xlsx")

#los datos en las columnas deben estar estandarizados o escalados
datos_docentes_part_escala <- datos_docentes_particional%>%
  select_if(is.numeric)%>%
  mutate_if(is.numeric,scale)
datos_docentes_part_escala
View(datos_docentes_part_escala)

#asignamos la columna docente como la etiqueta de fila
datos_docentes_part_escala <- datos_docentes_part_escala%>% select_if(is.numeric)
row.names(datos_docentes_part_escala) <-datos_docentes_particional$DOCENTE

#medidas distancia
#euclidea

distancia_euclidea <- datos_docentes_part_escala%>%
  dist(method = "euclidean",diag = FALSE,upper = TRUE)%>% as.matrix()
#manhattan
distancia_manhattan <-datos_docentes_part_escala%>%
  dist(method = "manhattan",diag = FALSE,upper = TRUE)%>% as.matrix()

#visualizamos distancia con la funcion fviz_dist()
fviz_dist(as.dist(distancia_euclidea),show_labels = TRUE,
          gradient = list(low="#00AFBB",mid="white",high="#EC4E08"))

#SEMILLA
set.seed(200)
#ejecutamos el algoritmo para el caso de 3 grupos
km_algoritmo <- Kmeans(datos_docentes_part_escala, centers = 3,
                       iter.max = 100,method = "euclidean")
km_algoritmo

#creamos la matriz particion
#identifica la asignacion en grupo de cada objeto
orden_grupos <-order(km_algoritmo$cluster)
#reordena los objetos de acuerdo con su orden en grupos
objeto=datos_docentes_particional$DOCENTE[orden_grupos]
#identifica el grupo de cada objeto
grupo=km_algoritmo$cluster[orden_grupos]

agrupamiento <- as_tibble(cbind(objeto,grupo))
agrupamiento

#metodo del codo
set.seed(200)
#calcular la silueta para los valores k entre 2 y 10
silueta <- map_dbl(c(2:10),function(i){
  km_algoritmo <- datos_docentes_part_escala%>%
    Kmeans(centers = i,iter.max = 100, method="euclidean")
  ss<- silhouette(km_algoritmo$cluster, dist(distancia_euclidea))
  mean(ss[,3])
})

#plot
plot(2:10, silueta,type = "b",pch=19, frame=FALSE,
     xlab = "numero de grupos", ylim = c(0,1))

#dibuja linea vertical para el promedio
abline(v=5, lty=2)

#metrica de calidad que permite determinar el numero optimo de grupos
#30 indicadores
set.seed(200)
evalua_k <-datos_docentes_part_escala%>%
  NbClust(diss = distancia_euclidea, distance = NULL,
          min.nc = 5,max.nc = 10, method = "kmeans",index = "all")

#VISUALIZACION DE GRUPOS
plot_resultado <- fviz_cluster(data = datos_docentes_part_escala,
                               object = km_algoritmo,
                               ellipse.type = "norm",
                               palette= c("#2e9dfd","#90aa90","#ffa500"),
                               labelsize = 2, main = "agrupamiento k-means", xlab = "", ylab = "")

#agregamos codigo de docente
#este seria el resultado en plot de agrupamiento particional kmeans.
plot_resultado+geom_text(data = plot_resultado$data,aes(x=x,y=y,label=name, colour=cluster),
                         vjust=-1, show.legend = F)
