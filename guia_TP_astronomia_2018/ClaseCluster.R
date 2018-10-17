#Practicas de limpieza
#Metodos de cluster
#Definicion de distancias
#Proyeccion de datos



library(cluster)
library(MASS)
install.packages("fpc")
library(fpc)
# install.packages("dplyr")
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("stringr")
library(stringr)



getwd()
setwd("C:/Users/loda7002/Downloads")
encuesta = as.data.frame(read.table("wiki4HE.csv",header = TRUE, sep = ";" , stringsAsFactors = FALSE))

dim(encuesta)
str(encuesta[1:10])

contar_na <- function(x) length(str_which(x, "\\?"))   #Funciones abreviadas
recuento_na <- sapply(encuesta, contar_na)

#Para cada una de las variables cuantos datos faltantes hay
sort(recuento_na, decreasing = T)

convertir_na <- function(x, na_symbol = "?"){
  if(typeof(x) == "character"){
    x[ x == na_symbol ] <- NA
  }
  return(as.numeric(x))
}

#Nuevo DataSet con los NA correspondientes:
encuesta_2 <- as.data.frame( sapply(encuesta, convertir_na) )

#Arreglamos variables como Factores que nos van a ayudar al momento de hacer distancias euclideas:

encuesta_2$GENDER <- factor( ifelse(encuesta_2$GENDER == 1, "F", "M") )
domain_labels <- c("Arts_Humanities", "Sciences", "Health_Sciences","Engineering_Architecture", "Law", "Political_Sciences")
encuesta_2$DOMAIN <- factor(encuesta_2$DOMAIN, labels = domain_labels )
pos_labels = c("Professor", "Associate", "Assistant", "Lecturer", "Instructor", "Adjunct")
encuesta_2$UOC_POSITION <- factor(encuesta_2$UOC_POSITION, labels = pos_labels)
encuesta_2$OTHERSTATUS <- factor(encuesta_2$OTHERSTATUS) # Queda codificado como númenro porque los nombres de categorías no coinciden con el número de categorías
encuesta_2$UNIVERSITY <- factor( ifelse(encuesta_2$UNIVERSITY == 1, "UOC", "UPF"))

encuesta_2$PhD <- as.logical(encuesta_2$PhD)
encuesta_2$USERWIKI <- as.logical(as.numeric(encuesta_2$USERWIKI))
encuesta_2$OTHER_POSITION <- if_else(encuesta_2$OTHER_POSITION == 1, TRUE, FALSE, NA)  #if_else permite dar valor a los faltantes con el ultimo parametro

encuesta_uoc <- encuesta_2 %>% 
  filter(UNIVERSITY == "UOC") %>% select(-UNIVERSITY)

table(complete.cases(encuesta_uoc)) #Casos sin NA por filas

table(sapply(encuesta_uoc, anyNA)) #Casos NA por variable

encuesta_uoc$OTHER_POSITION <- NULL
encuesta_uoc$Vis2 <- NULL
encuesta_uoc$PEU3 <- NULL
table(complete.cases(encuesta_uoc))

#Me quedo con los completos y separo por caracteristicas de las personas (1 a 7) y las preguntas (resto)
encuesta_uoc_c <- encuesta_uoc[complete.cases(encuesta_uoc), ]
uoc_personal <- encuesta_uoc_c[,1:7]
uoc_preguntas <- encuesta_uoc_c[, 8:49]

#1. Cluster sobre personas
#2. Cluster sobre respuestas
#3. Ver si ambos coinciden


uoc_personal_dgower <- daisy(uoc_personal, metric="gower")
## Warning in daisy(uoc_personal, metric = "gower"): setting 'logical'
## variable 4 to type 'asymm'

#la variable 4 es logica (T/F) la seteo como "asymm" ??

#Gower permite combinar diferentes tipos de variables
#Cluster Jerarquico, con la matriz de distancias
plot(as.dendrogram(hclust(uoc_personal_dgower)), leaflab="none")

#no es muy evidente donde cortar

#Usamos PAM, por qué no usamos K-medias?
#K-medias esta pensado para distancia euclidea, y aca no hay variables continuas (solo edad) 
#hay logicas y factores, en principio no puedo usar kmedias
#Si fueran todas continuas podría empezar por Kmedias

sse_p_pers <- array()
sil_pers <- array()
kit <- 14  #Arbitrario

for(i in 1:kit){
  # Cálculo de PAM:
  personal_pam <- pam(uoc_personal_dgower, i+1, diss = T)
  # Determinar el ID del medoide que le corresponde a cada registro:
  pers_meds <- personal_pam$medoids[personal_pam$clustering]
  # Cálculo de SSEs: construir un vector que registre las distancias entre
  # cada objeto y su correspondiente medoide elevadas al cuadrado, y luego
  # calcular su suma. Almacenar cada SSE en un vector.
  sse_p_pers[i] <- sum(as.matrix(uoc_personal_dgower)[cbind(row.names(uoc_personal), pers_meds)]^2)
  # Almacenar cada valor de silhouette global
  sil_pers[i] <- personal_pam$silinfo$avg.width
}
par(mfrow=c(2,1))
plot(2:(kit+1), sil_pers, type="b", xlab="k", sub="Silhouette")
plot(2:(kit+1), sse_p_pers, type="b", xlab="k", sub = "SSE")

#Trato de buscar el K viendo el error y Silhouette
#loop para distintos valores de K, corre PAM, calcula medoide, SSE y Silhouette.


#SSE mira el salto en los errores
#Silhoutte, esperamos que suba y listo .. pero aca vemos un comportamiento mas raro
#llega al max en 4 y en 14
#Se relaciona con el dendograma, donde habia 4 grupos grandes y subgrupos debajo (14)

par(mfrow=c(1,1))
personal_pam <- pam(uoc_personal_dgower, 14, diss = T)
plot(silhouette(personal_pam), main="Silhouette, k = 14")

#Cantidad de Silhoutte en cada grupo
#N=188 casos que quedaron "OUC"
#Silhoutte promedio


data.frame(uoc_personal[personal_pam$medoids,], tamaño=personal_pam$clusinfo[,1])
#Prototipo: MEDOIDE, el valor tipico de cada clase
#raro UOC_Position son todos "Adjunct"
#en el de 4 es un poco mas obvio


personal_pam$isolation
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 
#no no no no no no no no no no no no no no     >> no hay cluster aislados, todos tienen solapamientos
#Levels: no L L*

#cluster puede ser L (aislados) o L* (Cruzados)


#Y un gráfico para ver la relación entre grupos e individuos.
clusplot(personal_pam)
#Vemos con elipses los cluster
#las lineas sirven para dar idea de distancias entre clusters


# Probamos k=4
personal_pam <- pam(uoc_personal_dgower, 4, diss = T)
plot(silhouette(personal_pam))
#0.29 da un poco menor que el anterior


data.frame(uoc_personal[personal_pam$medoids,], tamaño=personal_pam$clusinfo[,1])
#Prototipos, son mas claros
#Masculinos sin doctorado bla

personal_pam$isolation
## 1 2 3 4
## no no no no
## Levels: no L L*
clusplot(personal_pam)
#lineas, relaciones de los cluster SIN señalar los que estan superpuestos
#Varianza baj a19.03% le falta una dimension mas para usar PSA


#Cluster por Densidad
#Buscar el EPS y la distancia tipica era usando 5 vecinos
#EPS : Distancia a la que estaban los 5 vecinos - Radio que usamos como parametro
buscar_eps <- apply(as.matrix(uoc_personal_dgower), 1, function(x) sort(x)[5])
plot(sort(buscar_eps), type="l")

personal_dbs_1 <- dbscan(uoc_personal_dgower, eps=0.09)
personal_dbs_1
## dbscan Pts=188 MinPts=5 eps=0.09
##
## 0
## 188                      >> encontro un solo cluster
personal_dbs_2 <- dbscan(uoc_personal_dgower, eps=0.15)
personal_dbs_2
## dbscan Pts=188 MinPts=5 eps=0.15
## 0 1
## border 183 4
## seed 0 1
## total 183 5              >> enconro dos clusters pero metió casi todos en uno (0)

#Es muy malo


#Clustering Difuso
#Linea para hacer el cluster, calcula el coef de Dunnet
personal_fuzz_1 <- fanny(uoc_personal_dgower, 4, diss = T, memb.exp = 1.35)
#Coeficiente de Dunnet
personal_fuzz_1$coeff
## dunn_coeff normalized
## 0.5843586 0.4458115

#Asigna la Probabilidad de pertenecer a una clase. Lo vemos en la siguiente tabla:
# Membresías (matriz y grupo con mayor puntaje) (solo 6 casos)
head(personal_fuzz_1$membership)

#parece ver que se muestra claro una clase con mayor probabilidad que otras (> 0.6)

head(personal_fuzz_1$clustering, 10)
# Distribución de las máximas membresías de cada registro:
hist(apply(personal_fuzz_1$membership,1, max), main="")



# ¿Cuántos registros tienen una membresía menor que 0.6?
fuzz_pers <- apply(personal_fuzz_1$membership,1, max) < 0.6
table(fuzz_pers)


# A los registros con unamembresía menor a 0.6
# los asignamos a un cluster "0", que corresponde a los
# que no agrupan claramente
fuzz_pers_col <- personal_fuzz_1$clustering
fuzz_pers_col[fuzz_pers] <- 0


uoc_personal_nmds <- isoMDS(uoc_personal_dgower + 0.0001)
#Nos dice si Converge o no
uoc_personal_nmds$stress  #Arriba de 20 es razonable, pero no ideal

#Otra forma de proyectar N en un plano ( y uno usar PSA , que tampoco era muy bueno en este caso)
#minimizacion de "stress"
plot(uoc_personal_nmds$points, col=personal_fuzz_1$clustering+1, pch=20)
#plotea todos los profesores y pintados en color a que grupo pertenece


#Cuidado: Si el ID de un cluster es cero, R no le va asignar color
plot(uoc_personal_nmds$points, col=fuzz_pers_col+1, pch=20)




#Análisis de las respuestas a las encuestas

#Primer ejemplo como "euclidean" pero no está del todo bien

preguntas_pam <- pam(uoc_preguntas, 5, metric = "euclidean")

# Quienes son los medoides
preguntas_pam$id.med

# como se agrupan los encuestados
head( preguntas_pam$clustering, 15)

# cual es el clustering que le corresponde a cada encuestado
head(preguntas_pam$medoids)

head( preguntas_pam$medoids[preguntas_pam$clustering,], 15)

vec_meds <- preguntas_pam$medoids[preguntas_pam$clustering,]

#Buscamos calcular el K
sse_vec <- array()
sil_global <- array()
for(i in 1:kit){
  preguntas_pam <- pam(uoc_preguntas, i+1, metric = "euclidean", keep.diss = T)
  vec_meds <- row.names(preguntas_pam$medoids)[preguntas_pam$clustering]
  sse_vec[i] <- sum(as.matrix(preguntas_pam$diss)[cbind(row.names(uoc_personal),vec_meds)]^2)
  sil_global[i] <- preguntas_pam$silinfo$avg.width
}
par(mfrow=c(2,1))
plot(2:(kit+1), sse_vec, xlab="k", type="b", main="SSE")
plot(2:(kit+1), sil_global, xlab="k", type="b", main="Silhouette")
par(mfrow=c(1,1))



#Vamos por un ejemplo mejor, no usamos Euclidea
#Calculamos nosotros la distancia, normalizamos la metrica (de 1 a 5 > 0 a 1)
sum(abs(uoc_preguntas[1,] - uoc_preguntas[2,])) / (ncol(uoc_preguntas)*4)

dist_enc <- matrix(NA, nrow(uoc_preguntas), nrow(uoc_preguntas))
min_dis <- ncol(uoc_preguntas)*4
mat_dat <- as.matrix(uoc_preguntas)
# El loop que sigue se podría acelerar teniendo en cuenta que el resultado
# es una matriz singular, pero para el tamaño que tiene, no haría falta
for(i in 1:nrow(mat_dat)){
  for(j in 1:nrow(mat_dat)){
    dist_enc[i, j] <- sum(abs(mat_dat[i,] - mat_dat[j,])) / min_dis
  }
}
row.names(dist_enc) <- row.names(mat_dat)
dist_enc <- as.dist(dist_enc)   #Distancia calculada y se usa en el PAM

#Definimos nosotros la distancia
#Y repetimos la misma actividad de antes, buscamos el k óptimo
#Para no usar la euclidea ya que no son variables continuas.
#Se considera mejor por no usar el cuadrado de la euclidea

sse_p_preg_d <- array()
sil_preg_d <- array()
for(i in 1:kit){
  preguntas_d_pam <- pam(dist_enc, i+1, diss = T)
  vec_meds_d <- preguntas_d_pam$medoids[preguntas_d_pam$clustering]
  sse_p_preg_d[i] <- sum(as.matrix(dist_enc)[cbind(row.names(uoc_preguntas), vec_meds_d)]^2)
  sil_preg_d[i] <- preguntas_d_pam$silinfo$avg.width
}
par(mfrow=c(2,1))
plot(2:(kit+1), sse_p_preg_d, xlab="k", type="b", main="SSE")
plot(2:(kit+1), sil_preg_d, xlab="k", type="b", main="Silhouette")
par(mfrow=c(1,1))

#En este caso, el análisis con Silhouette indica más fuertemente usar un k=2, y con SSE, como pasó antes, no
#se ve un claro ganador. Probemos con k=3.
preguntas_d_pam <- pam(dist_enc, 3, diss = T)  #Calculo PAM con la nueva distancia y K=3
dist_enc_nmds <- isoMDS(dist_enc + 0.0001)  #Proyeccion MDS
#Converged
dist_enc_nmds$stress   #Da peor
## [1] 19.35199
plot(dist_enc_nmds$points)

#¿Cómo se agrupan los encuestados según sus respuestas?
plot(dist_enc_nmds$points, col=preguntas_d_pam$clustering, pch=19)
#Verde y negro no se separan mucho, podria haber funcionado con K=2

#Mezclamos las caracteristicas que teniamos de antes
#Y según sus características demográficas y profesionales ¿Cómo se distribuyen sobre la nube de respuestas?
plot(dist_enc_nmds$points, col=personal_pam$clustering, pch=19)

#No termina teniendo mucha relacion a que grupo de personas pertenecia y el grupo que te otorga las respuestas que dió 
#por lo menos con estas cantidad de clusters


#Como una visualización complementaria podemos ver cómo se distribuyen según sus respuestas en el clustering
#por características deomgráficas y personales.
plot(uoc_personal_nmds$points, col=preguntas_d_pam$clustering, pch=20)
#Este es el caso contrario: Miramos en el espacio de las personas y pintados por respuestas, tampoco se ve nada relevante


#Finalmente, podemos hacer una matriz de confusión entre ambos agrupamientos para determinar si hay
#alguna asociación entre grupos demograficos y perfil de respuestas a la encuesta.

table(personal_pam$clustering, preguntas_d_pam$clustering, dnn=c("grupo demográfico", "grupo de respuestas" ))

#No se ve mucha relacion en la matriz de confusion tampoco



