

hip_cluster = hip[,c(2:3, 6:8)]

hip_cluster2 =  as.data.frame(apply(hip_cluster, 2, scale))

plot(hip_cluster2$RA_J2000, hip_cluster2$DE_J2000)

hip_dgower <- daisy(hip_cluster2, metric="gower")
## Warning in daisy(uoc_personal, metric = "gower"): setting 'logical'
## variable 4 to type 'asymm'

#Gower permite combinar diferentes tipos de variables
#Cluster Jerarquico, con la matriz de distancias
plot(as.dendrogram(hclust(hip_dgower)), leaflab="none")

#Probamos PAM, aunque tengamos todas variables continuas y es muy util kmedia

sse_p_hip <- array()
sil_hip <- array()
kit <- 14  #Arbitrario

for(i in 1:kit){
  # Cálculo de PAM:
  hip_pam <- pam(hip_dgower, i+1, diss = T)
  
  # Determinar el ID del medoide que le corresponde a cada registro:
  hip_meds <- hip_pam$medoids[hip_pam$clustering]
  # Cálculo de SSEs: construir un vector que registre las distancias entre
  # cada objeto y su correspondiente medoide elevadas al cuadrado, y luego
  # calcular su suma. Almacenar cada SSE en un vector.
  sse_p_hip[i] <- sum(as.matrix(hip_dgower)[cbind(row.names(hip_cluster2), hip_meds)]^2)
  
  # Almacenar cada valor de silhouette global
  sil_hip[i] <- hip_pam$silinfo$avg.width
}

par(mfrow=c(2,1))
plot(2:(kit+1), sil_hip, type="b", xlab="k", sub="Silhouette")
plot(2:(kit+1), sse_p_hip, type="b", xlab="k", sub = "SSE")

#Trato de buscar el K viendo el error y Silhouette
#loop para distintos valores de K, corre PAM, calcula medoide, SSE y Silhouette.


#SSE mira el salto en los errores
#Silhoutte, esperamos que suba y listo .. pero aca vemos un comportamiento mas raro
#Ambos muestran como max k=2

par(mfrow=c(1,1))
hip_pam <- pam(hip_dgower, 14, diss = T)
plot(silhouette(hip_pam), main="Silhouette, k = 14")

#No sale el grafico

data.frame(hip_cluster2[hip_pam$medoids,], tamaño=hip_pam$clusinfo[,1])
#Prototipo: MEDOIDE, el valor tipico de cada clase
#raro UOC_Position son todos "Adjunct"
#en el de 4 es un poco mas obvio


hip_pam$isolation
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 
#no no no no no no no no no no no no no no     >> no hay cluster aislados, todos tienen solapamientos
#Levels: no L L*

#cluster puede ser L (aislados) o L* (Cruzados)


#Y un gráfico para ver la relación entre grupos e individuos.
clusplot(hip_pam)
#Vemos con elipses los cluster
#las lineas sirven para dar idea de distancias entre clusters


# Probamos k=4
hip_pam <- pam(hip_dgower, 4, diss = T)
plot(silhouette(hip_pam))
#0.29 da un poco menor que el anterior


data.frame(hip_cluster2[hip_pam$medoids,], tamaño=hip_pam$clusinfo[,1])
#Prototipos, son mas claros
#Masculinos sin doctorado bla

hip_pam$isolation
## 1 2 3 4
## no no no no
## Levels: no L L*
clusplot(hip_pam)
#lineas, relaciones de los cluster SIN señalar los que estan superpuestos
#Varianza baj a19.03% le falta una dimension mas para usar PSA
