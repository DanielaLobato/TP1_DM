#poner a correr mongo en la consola C:\Program Files\MongoDB\Server\3.6\bin>mongod

#importar .json a una coleccion:
# C:\Program Files\MongoDB\Server\3.6\bin>mongoimport --db local --collection "Tweets" --file tweets-dolar.json /v /jsonArray

getwd()

install.packages("mongolite")
install.packages("RMongo")  #No funciona
install.packages("tm")

library(mongolite)
library(RMongo)
library(tm)


#Pegar el Json en Robo3T y lo importamos con el siguiente comando:

sampaoli = mongo(collection = "TP1_DM", db = "local")
sampaoli = sampaoli$find()


#Analisis exploratorio de Datos: 

#listado de variables:
names(sampaoli)

#Caracteristicas:
#is_quote es si tuvo contestaciones
#is_retweet , si proviene de otro tweet *ninguno
#favorite_count, cantidad de favoritos que tiene el tweet
#retwwet_count, cantidad de veces que lo retweetearon
#variable: hashtags
#media, url ???
#mentions_user_id (listado de ID de usuarios llamados, count de mencionados??)
#lang = todos español
#reply_to_user_id; si es respuesta de alguien, y lo nombra (lo vamos a tener en mentions_user_id)

summary(sampaoli)
#is_retweet son todos falsos
#favorite_count en promedio 48.2 con un max 24780
#retweet_count en promedio 19.11 con un max 10628
#se pueden hacer variables binarias con las variables de url/menciones/etc

unique(sampaoli$source) #Plataformas
unique(sampaoli$place_full_name) #Lugares, paises
unique(sampaoli$country)
unique(sampaoli$lang) #Idioma, espanol

  
#Subconjunto de datos:
datos = as.data.frame(sampaoli[,3:5])

# **********************************************************************************************************************#
#PUNTO 5 DEL TP, PROCESAMIENTO DE TEXTO:

#Revisamos tweets
sampaoli$screen_name[5]
sampaoli$text[5]

tweets_corpus_df = sampaoli[c('screen_name', 'text')]

head(tweets_corpus_df)
summary(tweets_corpus_df)
str(tweets_corpus_df)

#Crear corpus
#Sacamos tildes de los Tweets

docs <- chartr("áéíóúñü", "aeiounu", sampaoli$text)
docs

dfCorpus = Corpus(VectorSource(enc2utf8(docs)))

#ejemplo primer tweet
inspect(dfCorpus[1])

#Tareas de preprocesamiento
#Texto a minuscula
corpus.pro = tm_map(dfCorpus, content_transformer(tolower))
#remover numeros
corpus.pro = tm_map(corpus.pro, removeNumbers)
#removemos palabras "vacias" del español
corpus.pro = tm_map(corpus.pro, removeWords, stopwords("spanish"))
#removemos puntuaciones
corpus.pro = tm_map(corpus.pro, removePunctuation)
#Eliminamos espacios
corpus.pro = tm_map(corpus.pro, stripWhitespace)

#Revisamos cambios
inspect(corpus.pro[1])

#Matriz de Documento termino
dtm = TermDocumentMatrix(corpus.pro)

dtm

#Terminos mas frecuentes
m = as.matrix(dtm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(term = names(v), frec = v)
head(d,50)
d

top50 = head(d, 50)

#Visualizar resultados

install.packages("wordcloud")
install.packages("RColorBrewer")

library(RColorBrewer)
library(wordcloud)

par(bg = "grey30")  #Fijamos el fondo en color gris

wordcloud(d$term, d$frec, col=terrain.colors(length(d$term), alpha=0.9), random.order = FALSE, rot.per = 0.3)

#Visualizacion 2
top20 = head(d, 20)
par(bg = "lightblue")
wordcloud(top50$term, top50$frec, col = "white", random.order = FALSE, rot.per = 0.3)

#Escalado Multidimensional

install.packages("proxy")
library(proxy)

N=50

# Distancia del coseno
top50.dtm = dtm[as.character(top50$term)[1:N],]
cosine_dist_mat <- as.matrix(dist(as.matrix(top50.dtm), method = "cosine"))

fit <- cmdscale(cosine_dist_mat, eig=TRUE, k=2) # k is the number of dim

x <- fit$points[,1]
y <- fit$points[,2]


par(bg="white")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Escalado Multidimensional - Top 50 términos", type="n")
text(x, y, labels = rownames(fit$points), cex=.7, col = c("#e41a1c","#377eb8","#4daf4a","#984ea3")[as.factor(floor(log(top50$frec)))]) 

par(lty=2)
abline(h=0); abline(v=0)


# **********************************************************************************************************************#






