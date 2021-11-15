# NLP

# librerias
library(tidyverse)
library(NLP)
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)


# Dataset
dataset <- read_tsv(file = "Datasets/Restaurant_Reviews.tsv")


# Limpieza de texto
  # Corpus de datos
corpus <- VCorpus(VectorSource(dataset$Review))

  # cambiar a minuscula
corpus <- tm_map(x = corpus, content_transformer(str_to_lower))
  # consultar la primera linea del corpus
  # corpus[[1]][1]

  # Remover numeros
corpus <- tm_map(x = corpus, removeNumbers)

  # Remover puntos diacriticos
corpus <- tm_map(x = corpus, removePunctuation)

  # Remover palabras no esenciales
corpus <- tm_map(x = corpus, removeWords, stopwords(kind = "en"))

  # Raices de verbos
corpus <- tm_map(corpus, stemDocument)

  # Remover espacios en blanco
corpus <- tm_map(corpus, stripWhitespace)


# Bag of Words

  # Creacion de matriz dispersa de caracteristicas
dtm <- DocumentTermMatrix(corpus)

  # Remover palabras poco frecuentes
dtm <- removeSparseTerms(dtm, 0.999)

  # Estructura transformada a matriz
df <- as.data.frame(as.matrix(dtm))

  # Agregar variable respuesta
df$Liked <- dataset$Liked



# Modelo de clasificacion

  # Codificacion de la variable respuesta como factor
df$Liked <- factor(df$Liked, levels = c(0, 1))

  # Separacion en training y testing
set.seed(123)
split <- sample.split(df$Liked, SplitRatio = 0.8)
training <- subset(df, split == TRUE)
testing <- subset(df, split == FALSE)

  # Ajuste de un modelo de Random Forest

classifier <- randomForest(x = training[-692],
                          y = training$Liked,
                          ntree = 10)

  # Predicciones
y_pred <- predict(classifier, newdata = testing[-692])

  # Matriz de Confusion 
cm <- table(testing[, 692], y_pred)
