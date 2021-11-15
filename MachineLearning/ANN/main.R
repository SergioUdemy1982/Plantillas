# Redes Neuronales Artificiales

# Librerias
library(tidyverse)
library(magrittr)
library(caTools)
library(h2o)

# Carga de datos
dataset = read_csv(file = "Datasets/Churn_Modelling.csv")
glimpse(dataset)

# Revision de valores nulos
apply(X = dataset, MARGIN = 2, FUN = is.na) %>%
  apply(X = ., MARGIN = 2, FUN = sum)

# Seleccionamos las variables a utilizar
dataset %<>%
  select(-c(1:3))


# Codificamos variables categoricas
dataset$Geography <- as.numeric(factor(dataset$Geography,
                                       levels = c("France", "Spain", "Germany"),
                                       labels = c(1, 2, 3)))

dataset$Gender <- as.numeric(factor(dataset$Gender,
                                    levels = c("Female", "Male"),
                                    labels = c(0,1)))

# Separar dataset en training y testing
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalado
training_set[, -11] = scale(training_set[, -11])
test_set[, -11] = scale(test_set[, -11])

# ANN Modelo

# Abre servidor
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = "Exited",
                              training_frame = as.h2o(training_set),
                              activation = "Rectifier",
                              hidden = c(6,6),
                              epochs = 100,
                              train_samples_per_iteration = -2)

# Predicciones
prob_pred = h2o.predict(classifier, newdata = as.h2o(test_set[, -11]))

y_pred <- prob_pred > 0.5
y_pred <- as.vector(y_pred)

# Matriz de confusion
cm <- table(test_set[,11][[1]], y_pred)
cm

accuracy <- sum(diag(cm)) / nrow(test_set[,11])
accuracy

# Cerrar servidor
h2o.shutdown()
