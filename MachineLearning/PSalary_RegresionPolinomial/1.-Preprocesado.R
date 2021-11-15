# Plantilla de preprocesado de datos
#
#
# Importar librerias
library(tidyverse)
library(caTools)
library(fastDummies)
library(readr)
#
#
# Importar datos
dataset <- read_csv(file = "Datasets/Position_Salaries.csv")
View(dataset)
# Removemos la columna "Position"
dataset <- dataset %>% select(-Position)
#
#
# Division de dataset en training y testing
#set.seed(123)
#division <- sample.split(Y = , SplitRatio = 0.8)
#training <- subset(dataset, division == T)
#testing <- subset(dataset, division == F)