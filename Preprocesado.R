# Preprocesado

# Librerias
library(readr)
library(caTools)

# Carga de dataset
dataset = read_csv('Data.csv')

# Segmentacion de training y testing
set.seed(123)
split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
training = subset(dataset, split == TRUE)
testing = subset(dataset, split == FALSE)

# Escalado
training = scale(training)
testing = scale(testing)