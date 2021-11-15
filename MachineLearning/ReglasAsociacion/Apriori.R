# Apriori


# Librerias
library(tidyverse)
library(arules)
library(arulesViz)

# Datos
df <- read.csv(file = "Datasets/Market_Basket_Optimisation.csv", header=F)

# Creamos una matriz sparse, para eliminar los espacios vacios
dataset <- read.transactions(file = "Datasets/Market_Basket_Optimisation.csv",
                             sep = ",",
                             rm.duplicates = T)

summary(dataset)

itemFrequencyPlot(dataset, topN=20)


# Modelo de reglas de asociacion

rules <- apriori(data = dataset,
                 parameter = list(support= 4*7/7500, # 3 productos diarios
                                  confidence=0.2))   # Nivel de confianza defaul

# Visualizacion de los resultados
inspect(sort(rules, by="lift")[1:20])

# Grafo
# visualizations ---------------------------------------------------------
plot(rules[1:20], method = "graph", engine = "htmlwidget")
                 