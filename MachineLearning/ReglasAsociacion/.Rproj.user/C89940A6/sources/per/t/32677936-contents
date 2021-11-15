# Eclat

# Librerias

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


# Modelo de reglas de asociacion Eclat

rules <- eclat(data = dataset,
                 parameter = list(support= 0.004,
                                  minlen = 3))

# Visualizacion de los resultados
inspect(sort(rules, by="support")[20:40])


