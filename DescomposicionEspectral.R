# Descomposicion Espectral

# Librerias
library(tidyverse)

# Datos

x1 <- c(21,22,16,17,12,25,18,15,14,18,14,15,25,15)
x2 <- c(26,16,28,30,26,10,21,17,23,20,29,23,21,20)

tabla <- tibble(X1 = x1,
                X2 = x2)
# write_csv(x = tabla, file = "Data Science/Datasets/descespectr.csv")

# Normalizacion de los datos

tabla <- as_tibble(round(scale(tabla), 4))

View(tabla)

# Construimos la matriz de correlacion

tabla_corr <- cor(tabla)

# Eigenvalores y Eigenvectores

ev <- eigen(tabla_corr)

values <- ev$values
vectors <- ev$vectors

# Matriz no rotada

tribble(~filas,~F1,~F2,~Comunalidad,
        "X1",vectors[1,1],vectors[1,2],1,
        "X2",vectors[2,1],vectors[2,2],1,
        "eigenvalue",values[1],values[2],2)

# La suma de los cuadrados de las filas es 1, pero la suma de los
# cuadrados de las columnas es diferente a los eigenvalores. Se buscan
# eigenvectores que cumplan ambos criterios

tribble(~filas,~F1,~F2,~Comunalidad,
        "X1",-0.878,-0.878,1,
        "X2",0.878,-0.878,1,
        "eigenvalue",values[1],values[2],2) -> mnr
# ----------------

pca.sample <- prcomp(tabla, center = T, scale. = T)
summary(pca.sample)
str(pca.sample)

ggbiplot(pca.sample)
