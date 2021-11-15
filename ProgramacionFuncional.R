# Programacion funcional con Purrr

# Librerias
library(magrittr)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)

# Dataset
df <- iris

# Familia Map

map(.x = df, .f = summary) # Mapea sobre cada vector (columna) y devuelve una lista.

df %$%
  map2_dbl(.x = Sepal.Length,
       .y = Sepal.Width,
       .f = ~ .x * .y ^2)  # Mapea sobre 2 vectores y devuelve un vector



list(media=mean, sd = sd, var=var) %>%
  invoke_map(x = df$Sepal.Length)   # Mapea una lista de funciones sobre un vector


invoke_map(list(normal=rnorm, uniforme=runif),
           list(list(n=30, mean=30, sd=5), list(n =15))) # Los parametros son listas

invoke_map(list(sample),
           list(list(x=1:100, size=20), list(x=200:300, size=10)))


# Creacion de una lista con muestras
muestras <- vector(mode = "list", length = 10) %>%
  map(.x = ., .f = ~ df[sample(nrow(df),10,T),])

# Dataframe de muestras
muestras <- vector(mode = "list", length = 10) %>%
  map(.x = ., .f = ~ df[sample(nrow(df),10,T),]) %>%
  tibble(samples = .)


muestras %<>% # Ejecuta y guarda
  mutate(resumen = map(.x = samples, summary))

muestras %<>%
  mutate(modelo = map(.x = samples,
                      .f = ~ lm(formula = Sepal.Length ~ ., data = .x)))




