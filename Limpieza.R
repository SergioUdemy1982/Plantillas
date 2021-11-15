# Plantilla para limpieza de dataset

# Librerias
library(tidyverse)
library(magrittr)
library(stringr)

# Carga de dataset
dataset <- read_csv(file = "Data Science/Datasets/titanic_full.csv")

# Caracteristicas del dataset
View(dataset)     # Imprime el dataset
glimpse(dataset)  # Devuelve la estructura del dataset de manera breve
str(dataset)      # Devuelve la estructura del dataset con mayor detalle

# Seleccionar columnas por tipo de dato (dplyr y purrr)
dataset %>%
  select_if(is_double)  # Double

dataset %>%
  select_if(is_character) # Character

# Columnas con valores nulos NA
# Usando Tidyverse
dataset %>%
  summarise_all(~ sum(is.na(.))) %>%
  t()

# Usando Base
apply(X = dataset, MARGIN = 2, FUN = is.na) %>%
  apply(X = ., MARGIN = 2, FUN = sum)

# Sustitucion de NA por columna
# Ejemplo usando Tidyverse
dataset$Age %>%
  replace_na(mean(dataset$Age, na.rm = T))

# Sustitucion de NA por columna agrupada por categorias
# Ejemplo usando Tidyverse
dataset %<>%
  group_by(Pclass, Sex) %>%
  mutate(Age = round(ifelse(is.na(Age),
                      mean(Age, na.rm = T),
                      Age))) %>%
  ungroup()

# Sustitucion de NA para variables segun el tipo de dato con un valor determinado
# Ejemplo para columnas de tipo character usando Tidyverse
dataset %<>%
  mutate(across(where(is.character),
                ~replace_na(data = .x, replace = "NO-DATA")))



# Conversion a numero de la variable Sex 
dataset %<>%
  mutate(Sex = ifelse(Sex == "male", 0, 1))

# Separacion de columna name en Las.Name y First.Name
dataset %<>%
  separate(Name, into = c("Last.Name", "First.Name"), sep = ",")

# Cambio de . por ,
dataset$First.Name <- str_replace(dataset$First.Name,". ", ", ")

# Separacion de columna First.Name en Title y Name
dataset %<>%
  separate(First.Name, into = c("Title", "Name"), sep=",")

# Eliminacion de espacios innecesarios al iniciar la palabra
dataset$Title <- str_sub(string = dataset$Title, start = 2) 
dataset$Name <- str_sub(string = dataset$Name, start = 2)


