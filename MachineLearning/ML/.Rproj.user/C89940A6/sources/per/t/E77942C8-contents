# Preprocesado de datos
#
# Carga de librerias
library(tidyverse)

#----------------------

# Carga de dataset
dataset <- readr::read_csv("Datasets/data1.csv")
dataset <- dplyr::as_tibble(dataset)
View(dataset)
glimpse(dataset)
#---------------------

# Manejo de valores nulos NA
dataset %>% replace_na(list(Age = round(mean(dataset$Age, na.rm = T)),
                            Salary = round(mean(dataset$Salary, na.rm = T)))) -> dataset

#--------------------

# Codificacion de categorias

dataset$Purchased <- factor(dataset$Purchased,
                            levels = c("No", "Yes"),
                            labels = c(0, 1))

dataset$Country <- factor(dataset$Country,
                          levels = c("France", "Spain", "Germany"),
                          labels = c(1, 2, 3))

dataset_arranged <- dummy_columns(dataset, "Country")

#write.csv(x = dataset, file = "Datasets/data1.csv", row.names = F)

dataset_arranged %>% select(-Country) %>%
  rename(France = Country_France,
         Germany = Country_Germany,
         Spain = Country_Spain) -> dataset_arranged 

dataset_arranged %>% 
  select(France, Germany, Spain, Age, Salary, Purchased) -> dataset_arranged

#write.csv(x = dataset_arranged, file = "Datasets/data1_arranged.csv", row.names = F)
#---------------------------------

# Division de dataset en training y test
set.seed(123)
split = sample.split(dataset_arranged$Purchased, SplitRatio = 0.8)
split # Los valores TRUE son para training y los FALSE para testing
training_set <- subset(dataset_arranged, split == T)
testing_set <- subset(dataset_arranged, split == F)
#-------------------------------

# Escalamiento de datos
#
# Estandarizacion: X - mean(X) / sd(X)
# Normalizacion: X - min(X) / max(x) - min(X)

estandarizacion <- function(x){
  (x - mean(x, na.rm=T)) / (sd(x, na.rm=T))
}

normalizacion <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

training_set[, 4:5] <- scale(training_set[, 4:5])
testing_set[, 4:5] <- scale(testing_set[, 4:5])

