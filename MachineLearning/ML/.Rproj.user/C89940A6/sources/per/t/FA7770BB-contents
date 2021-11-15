# Regresion Lineal Simple
#
#
# Carga de librerias
library(tidyverse)
#-----------------------

# Carga de datos
library(readr)
datos <- read_csv("Datasets/Salary_Data.csv")
View(datos)
#-----------------------

# Analisis descriptivo
#
# Y
# X: YearsExperience. Variable cuantitativa continua de escala razon.
# Y: Salary. Variable cuantitiva continua de escala razon.

# Estadisticas basicas de las variables X e Y
summary(datos)
# Tanto en X como en Y, la media es mayor a la mediana, lo cual indica que la 
# distribucion de los datos en ambas variables esta sesgada hacia la derecha.
# 
# Construimos un diagrama de caja para visualizar mejor las estadisticas basicas
plt <- ggplot(data = datos)

plt +
  geom_boxplot(mapping = aes(x = YearsExperience), fill = "#00AFBB", alpha = 0.7) +
  coord_flip() +
  ggtitle(label = "Years Experience Boxplot") # No presenta outliers

plt +
  geom_boxplot(mapping = aes(y = Salary), fill = "#C4961A", alpha = 0.7) +
  ggtitle(label = "Salary Boxplot") # No presenta outliers
#
# Construimos un histograma para visualizar mejor la distribucion de los datos
plt +
  geom_histogram(mapping = aes(x = YearsExperience),
                 bins = 8,
                 fill = "#00AFBB",
                 alpha = 0.7) +
  labs(title = "Years Experience Histogram",
       x = "Years Experience",
       y = "Count")

plt +
  geom_histogram(mapping = aes(x = Salary),
                 bins = 8,
                 fill = "#C4961A",
                 alpha = 0.7) +
  labs(title = "Salary Histogram",
       x = "Salary",
       y = "Count")
#
#
# Construimos un diagrama de puntos para visualizar la relacion entre variables
plt +
  geom_point(mapping = aes(x = YearsExperience, y = Salary,
                           colour = Salary >= mean(Salary)),
             size = 3,
             alpha = 0.7) +
  labs(title = "Years Experience vs Salary",
       color = "Mean") +
  scale_color_manual(labels = c("Menor", "Mayor"),
                     values = c("#C4961A", "#00AFBB")) 
#
# La interpretacion del grafico de puntos es que parece que existe una relacion 
# lineal entre las variables. Esto sera confirmado con un analisis de correlacion
# y el modelo de regresion lineal.
#
#
# Preparacion del dataset para el analisis de regresion
#
#   * Cambiamos los nombres de las variables para trabajar con mayor comodidad
datos <- datos %>% rename(X = YearsExperience, Y = Salary)
#
#
#   * Division del dataset en training y testing
library(caTools)
set.seed(123)
split = sample.split(datos$Y, SplitRatio = 2/3)
split # Los valores TRUE son para training y los FALSE para testing
training_set <- subset(datos, split == T)
testing_set <- subset(datos, split == F)
#
#
# Creacion del modelo de regresion lineal
#
#
regressor <- lm(formula = Y ~ X, data = training_set)
#
#
# Analisis del modelo
summary(regressor)
anova(regressor)
confint(regressor)
#
# Analisis grafico del modelo
plt_training <- ggplot(data = training_set)

plt_training +
  geom_point(aes(X,Y),
             colour = "#00BFC4") +
  geom_abline(slope = regressor$coefficients[[2]],
              intercept = regressor$coefficients[[1]],
              colour = "#F8766D") +
  labs(title = "Training Model")
#
#
# Predicciones
#
# Los nombres de columnas deben ser iguales para la funcion predict()
y_pred <- predict(object = regressor, newdata = testing_set)
#
#
# Graficos de la prediccion

ggplot(data = testing_set) +
  geom_point(aes(X,Y),
             colour = "#F8766D") +
  geom_abline(intercept = regressor$coefficients[[1]],
              slope = regressor$coefficients[[2]],
              colour = "#00BFC4") +
  labs(title = "Testing Model")

