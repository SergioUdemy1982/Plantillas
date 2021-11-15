# Arboles de Decision para clasificacion

# Librerias
library(tidyverse)
library(caTools)
library(magrittr)
library(ElemStatLearn)
library(rpart)
#------------------------------------------------------------
# Carga de dataset
dataset = read_csv(file = "Datasets/Social_Network_Ads.csv")
View(dataset)
glimpse(dataset)

dataset %>%
  select(c(-1,-2)) -> dataset


# Codificacion de la variable respuesta como factor
#dataset$Purchased <- factor(x = dataset$Purchased, levels = c(0,1))
#--------------------------------------------------------------------
# Separacion de dataset en Training y Testing
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training = subset(dataset, split == TRUE)
testing = subset(dataset, split == FALSE)

# Modelo
classifier <- rpart(formula = Purchased ~ .,
                    data = training,
                    method = "class")

# Predicciones

y_pred <- predict(object = classifier,
                  newdata = testing[,-3],
                  type = "class")
testing %>%
  mutate(Preds = y_pred) -> modelo 

modelo %>%
  mutate(Correct = ifelse(Purchased == Preds, "Yes", "No")) -> modelo

View(modelo)

# Matriz de confusion

modelo %$%
  table(Real = Purchased, Predicted = Preds) -> cm

cm
sum(diag(cm)) # 86 predicciones correctas
sum(diag(apply(cm,2,rev))) # 14 predicciones incorrectas




# Visualizacion de testing
set_testing = testing
X1_testing = seq(min(set_testing[, 1]) - 1, max(set_testing[, 1]) + 1, by = 1)
X2_testing = seq(min(set_testing[, 2]) - 1, max(set_testing[, 2]) + 1, by = 500)
grid_set_testing = expand.grid(X1_testing, X2_testing)
colnames(grid_set_testing) = c('Age', 'EstimatedSalary')
y_grid_testing = predict(object = classifier, newdata = grid_set_testing, type = "class")

plot(set_testing[, -3],
     main = 'Decision Tree Predictions',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1_testing),
     ylim = range(X2_testing))

contour(X1_testing,
        X2_testing,
        matrix(as.numeric(y_grid_testing),
               length(X1_testing),
               length(X2_testing)),
        add = TRUE)

points(grid_set_testing,
       pch = '.',
       col = ifelse(y_grid_testing == 1, "#00BFC4", "#F8766D"))

points(set_testing,
       pch = 21,
       bg = ifelse(set_testing[, 3] == 1, 'green4', 'red3'))


# Grafico con ggplot2

cols <- c('Red' = 'red', 'Green' = 'green')
shapes <- c('No' = 4, 'Yes' = 1)

ggplot() +
  geom_tile(data = grid_set_testing,
            mapping = aes(x = Age,
                          y = EstimatedSalary,
                          fill = y_grid_testing)) +
  geom_point(data = modelo,
             mapping = aes(x = Age,
                           y = EstimatedSalary,
                           shape = Correct),
             size = 3) +
  labs(title = "Decision Tree Predictions",
       x = "Age",
       y = "Estimated Salary",
       subtitle = "Correct: 86 | Incorrect: 14",
       fill = "Purchase",
       shape = "Support",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) 

#-------------------------------------------------------------------------------
# Representacion del arbol de clasificacion

plot(classifier)
text(classifier)
