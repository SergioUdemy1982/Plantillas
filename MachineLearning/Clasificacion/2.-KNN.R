# K Nearest Neightbors

# Librerias
library(tidyverse)
library(readr)
library(forcats)

# Carga de dataset
dataset = read_csv(file = "Datasets/Social_Network_Ads.csv")
View(dataset)
glimpse(dataset)

dataset %>%
  select(c(-1,-2)) -> dataset


# Codificacion de la variable respuesta como factor
#dataset$Purchased <- factor(x = dataset$Purchased, levels = c(0,1))

# Separacion de dataset en Training y Testing
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training = subset(dataset, split == TRUE)
testing = subset(dataset, split == FALSE)

# Escalado
training[, 1:2] = scale(training[, 1:2])
testing[, 1:2] = scale(testing[, 1:2])


# Modelo
library(class)

training <- as.data.frame(training)
testing <- as.data.frame(testing)

y_pred <- knn(train = training[,-3],
              test = testing[,-3],
              cl = training[,3],
              k = 5)
y_pred

modelo <- cbind(testing, preds = y_pred)
View(modelo)

# Matriz de confusion
cm <- table(modelo$Purchased, modelo$preds)
cm 

#    0  1
# 0 59  5
# 1  6 30

sum(diag(cm))  # 89 predicciones correctas
sum(diag(apply(cm,2,rev))) # 11 predicciones incorrectas

# Visualizacion de training
library(ElemStatLearn)
set = training
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training[,-3],
             test = grid_set,
             cl = training[,3],
             k = 5)
plot(set[, -3],
     main = 'KNN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
set = testing
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training[,-3],
             test = grid_set,
             cl = training[,3],
             k = 5)
plot(set[, -3],
     main = 'KNN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


