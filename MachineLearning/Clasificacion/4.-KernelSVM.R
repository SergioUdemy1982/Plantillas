# Kernel SVM

# Librerias
library(tidyverse)
library(readr)
library(forcats)
library(magrittr)

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


# Visualizacion
df <- dataset
df[, 1:2] <- scale(df[, 1:2])

df %>%
  ggplot(aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased))) +
  geom_point() +
  labs(color = "Purchased",
       title = "Dataset") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("No", "Yes"))

training %>%
  ggplot(aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased))) +
  geom_point() +
  labs(color = "Purchased",
       title = "Training") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("No", "Yes"))

testing %>%
  ggplot(aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased))) +
  geom_point() +
  labs(color = "Purchased",
       title = "Testing") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("No", "Yes"))


# Modelo
library(e1071)

classifier <- svm(formula = Purchased ~ .,
                  data = training,
                  type = "C-classification",
                  kernel = "radial")

# Predicciones

y_pred <- predict(object = classifier, newdata = testing[,-3])

testing %>%
  mutate(Preds = y_pred) -> modelo 

modelo %>%
  mutate(Support = ifelse(Purchased == Preds, "Yes", "No")) -> modelo

View(modelo)

# Matriz de confusion

modelo %$%
  table(Real = Purchased, Predicted = Preds) -> cm

cm
sum(diag(cm)) # 90 predicciones correctas
sum(diag(apply(cm,2,rev))) # 10 predicciones incorrectas


# Visualizacion de training
library(ElemStatLearn)
set = training
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(object = classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "#00BFC4", "#F8766D"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


ggplot(data = grid_set, aes(x = Age, y = EstimatedSalary, color = y_grid)) +
  geom_tile()

#-----------------------------------------------------------------------
# Visualising the Test set results
set = testing
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(object = classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "#00BFC4", "#F8766D"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


cols <- c('Red' = 'red', 'Green' = 'green')
shapes <- c('No' = 4, 'Yes' = 1)

ggplot() +
  geom_tile(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = y_grid)) +
  geom_point(data = modelo, mapping = aes(x = Age, y = EstimatedSalary, shape = Support),
             size = 3) +
  labs(title = "SVM Predictions",
       subtitle = "Kernel: Radial Basis (Gaussian)",
       fill = "Purchase",
       shape = "Support",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) 


