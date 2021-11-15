# Plantilla Clasificacion

# Librerias
library(tidyverse)
library(caTools)
library(magrittr)
library(ElemStatLearn)
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

# Escalado
training[, 1:2] = scale(training[, 1:2])
testing[, 1:2] = scale(testing[, 1:2])

#-------------------------------------------------------------------
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

#--------------------------------------------------------------
# Modelo
library(e1071)

classifier <- naiveBayes(formula = Purchased ~ .,
                         data = training)
# Predicciones

y_pred <- predict(object = classifier, newdata = testing[,-3])

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

#---------------------------------------------------------------------
# Visualizacion de training

set_training = training
X1_training = seq(min(set_training[, 1]) - 1, max(set_training[, 1]) + 1, by = 0.01)
X2_training = seq(min(set_training[, 2]) - 1, max(set_training[, 2]) + 1, by = 0.01)
grid_set_training = expand.grid(X1_training, X2_training)
colnames(grid_set_training) = c('Age', 'EstimatedSalary')
y_grid_training = predict(object = classifier, newdata = grid_set_training)

plot(set_training[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1_training), ylim = range(X2_training))

contour(X1_training,
        X2_training,
        matrix(as.numeric(y_grid_training),
               length(X1_training),
               length(X2_training)),
        add = TRUE)

points(grid_set_training, pch = '.',
       col = ifelse(y_grid_training == 1, "#00BFC4", "#F8766D"))

points(set_training, pch = 21,
       bg = ifelse(set_training[, 3] == 1, 'green4', 'red3'))


ggplot(data = grid_set, aes(x = Age, y = EstimatedSalary, color = y_grid)) +
  geom_tile()

#-----------------------------------------------------------------------
# Visualizacion de testing
set_testing = testing
X1_testing = seq(min(set_testing[, 1]) - 1, max(set_testing[, 1]) + 1, by = 0.01)
X2_testing = seq(min(set_testing[, 2]) - 1, max(set_testing[, 2]) + 1, by = 0.01)
grid_set_testing = expand.grid(X1_testing, X2_testing)
colnames(grid_set_testing) = c('Age', 'EstimatedSalary')
y_grid_testing = predict(object = classifier, newdata = grid_set_testing)

plot(set_testing[, -3],
     main = 'SVM (Test set)',
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
                           shape = Support),
             size = 3) +
  labs(title = "SVM Predictions",
       subtitle = "Kernel: Radial Basis (Gaussian)",
       fill = "Purchase",
       shape = "Support",
       color = "Color") +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) 
