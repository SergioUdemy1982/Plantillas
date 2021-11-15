# Modelo con Random Forest
#
#

library(randomForest)

?randomForest

set.seed(1234)
regression_RF <- randomForest(x = dataset[1], # X debe ser un dataset o matriz
                              y = dataset$Salary,
                              ntree = 500) # Y puede ser un vector

# Predicciones

preds <- predict(object = regression_RF, newdata = dataset)

dataset %>% mutate(Preds = preds)

x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() +
  geom_point(aes(dataset$Level, dataset$Salary),
             size = 2,
             color = "#FC4C02") +
  geom_line(aes(dataset$Level, preds),
            size = 2,
            alpha = 0.6,
            color = "#008E97") +
  geom_line(mapping = aes(x = x_grid,
                          y = predict(regression_RF, newdata = data.frame(Level = x_grid))),
            size = 2,
            alpha = 0.6,
            color = "orange")
  