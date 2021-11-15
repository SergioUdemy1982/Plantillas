# Arbol de Decision para Regresion

# Libreria para generar modelos de arboles de decision
library(rpart)

# Modelo
regression <- rpart(formula = Salary ~ .,
                    data = dataset,
                    control = rpart.control(minsplit = 1))

# Predicciones

y_preds <- predict(object = regression, newdata = dataset)
y_preds

# Visualizacion del modelo

ggplot() +
  geom_point(aes(dataset$Level, dataset$Salary),
             size = 2,
             color = "#FC4C02") +
  geom_line(aes(dataset$Level, y_preds),
            size = 2,
            alpha = 0.6,
            color = "#008E97")
