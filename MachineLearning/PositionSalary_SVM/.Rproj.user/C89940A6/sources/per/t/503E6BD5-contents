# Modelo
#
#
# Support Vector Machine
library(e1071)

regression <- svm(formula = Salary ~ Level,
                  data = dataset,
                  type = "eps-regression",
                  kernel = "radial")

y_pred <- predict(regression, dataset)

dataset %>% mutate(Preds = y_pred)

# Visualizacion

ggplot() +
  geom_point(data = dataset,
             mapping = aes(x = Level, y = Salary),
             size = 3,
             color = "steelblue",
             alpha = 0.7) +
  geom_line(aes(dataset$Level, y_pred),
            size = 1,
            color = "orange")
