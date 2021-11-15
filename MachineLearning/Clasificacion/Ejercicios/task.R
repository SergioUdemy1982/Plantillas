# Task Success Logistic Regression

# Libraries
library(tidyverse)

# Dataset
dataset <- read_csv(file = "Datasets/experience_task.csv")
glimpse(dataset)

# Description
#
# experience: variable predictora de tipo discreta y escala razon. La unidad
#             de medicion es en meses.
#
# success: variable respuesta de tipo discreta, indicadora binaria. El valor
#          1 representa que la tarea se completo exitosamente en el tiempo
#          establecido, mientras que el valor 0 representa que la tarea no fue
#          completada en el tiempo establecido.


# Estadisticas descriptivas
#
# Distribucion de la variable experience
summary(dataset$experience)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00    9.00   18.00   16.88   24.00   32.00 
#
# Dado que la Mediana es mayor a la Media, deducimos que el sesgo de la distri-
# bucion de los datos de experience es hacia la izquierda.
#
boxplot(x = dataset$experience, title="Boxplot Experience (Months)")
hist(x = dataset$experience)

# Media de experiencia respecto al exito
dataset %>%
  group_by(success) %>%
  summarise(meanExperience = mean(experience))

# success meanExperience
#  0           12.4
#  1           22.5
#
# La media de edad de los que cumplieron exitosamente la tarea es de 22.5, mucho
# mayor a la media de edad de quienes no la cumplieron.

# modelo

# Separacion de dataset en Training y Testing
library(caTools)
set.seed(123)
split = sample.split(dataset$success, SplitRatio = 0.75)
training = subset(dataset, split == TRUE)
testing = subset(dataset, split == FALSE)

# Creacion del modelo
modelo <- glm(formula = success ~ .,
              data = dataset,
              family = binomial(link = "logit"))

summary(modelo)
anova(modelo)

# Predicciones

prob_pred <- predict(object = modelo,
                     type = "response",
                     newdata = dataset[,1])

y_pred <- ifelse(prob_pred > 0.5, 1, 0)


dataset %>%
  mutate(Preds = y_pred,
         Probs = prob_pred) -> tm

tm %>%
  mutate(Correct = ifelse(success == Preds, "Yes", "No")) -> tm


# Matriz de confusion para revisar predicciones correctas
cm = table(Real = tm$success, Predict = tm$Preds)
cm

#     Predict
# Real 0 1
#    0 4 0
#    1 2 1

sum(diag(cm))  # 5 predicciones correctas
sum(diag(apply(cm,2,rev))) # 2 predicciones incorrectas


ggplot(data = tm, mapping = aes(x = experience, y = success)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", se=F, method.args=list(binomial(link = "logit"))) +
  geom_smooth(method = "glm", se=F, method.args=list(binomial(link = "probit")),
              color ="orange",
              alpha=0.6)
