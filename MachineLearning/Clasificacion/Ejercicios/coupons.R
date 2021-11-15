# Coupons Logistic Regression

# Libraries
library(tidyverse)

# Dataset
dataset <- read_csv(file = "Datasets/cupons.csv")
glimpse(dataset)

dataset$coupons.redeemed <- 1

# modelo

# Separacion de dataset en Training y Testing
library(caTools)
set.seed(123)
split = sample.split(dataset$success, SplitRatio = 0.75)
training = subset(dataset, split == TRUE)
testing = subset(dataset, split == FALSE)

# Creacion del modelo
modelo <- glm(formula = coupons.redeemed ~ number.households,
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
