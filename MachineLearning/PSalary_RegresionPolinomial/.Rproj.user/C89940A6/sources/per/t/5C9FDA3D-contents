# Modelo
#
#
# Regresion lineal simple
regresion1 <- lm(formula = Salary ~ Level, data = dataset)
# Modelo
# Salary_pred = -195333.33 + 80878.79 * Level
summary(regresion1)
anova(regresion1)


# Regresion lineal polinomica grado 2
regresion2 <- lm(formula = Salary ~ poly(Level, 2, raw = T), data = dataset)
# Modelo
# Salary_pred = 232166.67 + -132871.21 * Level + 19431.82 * Level^2
summary(regresion2)
anova(regresion2)

# Regresion polinomica grado 3
regresion3 <- lm(formula = Salary ~ poly(Level, 3, raw = T), data = dataset)
summary(regresion3)
anova(regresion3)

# Regresion polinomica grado 4
regresion4 <- lm(formula = Salary ~ poly(Level, 4, raw = T), data = dataset)
# Modelo
# Salary_pred = 184166 - 211002.3310*Level + 94765.4429*Level^2
#               - 15463.2867*Level^3 + 890.1515*Level^4

summary(regresion4)
anova(regresion4)

# Regresion Polinomica grado 5
regresion5 <- lm(formula = Salary ~ poly(Level, 5, raw=T), data = dataset)
summary(regresion5)
anova(regresion5)

# Regresion polinomica grado 6
regresion6 <- lm(formula = Salary ~ poly(Level, 6, raw = T), data = dataset)
# Modelo
#
#

summary(regresion6)
anova(regresion6)


# Regresion polinomica grado 8
regresion8 <- lm(formula = Salary ~ poly(Level, 8, raw = T), data = dataset)
summary(regresion8)
anova(regresion8)

#
#


ggplot() +
  geom_point(aes(dataset$Level, dataset$Salary)) +
  geom_line(aes(dataset$Level, predict(regresion4, dataset)),
            colour = "orange",
            size = 1) +
  stat_smooth(mapping = aes(dataset$Level, predict(regresion4, dataset)),
              method = "lm",
              formula = y ~ poly(x, 4, raw = T),
              se = T)

preds_4 <- tibble(Level = dataset$Level,
                Salary = dataset$Salary,
                Salary_pred = regresion4$fitted.values)
View(preds_4)
