# Graficos de Curva Logistica

View(mtcars)

# Fit model
model <- glm(formula = vs ~ hp, data = mtcars, family = "binomial")

# New data with predictor variable
newdata <- data.frame(hp = seq(min(mtcars$hp), max(mtcars$hp), len=500))

# Add predicted values
newdata$vs <- predict(model, newdata, type = "response")

# plot logistic regression
plot(vs ~ hp, data=mtcars, col="steelblue")
lines(vs ~ hp, data=newdata, lwd = 2)

# Using ggplot
ggplot(data = mtcars, mapping = aes(x = hp, y = vs)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", se=F, method.args=list(family="binomial"))



