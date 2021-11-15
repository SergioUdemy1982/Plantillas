# Analisis exploratorio
#
#
ggplot(data = dataset, mapping = aes(x = Level, y = Salary)) +
  geom_point() +
  labs(title = "Position Salary", x = "Level", y = "Salary")

ggplot(data = dataset, mapping = aes(x = Level, y = Salary)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ poly(x,6,raw=T),
              se=F,
              colour = "steelblue") +
  stat_smooth(method = "lm",
              formula = y ~ poly(x,2,raw=T),
              se=F,
              colour="orange")
  labs(title = "Position Salary", x = "Level", y = "Salary")
