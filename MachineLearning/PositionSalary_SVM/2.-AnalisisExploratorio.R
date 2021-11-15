# Analisis Exploratorio

# Analisis Visual

plt <- ggplot(data = dataset, mapping = aes(x = as.factor(Level), y = Salary))

plt +
  geom_point(size=3,
             alpha = 0.7,
             colour = "steelblue") +
  labs(title = "Position Salary",
       x = "Level",
       y = "Salary")

ggplot(data = dataset) +
  geom_boxplot(mapping = aes(y = Salary),
               fill = "steelblue",
               alpha = 0.5,
               outlier.color = "orange") +
  labs(title = "Position Salary Boxplot",
       y = "Salary")

plt +
  geom_bar(stat = "identity",
           fill = "steelblue",
           alpha = 0.7) +
  geom_point(color = "orange",
             alpha = 0.7) +
  labs(title = "Position Salary",
       x = "Level",
       y = "Salary")


# Analisis Numerico
summary(dataset$Salary)


