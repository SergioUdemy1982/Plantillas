# Analisis Exploratorio de los datos
library(tidyverse)
#
#
# Es importante preguntar acerca de como varian los datos y como es
# su covariacion
#
#
# Variacion
#
# * Variacion de variables categoricas
#   En R, las variables categoricas se expresan con el tipo de dato "factor"
#   o vector de caracteres.
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)

# En el ejemplo del dataset diamonds, la variable "cut" es una variable de tipo
# categrica con escala ordinal, puesto que las categorias se distinguen entre si
# por tener jerarquia unas repecto a otras. Una operacion muy comun que se puede
# realizar sobre este tipo de variables es contabilizar sus categorias.
#
#
# * Variacion de variables continuas
#   Este tipo de variables son metricas o numericas, de tipo descreto o continuo
#   con diferentes escalas, como puede ser intervalo o razon. Estas escalas tienen
#   caracteristicas en lo concerniente al tipo de operacion que puede ser realizada
#   sobre ellas.

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.1) +
  geom_freqpoly(mapping = aes(x = carat), binwidth = 0.1)

diamonds %>%
  count(bin = cut_width(carat, 0.2))

diamonds %>% filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# * ¿Cuales son los valores mas comunes y por que?
# * ¿Cuales son los valores mas raros?
# * ¿Que tipos de patrones se observan?

diamonds %>% filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)


# * ¿Que hace que los elementos de un grupo se parezcan entre si?
# * ¿Que hace que existan diferencias entre grupos?
# * ¿Por que algun dato se clasifica erroneamente?

View(faithful)
summary(faithful)

faithful_graphic <- ggplot(data = faithful)

faithful_graphic +
  geom_histogram(mapping = aes(x = eruptions), 
                 binwidth = 0.2, 
                 fill = "#1d3c45",
                 alpha = 0.7)

faithful_graphic +
  geom_histogram(mapping = aes(x = waiting), 
                 binwidth = 2, 
                 fill = "#d2601a",
                 alpha = 0.7)

faithful_graphic +
  geom_point(mapping = aes(x = waiting, y = eruptions, color = waiting > mean(waiting))) +
  geom_vline(xintercept = mean(faithful$waiting), colour = "#3a6b35") +
  geom_hline(yintercept = mean(faithful$eruptions), colour = "#3a6b35")


# Manejo de outliers
#
glimpse(diamonds)
summary(diamonds$price)

ggplot(data = diamonds, mapping = aes(x = 0, y = price)) +
  geom_boxplot()

# La variable "price" tiene valores atipicos. Filtramos para conocer
# que tipo de observaciones son.
# Hacemos uso del limite superior e inferior del boxplot
#
LS <- quantile(diamonds$price)[4] + (1.5 * IQR(diamonds$price))

diamonds %>% filter(price < LS) %>% 
  ggplot(mapping = aes(x = 0, y = price)) +
  geom_boxplot()

# Siguen presentandose outliers, aunque en menor cantidad. Filtramos desde el
# tercer quantil
diamonds %>% filter(price < LS) %>% filter(price < quantile(price)[4]) %>%
  ggplot(mapping = aes(x = 0, y = price)) + 
  geom_boxplot()

# Reemplazo de errores por NA

unusual_diamonds <- diamonds %>%
  filter(y < 2 | y > 30) %>%
  select(price, x, y, z) %>%
  arrange(y)

View(unusual_diamonds)

# Informacion muestra inconsistencias al tener valores para precios pero ceros
# para variables x, y y z, variables que deben ser mayores a cero. Se procede
# a reemplazar estos valores cero por NA, puesto que el NA implica una anomalia
# en la informacion recabada, pero no una inconsistencia. 

# Reemplazo de valores atipicos con NA
good_diamonds <- diamonds %>%
  filter(between(y, 2.5, 29.5))

good_diamonds <- diamonds %>%
  mutate(y = ifelse(y < 2 | y > 30, NA, y))

#------------------------------------

# Covariacion
#
# * Categoria vs Continuo

# Todas las areas bajo la curva valen 1. Mediante "density" se pueden
# comparar las areas bajo la curva
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# * Covariacion mediante boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# Si la variable categorica no es ordinal, se puede convertir a ordinal
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()
# reordena la mediana por valor

ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = reorder(cut, price, FUN = median), y = price))

# * Categoria vs Categoria
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color, colour = cut))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = cut, y =  color)) +
  geom_tile(mapping = aes(fill = desc(n)))

diamonds %>%
  select(color, cut) %>%
  table()

# Paquete para mapas de calor
# d3heatmap
# heatmaply

# * Continua vs Continua

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_point(alpha = 0.3)

ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))

library(hexbin)

ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))

# Uso de intervalos en variables continuas
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.5)))

ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_boxplot(mapping = aes(group = cut_width(Sepal.Length, 1)), varwidth = T) +
  geom_jitter(aes(colour = Sepal.Width >= mean(Sepal.Width)),
              position = position_jitter(0.05, 0.05)) +
  labs(colour = "Mean") +
  scale_color_manual(labels = c("LT", "GT"),
                      values = c("#F8766D", "#00BFC4"))
#----------------------------------------------

# Patrones
#
# Considerar:
#   
#   * Coincidencias
#   * Relaciones
#   * Fuerza de la relacion
#   * Otras variables implicadas
#   * Subgrupos

library(modelr)

modelo <- lm(data = diamonds,
             formula = log(price) ~ log(carat))

modelo$coefficients
anova(modelo)
confint(modelo)
