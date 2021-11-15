# Visualizacion de datos con ggplot2

# Carga de libreria tidyverse 1.3.1
library(tidyverse)

#v ggplot2 3.3.3     v purrr   0.3.4
#v tibble  3.1.1     v dplyr   1.0.6
#v tidyr   1.1.3     v stringr 1.4.0
#v readr   1.4.0     v forcats 0.5.1
#-- Conflicts -----  tidyverse_conflicts() --
#  x dplyr::filter() masks stats::filter()
#  x dplyr::lag()    masks stats::lag()

# Carga de dataset mpg de la libreria ggplot
View(mpg)
help(mpg)
#--------------------------------------#

# Identificacion de problema a resolver:
# 1.- ¿Los coches con motor grande consumen mas gasolina que los coches de motor
#      más pequeño?

# 2.- ¿La relación entre consumo y tamaño es lineal? ¿Es positiva o negativa?

# -------------------------------------#

# La clasificación de las variables es la siguiente:
# Nombre: "manufacturer"| Tipo: Cualitativa | Escala: Nominal
# Nombre: "model" | Tipo: Cualitativa | Escala: Nominal
# Nombre: "displ" | Tipo: Métrica Contínua | Escala: Intervalo
# Nombre: "year" | Tipo: Métrica Discreta | Escala: Intervalo
# Nombre: "cyl" | Tipo: Cualitativa | Escala: Ordinal
# Nombre: "trans" | Tipo: Cualitativa | Escala: Nominal
# Nombre: "drv" | Tipo: Cualitativa | Escala: Nominal
# Nombre: "cty" | Tipo: Métrica Discreta | Escala: Razón | 
# Nombre: "hwy" | Tipo: Métrica Discreta | Escala: Razón | 
# Nombre: "fl" | Tipo: Cualitativa | Escala: Nominal |
# Nombre: "class" | Tipo: Cualitativa | Escala: Nominal | 

#----------------------------------------#

#1.- ¿Los coches con motor más grande consumen más gasolina que los coches de
#     motor más pequeño?

# Variables a estudiar:
# "displ": Tamaño del motor en litros
# "hwy": Millas por galón en autopista (1 galón = 3.78 litros )

# Mapa de puntos
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

# Mapa de puntos con los puntos coloreados por la variable "class"
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = class, alpha=0.6, size=0.5))

# Mapa de puntos con estetica "size"
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = class, size = cyl))

# Mapa de puntos con diferentes esteticas
ggplot(data = mpg2) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=cyl, color=class), size=3)

# Mapa de puntos con los puntos coloreados por la variable "class"
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class), shape=1) +
  geom_point(mapping = aes(x=displ, y=cty, color=class), shape=2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = class == "compact"))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = hwy > mean(hwy),
                           shape = class == "compact"))

#-----------------------------------#
# Uso de facets
#
# facet_wrap(~<FORMULA_VARIABLE>) *La variable debe ser discreta

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=-cyl ), size=3) + 
  facet_wrap(~class, nrow = 2)

# facet_grid(<FORMULA_VARIABLE1> ~ <FORMULA_VARIABLE2>) *La variable debe ser discreta
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=hwy >= mean(hwy), color=class), size=3) + 
  facet_grid(cyl ~ fl)

#-----------------------------------#
# Otras geometrias
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= hwy >= mean(hwy))) +
  geom_smooth(mapping = aes(x=displ, y=hwy)) + 
  geom_hline(yintercept = mean(mpg$hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=hwy >= mean(hwy)), size=3, alpha=0.6) +
  geom_smooth(mapping = aes(x=displ, y=hwy, linetype=drv, color=drv), se = FALSE)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=drv)) +
  geom_smooth(mapping = aes(x=displ, y=hwy, group=drv, color=drv))

# Reutilizacion de codigo
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color=drv), size=3, alpha=0.6) +
  geom_smooth(mapping = aes(group=drv, color=drv), se = F)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color=factor(cyl)))


ggplot(data = mpg, mapping = aes(x=displ, size=2, alpha=0.6)) + 
  geom_point(mapping = aes(y=hwy, color="Highway")) +
  geom_point(mapping = aes(y=cty, color="City")) +
  labs(x= "Engine Displacement", y="Area", color="Area", title = "MPG") +
  guides(size=F, alpha=F)

#-----------------------------------------#
# Diagramas de barras

View(diamonds)

dia <- ggplot(data = diamonds)

dia +
  stat_count(mapping = aes(x=cut))

demo_dia <- diamonds %>% count(cut)
demo_dia

ggplot(data=demo_dia) +
  geom_bar(mapping = aes(x=cut, y=n), stat = "identity")


demo2 <- tribble(
                  ~cut, ~freqs,
                  "Fair", 1610,
                  "Good", 4906,
                  "Very Good", 12082,
                  "Premium", 13791,
                  "Ideal", 21551)

ggplot(data = demo2) + 
  geom_bar(mapping = aes(x=cut, y=freqs),fill="steelblue", stat = "identity")

dia + 
  geom_bar(mapping = aes(x=cut, y= ..prop.., group=1)) # Eje Y en escala de proporcion

# Uso de stat_summary

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut, y=depth, fill=cut),
    fun.min = min,
    fun.max = max,
    fun=mean
  )


# Positions (Stack, Identity, Fill, Dodge)

ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity)) + 
  geom_bar(position = "stack", alpha=0.7)

ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity)) + 
  geom_bar(position = "identity", alpha=0.7)

ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity)) + 
  geom_bar(position = "fill")

ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity)) + 
  geom_bar(position = "dodge")


# Regreso a point plot. Posiciones

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(position = "jitter")

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_jitter()


# Sistemas de coordenadas

# coord_flip()  ----> cambia los paneles de x e y

ggplot(data = mpg, mapping = aes(x=class, y= hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x=class, y= hwy)) +
  geom_boxplot() +
  coord_flip()

# coord_quickmap() ----> cambia el aspect ratio de los mapas

usa <- map_data("usa")
View(usa)

ggplot(data = usa, mapping = aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="steelblue") +
  coord_quickmap()


it <- map_data("italy")
View(it)

ggplot(data = it, mapping = aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="#008C45") +
  coord_quickmap()


# Coord_polar() ----> coordenas polares

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x=cut, fill=cut),
    show.legend = F,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x=NULL, y=NULL) +
  coord_polar()


# ggplot por capas
# Guia de plantilla
#
# ggplot(data = <DATAFRAME>) +
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>),
#                   stat = <STAT>,
#                   position = <POSITION>) +
#   <COORDINATE_FUNCTION>() +
#   <FACET_FUNCTION>()