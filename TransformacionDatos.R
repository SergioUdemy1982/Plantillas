# Transformacion de los datos
# Uso de la libreria dplyr
# filter | arrange | select | mutate | summarise | group_by

#----------------------------#
# Carga de librerias
library(dplyr)
library(tidyverse)
#
# Carga de archivo
library(nycflights13)

vuelos <- nycflights13::flights
#----------------------------#

#----------------------------#
# Exploracion de datos

View(head(vuelos, n=20))  # Head muestra las primeras n filas (6 por defecto) 
tail(vuelos, n=20) # Tail muestra las ultimas n filas (6 por defecto)
?nycflights13::flights
glimpse(vuelos)
#----------------------------#

#----------------------------#
# Filter
# Filtrado de datos con dplyr::filter

jan1 <- dplyr::filter(vuelos, month == 1, day == 1)
head(jan1)
glimpse(jan1)

dplyr::filter(vuelos, month == 1, distance <= 500)

summary(vuelos$distance)

vuelos %>% select(month, distance) %>% filter((month == 7 | month == 10) & distance <= 500)

vuelos %>% select(month, distance, carrier) %>% 
  filter(month %in% c(4,5,7,9) & distance < 100)

# Uso de De Morgan: 
# !(x & y) = !x | !y 
# !(x | y) = !x & !y

vuelos %>% filter(!(carrier == "UA" & dest == "JFK"))
View(vuelos %>% filter((carrier == "AA" & distance <= 500) & (month == 7 | month == 10)))


# Manejo de NA o valores nulos
# Dataframe de ejemplo para trabajo con NA's
df <- tibble(X = c(1,2,NA,4,5,NA, 10))
df
is.na(df$X)
df[is.na(df$X),] # Filtrado directo de NA
df %>% filter(is.na(X))  # Filtrado con dplyr
df %>% filter(is.na(X) | X > 5)
df[is.na(df$X),] <- mean(df$X, na.rm = T) # Se sustituyen los NA por la media
df

#-------------------------------------#
# Arrange con dplyr
# Ordenacion de filas con la funcion dplyr::arrange()

tail(vuelos %>% arrange(month, day)) # Por defecto ordena de menor a mayor
tail(vuelos %>% arrange(month, desc(day))) # Usando desc() ordena de mayor a menor

#--------------------------------------#
# Select con dplyr
# Seleccion de columnas con la funcion dplyr::select()

colnames(vuelos)  # La funcion colnames devuelve una lista con los nombres de
                  # las columnas del dataset

dplyr::glimpse(vuelos)  # dplyr::glimpse() devuelve un resumen de 
                        # las caracteristicas del dataset

vuelos %>% select(colnames(vuelos)[c(1,2,3,13,14)])

vuelos %>% select(starts_with("dep"))  # el parametro starts_with recibe
                                       # una cadena y selecciona usando regex

vuelos %>% select(ends_with("delay"))

vuelos %>% select(contains("ay"))

vuelos %>% select(matches("(.)\\1"))  # Funcion matches() usando regex

vuelos %>% select(num_range("x", 1:5)) # Busca x1, x2, x3, x4, x5

vuelos %>% rename(Mes = month) # Renombra una columna

vuelos %>% select(air_time, carrier, distance, everything()) # Ordenamiento
                                                             # de columnas

#---------------------------------#
# Mutate con dplyr
# Creacion de nuevas columnas a partir de las existentes

v <- vuelos %>% filter(month == 1 | month == 2 | month ==3) %>%
                select(month, day, ends_with("delay"), distance, air_time) %>%
                drop_na()

v <- v %>% mutate(time_gain = arr_delay - dep_delay,
             flight_speed = distance /(air_time/60))

v %>% filter(time_gain < 0) %>% arrange(time_gain)

# transmutate() genera solo las nuevas variables y prescinde de las anteriores
#
#   * Operaciones aritmeticas
#   * Agregados de funciones. Ejemplo: x / sum(x) **Proporcion**
#                                      (x - min(x)) / (max(x) - min(x)) **Escalado**
#
#   * Aritmetica modular. %/% -> cociente de division entera. %% -> resto de division
transmute(v,
          air_time,
          hour_air = air_time %/% 60,
          minute_air = air_time %% 60)

#   * Logaritmos
#   * Offset: lead() -> mueve hacia la izquierda, lag() -> mueve hacia la derecha
df <- c(1:12)
lag(df) # Recorre hacia la derecha
lead(df) # Recorre hacia la izquierda

#   * Funciones acumulativas: cumsum(), cumprod(), cummin(), cummax(), cummean()
cumsum(df)
cumprod(df)
cummin(df)
cummax(df)
dplyr::cummean(df)


#   * Comparaciones logicas >, >=, <, <=, ==, !=
v %>% transmute(Delayed = dep_delay > 0)

#   * Rankings: min_rank()
df <- c(1,2,3,3,3,NA,3,4)
dplyr::min_rank(df)

#   * Ntile: ntile() divide en cuantiles o percentiles
transmute(v,
          dep_delay,
          ntile(dep_delay, n=10))


#----------------------------------#
# Summarise con dplyr
# Resumenes estadisticos del dataset

summarise(v, delay = mean(dep_delay, na.rm = T))

z <- v %>% group_by(month)
summarise(z, Distancia_Media = mean(distance, na.rm=T))

vuelos %>% group_by(carrier) %>% summarise(DepDelay = mean(dep_delay, na.rm=T)) %>%
           mutate(DepDelay, Rank = min_rank(DepDelay)) %>% arrange(desc(DepDelay)) 


#---------------------------------#
# Pipes en dplyr
# Las pipes son tuberias que modifican una secuencia

D <- flights %>% 
  group_by(dest) %>% 
  summarise(Count = n(),
            Dist = mean(distance, na.rm = T),
            Delay = mean(arr_delay, na.rm = T)) %>%
  filter(Count > 100, dest != "HNL")

ggplot(data = D, mapping = aes(x = Dist, y = Delay)) +
  geom_point(aes(size = Count), alpha = 0.2) + 
  geom_smooth(se = F) +
  geom_text(aes(label = dest))


#-------------------------------#
# Eliminacion de NA
# !is.na()


#-------------------------------#

View(Lahman::Batting)

bateo <- dplyr::as_tibble(Lahman::Batting)
glimpse(bateo)

# Creamos un dataset de bateadores
bateador <- bateo %>%
  group_by(playerID) %>%
  summarise(Hits = sum(H, na.rm = T),
            Bats = sum(AB, na.rm = T),
            Bat.Average = round(Hits / Bats, 3)) %>%
  replace(is.na(.), 0)
 

View(bateador)

bateador %>%
  filter(Bats >= mean(Bats)) %>%
  ggplot(mapping = aes(x = Bats, y = Bat.Average)) +
  geom_point(alpha = 0.2) +
  geom_smooth()

bateador %>% mutate(Ranking = min_rank(Bat.Average))

# Filtrado usando el Rango o Cuantil
bateador %>%
  filter(Hits %in% range(Hits))

bateador %>%
  filter(Hits %in% quantile(Hits)[2:4] & Bat.Average >= 0.3)  # Hits dentro del IQR

#----------------------------------
# Agrupaciones Multiples
#
vuelos %>% group_by(month, day) %>%
  summarise(Total = n())

View(
  vuelos %>%
    drop_na() %>%
    group_by(carrier, origin, dest) %>%
    summarise(total = n())
)

#-----------------------------------
# Mutates y filters por segmento
#

vuelos2 <- vuelos %>%
  select(year, month, day, dep_delay, arr_delay, carrier,
         origin, dest, air_time, distance)

#write.csv(vuelos2, "Data Science/Datasets/vuelos.csv")

vuelos <- drop_na(vuelos)

vuelos %>%
  mutate(
    air_time,
    hour_air = air_time %/% 60,
    minute_air = air_time %% 60
  ) -> vuelos

#write.csv(vuelos, "Data Science/Datasets/vuelos.csv")

vuelos %>%
  filter(dep_delay > mean(dep_delay))

vuelos %>%
  filter(dep_delay %in% quantile(dep_delay)[4:5]) %>%
  group_by(month) %>%
  summarise(mean(distance))
