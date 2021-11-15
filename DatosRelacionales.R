# Datos Relacionales con R

# Librerias
library(tidyverse)
library(nycflights13)

# Ejemplos

tabla1 <- tribble(~key,~country_key,~city,~local,
                  1,1, "Monterrey", "FEMSA",
                  2, 2, "Los Angeles", "Apple",
                  3, 1, "Guadalajara", "Omnilife",
                  4, 1, "CDMX", "Carso",
                  5, 2, "New York", "Hilton",
                  6, 3, "Vancouver", "Price Inc",
                  7, 4, "Sao Paulo", "Odebrecht")

tabla2 <- tribble(~country_key,~country,~code,
                  1, "Mexico","MX",
                  2, "USA","US",
                  3, "Canada","CA",
                  4, "Brazil","BR")

tabla2 <- tabla2 %>% add_row(country_key = 5, country = "Argentina", code = "AR")

# Inner Join: interseccion de filas de las dos tablas. Conserva columnas de ambas
# tablas donde existe la interseccion de filas.
tabla1 %>% inner_join(tabla2, by = "country_key") # Tabla1 a la derecha
tabla2 %>% inner_join(tabla1, by = "country_key") # Tabla2 a la derecha

# Leftjoin: A partir de la tabla x, junta la tabla y, conservando columnas de
# ambas tablas. Si la tablas tablas no tienen coincidencias en filas, se 
# generan NA.
tabla1 %>% left_join(tabla2, by = "country_key") # Todas las filas de x coinciden
                                                 # con las de y

tabla2 %>% left_join(tabla1, by = "country_key") # No todas las filas de x coinciden
                                                 # con las de y. Se produce NA

# Rightjoin: inverso de left join.
tabla1 %>% right_join(tabla2, by = "country_key") # x es la tabla "principal" y junta
                                                  # lo que coincida con y. en este caso
                                                  # no hay coincidencia en ciertas filas

tabla2 %>% right_join(tabla1, by = "country_key") # Todas las filas de y coinciden
                                                  # con las de x.

# Fulljoin: reune todas las filas de x e y, incluidas las no coincidencias

tabla1 %>% full_join(tabla2, by = "country_key")
tabla2 %>% full_join(tabla1, by = "country_key")

# Semijoin: es un innerjoin SIN DUPLICADOS
tabla1 %>% semi_join(tabla2, by = "country_key") # Intersecta con y, pero solo conserva x
tabla2 %>% semi_join(tabla1, by = "country_key") # Intersecta con x, pero solo conserva y

# Antijoin: devuelve todas las filas de x que NO COINCIDEN con y, conservando
#           solo las columnas de x
tabla1 %>% anti_join(tabla2, by = "country_key") # Tabla vacia. Todo lo de x coincide
                                                 # con y.

tabla2 %>% anti_join(tabla1, by = "country_key") # Devuelve una fila que o coincide con x.

# Nestjoin devuelve todas las filas y columnas de x, agregando una columna
# de tipo lista con tibbles en cada fila con las filas de y que coinciden
# con esa fila de x.

tabla1 %>% nest_join(tabla2, by = "country_key") 
tabla2 %>% nest_join(tabla1, by = "country_key")

nj <- tabla2 %>% nest_join(tabla1, by = "country_key")

#
#--------------------------------------------------------------
vuelos <- flights %>% select(year:day,hour,origin,dest,tailnum,carrier)
View(vuelos)

# Anadir aerolinea

# leftjoin
vuelos %>% 
  left_join(airlines, by = "carrier")

vuelos %>%
  mutate(carrier_name = airlines$name[match(carrier, airlines$carrier)]) # alternativo

# Claves Duplicadas

x <- tribble(~key,~value,
             1,"X1",
             2,"X2",
             2,"X3",
             1,"X4")

y <- tribble(~key,~value,
             1,"Y1",
             2,"Y2")


x %>% left_join(y, by = "key")
y %>% left_join(x, by = "key")
x %>% inner_join(y, by = "key")
#
# --------------------------


vuelos %>% left_join(weather)
colnames(vuelos)
colnames(weather)
match(colnames(vuelos), colnames(weather))
colnames(vuelos) %in% colnames(weather)


vuelos %>% left_join(planes, by = "tailnum")

#
#
# merge() del paquete base

merge(x = tabla1, y = tabla2, all.x = T) # Leftjoin
merge(x = tabla1, y = tabla2, all.y = T) # Rightjoin
merge(x = tabla1, y = tabla2, all.x = T, all.y = T) # Fulljoin
merge(x = tabla1, y = tabla2) # Innerjoin


#
# Conjuntos
#
x <- tribble(~a,~b,
               1,1,
               2,1)

y <- tribble(~a,~b,
             1,1,
             1,2)

# Interseccion
intersect(x,y)

# Union
union(x,y)

# Diferencia
setdiff(x,y)
