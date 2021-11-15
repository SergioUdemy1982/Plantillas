# Fechas y Horas con Lubridate

library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

ymd("2015-06-30") # Year Month Day
dmy("23-07-2021") # Day Month Year
dmy(23072021)
dmy_hm(230720211435)
dmy_hm(200920211234)

# Crear un objeto tiempo
nycflights13::flights %>%
  select(year,month,day,hour,minute) %>%
  mutate(departure = make_datetime(year,month,day,hour,minute))


# Funcion con modulo 100

mk_datetime <- function(year, month, day, time) {
  return(make_datetime(year, month, day, time %/% 100, time %/% 100))
}

flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(dep_time = mk_datetime(year,month,day,dep_time),
         arr_time = mk_datetime(year,month, day, arr_time),
         sched_dep_time = mk_datetime(year,month, day, sched_dep_time),
         sched_arr_time = mk_datetime(year,month, day, sched_arr_time)) %>%
  select(origin, dest, ends_with("delay"), ends_with("time")) -> flights_dt

View(flights_dt)

sample_n(tbl = flights_dt, size = 100, replace = F) -> flights_dt



flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 24*60*60) # 1 dia


flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 segundos = 10 minutos


# EPOCH -> 1970-01-01 00:00
as_datetime(60*60*2018) # cada unidad representa 1 segundo
as_datetime(1000*13*120000)

as_date(365*12 + 206) # 1970-01-01 + 12 anios (365*12) + 206 dias = 1982-07-23


glimpse(flights_dt)

# Extraer componentes de fechas
year(flights_dt$dep_time)
day(flights_dt$dep_time)
wday(now(), label=T)

flights_dt %>%
  filter(day(dep_time) < 20)

flights_dt %>%
  filter(month(arr_time) == 07)


# Salidas por dia de la semana
flights_dt %>%
  mutate(wday = wday(dep_time, label = T, abbr = F)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()


# Promedio de retraso de avion por hora
flights_dt %>%
  mutate(hour = hour(sched_dep_time)) %>%
  group_by(hour) %>%
  summarise(avg_delay = mean(dep_delay), n = n()) %>%
  View() %>%
  ggplot(aes(hour, avg_delay)) +
  geom_line()

# Redondeos y actualizaciones

# redondeo de fecha por semana
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week,n)) +
  geom_line()

# Modificar una fecha 

d <- now()
d

update(d, year = 2022)

# Ajuste de dias para diferente longitud de mes

ymd("2024-02-01") %>%  # Fecha inicia en 1 de Febrero
  update(mday = 30)    # Se agregan 30 dias y se ajusta a la fecha correcta
                       # "2021-03-02"


# Escalado de las fechas de un anio a un dia para visualizar patrones
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%  # Se escalan todos los vuelos
  ggplot(aes(dep_hour, color = am(dep_time))) +
  geom_freqpoly(binwidth = 300)

flights_dt %>%
  group_by(origin) %>%
  summarise(am = sum(am(dep_time)),
            pm = sum(pm(dep_time)))


# Crear vector de fechas

dates <- seq(ymd("2014-08-04"), ymd("2021-05-29"), by = "days")

# Lapsos de tiempos
#
#   * Duraciones: numero exacto de segundos
#   * Periodos: unidades (semana, mes, anio)
#   * Intervalos: segmentos de tiempo definidos con un inicio y final

nor <- ymd_hms("2018-01-01 00:00:00", tz = "US/Central") # Dia normal
gap <- ymd_hms("2018-03-11 01:30:00", tz = "US/Central") # Inicio horario de verano
lap <- ymd_hms("2018-11-04 00:30:00", tz = "US/Central") # Fin horario de verano
leap <- ymd("2019-03-01") # Salto de anios (anio bisiesto)


# Periodos
# Trazan cambios en el tiempo, los cuales ignoran las irregularidades de la linea
# del tiempo

nor + minutes(90) 
gap + minutes(90)
lap + minutes(90)
leap + years(1)

# Duraciones
# Rastrean el paso del tiempo físico, que se desvía de la hora del reloj 
# cuando ocurren irregularidades

nor + dminutes(90)
gap + dminutes(90)
lap + dminutes(90)
leap + dyears(1)

# Intervalos
# Los intervalos representan intervalos específicos de la línea de tiempo, 
# delimitados por fechas y horas de inicio y finalización.

interval(nor, nor + minutes(90))
interval(gap, gap + minutes(90))
interval(lap, lap + minutes(90))
interval(leap, leap + years(1))

# Evaluar si una fecha esta en un intervalo de tiempo

date1 <- ymd_hms("2009-03-08 01:59:59")
date2 <- ymd_hms("2000-02-29 12:00:00")
int <- interval(date2, date1)


# Operaciones con intervalos

int_start(int) # Inicio de intervalo
int_end(int)  #  Final de intervalo


v <- c(date1, date1 + 100, date1 + 1000)
int_diff(v) # Genera sub intervalos dentro de un intervalo mayor

int2 <- interval(date2, date1 + years(3))
int_aligns(int, int2)  # Devuelve un valor logico si los intervalos se traslapan
int_overlaps(int, int2)

int_flip(int) # Invierte la direccion del intervalo

int_length(int) # Tamano del intervalo en segundos

# desplaza un intervalo hacia arriba o hacia abajo 
# en la línea de tiempo en un intervalo de tiempo

int_shift(int, days(-1))
