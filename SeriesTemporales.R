# Series Temporales

# Librerias
library(reticulate)
library(lubridate)
library(tseries)
library(forecast)
library(ggplot2)
library(purrr)
library(magrittr)
library(zoo)
library(readr)

# Ejemplos

x <- as.POSIXlt("2021-10-06 10:46:12")
unclass(x)


y <- as.Date("2021-10-06")
unclass(y)

# Objeto serie de tiempo

# Simulamos datos
mydata <- runif(n = 50, min = 10, max = 45)

# Creamos objeto
myts <- ts(data = mydata,start = 1956, frequency = 4)

class(myts)
unclass(myts)
time(myts)
# Grafico
plot(myts)


# La serie empieza en el mes 3
myts2 <- ts(data = mydata, start = c(1956,3), frequency = 4)
plot(myts2)

# Nottem dataset

nottem
plot(nottem)
autoplot(nottem)

autoplot(myts) +
  labs(title = "Serie de Tiempo",
       x = "Año",
       y = "Valor")


# Datos faltantes

mydata <- read.csv(file = "Datasets/Rmissing.csv")

map(.x = mydata, .f = ~ sum(is.na(.x)))

# Segunda columna convertida a serie del tiempo
myts <- ts(data = mydata$mydata)
myts
summary(myts)

autoplot(myts)

# Zoo para localizar y rellenar NA
myts <- na.locf(myts)

# Deteccion de outliers con forecast
atipicos <- tsoutliers(myts)

myts[atipicos$index] <- atipicos$replacements

autoplot(myts)


# tsclean limpia de NA y outliers directamente
myts <- tsclean(myts)
autoplot(myts)

#----------------------------------------

# Precios de cierre de Microsoft y Starbucks

sbux.df <- read_csv(file = "Datasets/sbuxPrices.csv")
mstf.df <- read_csv(file = "Datasets/msftPrices.csv")



prices %<>%
  select(-Date)

# Objetos ts

sbux.ts <- ts(data = sbux.df$Adj.Close,
              frequency = 12,
              start = c(1993,3),
              end = c(2008,3))

mstf.ts <- ts(data = mstf.df$Adj.Close,
              frequency = 12,
              start = c(1993,3),
              end = c(2008,3))

# Subconjunto de series de tiempo

tmp <- window(x = sbux.ts, start=c(1993,3), end=c(1994,7))

# Combinar series de tiempo

sbuxmsft.ts <- cbind(Starbucks = sbux.ts, Microsoft = mstf.ts)
class(sbuxmsft.ts)


# Graficos
plot(sbux.ts,
     col="steelblue",
     lwd=2,
     ylab="Adjusted Close",
     xlab="Year",
     main="Monthly Starbucks Prices (1993-2008)")

# Graficos multiples

# Segmentado
plot(sbuxmsft.ts,
     col="steelblue",
     lwd=2,
     xlab="Year",
     main="Monthly Prices (1993-2008)")

autoplot(sbuxmsft.ts) + 
  facet_grid(Starbucks ~ Microsoft)

# Combinado

plot(sbuxmsft.ts,
     plot.type="single",
     ylab="Adjusted Close",
     xlab="Year",
     col=c("steelblue", "orange"),
     lwd = 2,)
legend("topleft",legend = c("Starbucks", "Microsoft"),
       col=c("steelblue", "orange"),
       lty=1:1,
       lwd=2:2)

#--------------------------------------------
# Objetos Zoo

# Importar con Zoo

read.zoo(file = "Datasets/sbuxPrices.csv", format = "%m/%d/%Y", sep=",", header=T)
# Creamos la fecha

fecha <- seq(as.Date("1993/3/1"), as.Date("2008/3/1"), "months")
class(fecha)

# alternativa
# as.Date(sbux.df$Date, format = "%m/%d/%Y")

# Creacion del objeto Zoo

sbux.zoo <- zoo(x = sbux.df$Adj.Close, order.by = fecha)
mstf.zoo <- zoo(x = mstf.df$Adj.Close, order.by = fecha)

# Caracteristicas del objeto Zoo

index(x = sbux.zoo)
coredata(x = sbux.zoo)
start(x = sbux.zoo)
end(x = sbux.zoo)

sbux.zoo[as.Date(c("2000-03-01","2002-04-01"))]
window(x = sbux.zoo, start = "2000-03-01", end = "2002-04-01")

# combinando series
prices <- cbind(Starbucks = sbux.zoo, Microsoft = mstf.zoo)

# Graficas

plot(prices,
     col = 4:5,
     lwd = 2,
     main="Monthly Prices",
     xlab="Year")

autoplot.zoo(object = prices)

# Obtener datos desde yahoo finance

library(tseries)

elektra <- get.hist.quote(instrument = "ELEKTRA.MX",
                          start = "2018-01-01",
                          end = "2021-10-14",
                          quote = "AdjClose",
                          provider = "yahoo",
                          origin = "1970-01-01",
                          compression = "d",
                          retclass = "zoo")


# Graficos
plot(elektra,
     col = 4,
     lwd=2,
     main="Elektra Prices (2018-2021)",
     xlab="Year",
     ylab="Close")

autoplot.zoo(elektra)

# dygraphs
library(dygraphs)

dygraph(data = elektra,
        main = "Elektra Prices (2018-2021)",
        xlab = "Year",
        ylab = "Close") %>%
  dyRangeSelector() %>%
  dyLegend(show="follow")

dygraph(data = prices,
        xlab="Year",
        ylab="Close",
        main="Monthly Prices (1993-2008)") %>%
  dyLegend(show="follow")


# Datos aleatorios
library(zoo)
library(dygraphs)

datos <- rnorm(78,0,10)
fechas <- seq(as.Date("2020-03-06"), as.Date("2020-05-22"), by="day")
as.numeric(format(fechas[1], "%j"))

miserie.ts<-ts(datos,start=c(2016,66), frequency=365)
plot(miserie.ts)

miserie.z=zoo(datos, fechas)
plot(miserie.z)
dygraph(miserie.z)

#-------------------
# Caminata Aleatoria

RW <- function(N, x0, mu, variance) {
  z <- cumsum(rnorm(n = N,
                    mean = 0,
                    sd = sqrt(variance)))
  t <- 1:N
  x <- x0 + t * mu + z
  return(x)
}

P1 <- RW(100,10,0,0.0004)
P2 <- RW(100,10,0,0.0004)


plot(P1,
     main="Random Walk",
     xlab="t",
     ylab="Price",
     ylim=c(9.7,10.3),
     typ='l',
     col="red")


#-------------
# Ruido Blanco
# Carga de datos y creacion de objeto Zoo
sbux.df <- sbux.df <- read_csv(file = "Datasets/sbuxPrices.csv")

fecha <- seq(as.Date("1993/3/1"),
             as.Date("2008/3/1"),
             "months")

class(fecha)

sbux.zoo <- zoo(x = sbux.df$Adj.Close,
                order.by = fecha)

# Ruido Blanco
wn <- rnorm(n = length(sbux.df$Adj.Close),
            mean = mean(sbux.df$Adj.Close),
            sd = sd(sbux.df$Adj.Close))

sbux.wn <- sbux.df %>%
  mutate(wn = wn)

sbux.wn.zoo <- zoo(x = sbux.wn$wn, order.by = fecha)

plot(sbux.wn.zoo)
dygraph(sbux.wn.zoo)

#---------------------
# Suavizado exponencial

### Suavizado con SMA

library("TTR")

# Ejemplo mas sencillo
# n es el orden del SMA

x = c(1,2,3,4,5,6,7)

SMA(x, n = 3) # 3er order: 2 NA's


# Ejemplo datos linces

lynxsmoothed = SMA(lynx, n = 3); lynxsmoothed
plot(lynx)
plot(lynxsmoothed)


lynxsmoothed = SMA(lynx, n = 9); lynxsmoothed
plot(lynx)
plot(lynxsmoothed)





### Suavizado exponencial ets

library(forecast)

# Funcion ets
etsmodel = ets(nottem); etsmodel

# Modelo vs original
plot(nottem, lwd = 3)
lines(etsmodel$fitted, col = "red")

# Forecast
plot(forecast(etsmodel, h = 12))

# Intervalo de prediccion: nivel de confianza 95%
plot(forecast(etsmodel, h = 12, level = 95))

# Holt Winters multiplicativo
etsmodmult = ets(nottem, model ="MMM"); #error, tendencia, estacionalidad. Defecto ZZZ

# Comparacion
plot(nottem, lwd = 3)
lines(etsmodmult$fitted, col = "red")
