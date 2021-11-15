# Demo 1

# Carga de datos
library(h2o)
h2o.init(nthreads = -1)

airlinesURL = "https://s3.amazonaws.com/h2o-airlines-unpacked/allyears2k.csv"
airlines.hex = h2o.importFile(path = airlinesURL,
                              destination_frame = "airlines.hex")

colnames(x = airlines.hex)
summary(airlines.hex)

# Cuantiles e histogramas
quantile(x = airlines.hex$ArrDelay, na.rm = T)
h2o.hist(x = airlines.hex$ArrDelay)

# Numero de vuelos por aeropuerto
originFlights = h2o.group_by(data = airlines.hex,
                             by = "Origin",
                             nrow("Origin"),
                             gb.control = list(na.methods="rm"))

originFlights.R = as.data.frame(originFlights)


# Vuelos por mes
flightsByMonth = h2o.group_by(data = airlines.hex,
                              by = "Month",
                              nrow("Month"),
                              gb.control = list(na.methods = "rm"))

flightsByMonth.R = as.data.frame(flightsByMonth)

# Meses con la tasa mas alta de vuelos cancelados
which(colnames(airlines.hex) == "Cancelled")

cancellationsByMonth = h2o.group_by(data = airlines.hex,
                                    by = "Month",
                                    sum("Cancelled"),
                                    gb.control = list(na.methods = "rm"))

cancellationRate = cancellationsByMonth$sum_Cancelled / flightsByMonth$nrow

rates_table = h2o.cbind(flightsByMonth$Month, cancellationRate)
rates_table.R = as.data.frame(rates_table)

# Dataset en train y test
airlines.split = h2o.splitFrame(data = airlines.hex, ratios = 0.85)

airlines.train = airlines.split[[1]]
airlines.test = airlines.split[[2]]

h2o.table(airlines.train$Cancelled)
h2o.table(airlines.test$Cancelled)

# Designamos los predictores y variable respuesta
colnames(airlines.hex)

Y = "IsDepDelayed"
X = c("Origin","Dest", "DayofMonth","Year","UniqueCarrier","DayOfWeek",
      "Month","DepTime","ArrTime","Distance")

# Modelo
airlines.glm <- h2o.glm(training_frame = airlines.train,
                        x = X,
                        y = Y,
                        family = "binomial",
                        alpha = 0.5)

summary(airlines.glm)

pred = h2o.predict(object = airlines.glm, newdata = airlines.test[, X])



h2o.confusionMatrix(airlines.glm)
h2o.auc(object = airlines.glm)
h2o.hit_ratio_table(object = airlines.glm)

h2o.shutdown()
