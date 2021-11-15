# Conexion a API

# Librerias
library(httr)
library(jsonlite)
library(rvest)
library(stringr)

# Rutas y endpoints

url <- "https://opensky-network.org/api"

# Endpoint All State Vectors
endp1 <- "/states/all"

# Conexion
path <- "https://api.datos.gob.mx/v1/condiciones-atmosfericas"
datos <- GET(path)

# Decodificacion JSON
datos <- fromJSON(content(datos, type = "text", encoding = "utf-8"))

df <- datos$results
View(df)
