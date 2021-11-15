# Funciones Algebra Lineal

library(magrittr)

# Producto punto
dot <- function(xs, ys){
  return (xs %*% ys)
}

# Norma (longitud)
norma <- function(xs){
  d <- dot(xs, xs)
  return(sqrt(d))
}

# Normalizacion (Generar un vector unitario con la misma direccion del original)
normalizacion <- function(xs){
  n <- c(1 / (norma(xs)))
  return(n * xs)
}

# Distancia Euclidea
dEucl <- function(xs, ys){
  dif <- xs - ys
  return (norma(dif))
}

# Angulo entre vectores (grados)
anguloVD <- function(xs, ys){
  d <- dot(xs,ys)
  nu <- norma(xs)
  nv <- norma(ys)
  cosTeta <- d / (nu * nv)
  return(acos(cosTeta)*180 / pi)
}

# Angulo entre vectores (radianes)
anguloVR <- function(xs, ys){
  d <- dot(xs,ys)
  nu <- norma(xs)
  nv <- norma(ys)
  cosTeta <- d / (nu * nv)
  return(acos(cosTeta))
}

# Proyeccion de v sobre u
proyVU <- function(xs, ys){
  v <- xs
  u <- ys
  p <- c(dot(u,v) / dot(u,u))
  return(p * u)
}
