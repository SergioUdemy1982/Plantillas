# Vectores

u <- c(1,2,-3)
v <- c(2,-1,3)
w <- c(-1,3, 4)

# Propiedad conmutativa
# u+v = v+u
u+v == v+u

# Propiedad asociativa
# (u+v)+w = u+(v+w)
(u+v)+w == u+(v+w)

# Elemento neutro bajo la suma
# u+0=u
u+0 == u


# Producto punto (Producto escalar)
#
# u * v = u1v1 + u2v2 + ... + unvn

u %*% v


# Propiedades del producto punto
u %*% v   # u*v = v*u
u %*% (v + w) == u%*%v + u%*%w # u*(v + w) = u*v + u*w
(4*u) %*% v == 4*(u%*%v) # (cu)*v = c(u*v)
u %*% u  # u*u >= 0 si u != 0
# u*u = 0  si u = 0

# Longitud (norma) de un vector
#
# ||v|| = sqrt(v*v)=sqrt(v1^2 + v2^2 + ... + vn^2)



sqrt(v%*%v)
sqrt(w%*%w)
norma(v)

# ||cv|| = |c|*||v||
sqrt((4*v) %*% (4*v)) == abs(4)*sqrt(v%*%v)

# ||cv|| ^2 = c^2 * ||v||^2
sqrt((4*v) %*% (4*v))^2 == (4^2) * (sqrt(v%*%v)^2)

# Vector unitario: vector que tiene la misma direccion que v, pero de 
# longitud 1. Es escalar o normalizar un vector a la longitud 1 conservando la 
# misma direccion del vector original.
#
# v/||v||   es el vector v dividido entre su longitud (norma)


# Distancia Euclidiana
#
# ||u - v||
# 
# es la norma de la diferencia de dos vectores

a <- c(sqrt(2), 1, -1)
b <- c(0, 2,-2)



dEucl(u, v)


# Dos vectores son ortogonales si su producto punto es 0
# Lo anterior implica que el coseno del angulo 90 grados o pi/2 radianes es 0

v1 <- c(1,1,-2)
v2 <- c(3,1,2)

dot(v1,v2) # Estos vectores son ortogonales

anguloVD(v1,v2)
anguloVR(v1,v2)


# Teorema de Pitagoras: ||u+v|| ^2 = ||u|| ^2 + ||v|| ^2 si y solo si u y v son
# ortogonales

# Proyeccion de V sobre U
#
#  proyu(V) = (dot(u,v) / dot(u,u)) * u      u!= 0

proyVU(c(-1,3), c(2,1))
proyVU(c(1,2,3), c(0,0,1))
proyVU(c(1,2,3), c(1/2,1/2,1/sqrt(2)))
