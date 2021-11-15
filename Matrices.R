#-----------------------------------------#
# Manejo de matrices en R
#-----------------------------------------#
# Creacion de un vector (varias maneras)

a <- 1:9
b <- c(1:9)
c <- rep(1, 9) 

# Creacion de una matriz unidimensional

matrix(1:9)
dim(matrix(1:9)) # Dimension de la matriz

# Creacion de una matriz de 2 dimensiones
matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE) # ordenada por fila

# Creacion de una matriz a partir de varios vectores
x <- c(1:3)
y <- c(4:6)
z <- c(7:9)

rbind(x,y,z) # Por filas
cbind(x,y,z) # Por columnas


# Diagonal de una matriz
diag(matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE))

# Numero de filas y columnas
nrow(matrix(1:9, nrow = 3, ncol = 3))
ncol(matrix(1:9, nrow = 3, ncol = 3))

# Agregar columnas y filas a una matriz
A = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)

rbind(A, c(10:12)) # Filas
cbind(A, c(1,1,1)) # Columnas

# Eliminar columnas y filas a una matriz
A[, -1] # Primera columna
A[, -c(1,3)] # Primera y ultima

A[-1,] # Primera fila
A[-c(2,3),] # Segunda y tercera

subset(A, select = -1) # Primera columna

# Agregar nombres a filas y columnas

rownames(A) <- c("x", "y", "z")
colnames(A) <- c("a", "b", "c")

# Eliminar nombres de filas y columnas
rownames(A) <- NULL
colnames(A) <- NULL
unname(A) # Filas y columnas a la vez

# Acceso a elementos de una matriz
# matriz[filas, columnas]

A[1,1] # Primero elemento de la primera columna
A[2,3] # Elemento de la segunda fila y tercera columna
A[1,]  # Primera fila
A[1, 2:3] # Primera fila y columnas 2 y 3
A[, -2] # Todas las columnas excepto la 2
A[1,,drop=FALSE] # Primera fila en forma matricial
A["x", c("b", "c")] # Indexacion por nombre de filas y columnas

# Eliminar valores NA, NaN, Inf de una matriz
C <- matrix(c(14, NaN, 3, Inf, -5, 4, 1, NA), ncol = 4)
C

C[!rowSums(!is.finite(C)),] # Borra filas con valores o finitos
C[,!colSums(!is.finite(C)), drop=FALSE] # Borra columnas con valores no finitos

C[is.na(C)] # Localiza los valores nulos
C[is.na(C)] <- 1 # Sustituye los valores nulos con 1 (o cualquier valor elegido)
C
#-----------------------------------------#

#-----------------------------------------#
# Algebra lineal basica con R
#-----------------------------------------#

# Matriz Identidad
I <- diag(3) # matriz identidad de 3x3
I

I %*% A # producto punto de una matriz identidad y otra no identidad
A %*% I # produce la misma matriz

# Matriz Cero
O <- matrix(rep(0,9), nrow = 3, ncol = 3, byrow = TRUE)
O

O + A # La suma de la matriz cero a una matriz numerica es igual
A + O # a la misma matriz numerica

# Matriz Cuadrada
# Tiene el mismo numero de filas y columnas
matrix(c(1:9), nrow = 3, ncol = 3, byrow = TRUE)

# Matriz Diagonal
# Es una matriz cuadrada con ceros sobre y debajo de la diagonal
# principal

diag(c(3,2,4,1), nrow = 4, ncol = 4)

# Matriz Trangular
# Si tiene ceros bajo la diagonal principal, se denomina Matriz Triangular Superior
# Si tiene ceros sobre la diagonal principal, se denomina Matriz Triangular Inferior

# Matriz Simetrica
# Es una matriz que es igual a su transpuesta
# Ejemplo:

B <- matrix(c(2,3,-4,3,10,0,-4,0,-18), nrow = 3, ncol = 3, byrow = T)
B
t(B)
B == t(B)

# Matriz Antisimetrica
# Es antisimetrica cuando la matriz es igual al negativo de su transpuesta
# Ejemplo:

C <- matrix(c(0,2,3,-2,0,-8,-3,8,0), nrow = 3, ncol = 3, byrow = T)
C
t(C)
C == t(C)
C == -t(C)

# Matriz Ortogonal
# Es una matriz que multiplicada por su transpuesta, produce una matriz identidad
# Debe tener inversa, cumpliendo que su inversa es igual a su transpuesta
# Debe ser una matriz cuadrada y el producto punto de la matriz por su inversa
# genera una matriz identidad.
# Ejemplo:

A <- matrix(c(1/sqrt(2), 1/sqrt(2), -1/sqrt(2), 1/sqrt(2)),
            nrow = 2, ncol = 2, byrow = T)
A
t(A)
solve(A)
A %*% t(A)
A %*% solve(A)

# Matriz Normal
# Cumple con la propiedad conmutativa con la transpuesta en la multiplicacion
# Ejemplo

D <- matrix(c(2,-3,-4,-3,4,2,-4,2,6), nrow = 3, ncol = 3, byrow = T)
D
t(D)
D == t(D)
D %*% t(D)
t(D) %*% D
D %*% t(D) == t(D) %*% D

#---------------------------------------------#
# Operaciones con matrices
#---------------------------------------------#

# Traza de una matriz
# Es la suma de los elementos de la diagonal principal.
# Ejemplo:

sum(diag(B))
B

# Transpuesta de un vector y una matriz

C = matrix(c(4,-5,6), nrow = 3, ncol = 1)
C
t(C)

B = matrix(c(2,3,5,7,6,8), nrow = 3, ncol = 2, byrow = T)
B
t(B)

# Propiedades de la transpuesta de una matriz
#   * Si X es un escalar y A una matriz, entonces (XA)^T = A^T
#   * Si A es una matriz, entonces (A^T)^T = A
#   * Si A y B son matrices, entonces (A + B)^T = A^T + B^T
#   * Si A y B son matrices, entonces (AB)^T = B^T * A^T
#   * Si A, B y C son matrices, entonces (ABC)^T = C^T * B^T * A^T

# Determinante de una matriz

A = matrix(c(2,3,4,5), nrow = 2, ncol = 2, byrow = T)
A
det(A)
D = matrix(c(2,3,1,6,4,5,1,7,8), nrow = 3, ncol = 3, byrow = T)
D
det(D)

# Suma y Resta de matrices

E <- matrix(c(-1,4,2,1,8,9), nrow = 3, ncol = 2, byrow = T)
G <- matrix(c(2,3,5,7,6,8), nrow = 3, ncol = 2, byrow = T)
E
G
E + G

# Propiedades de la suma y resta de matrices
#   * A + B + C = (A + B) + C
#   * A + B = B + A
#   * A + 0 = A
#   * A + (-A) = 0
#   * A + (-B) = A - B

G - E
G + 0


# Producto de matrices
# El producto de matrices solo es posible si el numero de columnas de una 
# matriz A es igual al numero de filas de una matriz B. Esto es, si A es de
# dimension MxN, entonces B debe ser de dimension NxP, y la matriz resultante
# C es de dimension MxP.
#
#
#   * Producto de escalar por una matriz
#     Sea K un escalar y A una matriz. El producto de K * A es la matriz de A
#     cuyos elementos son multiplicados por el escalar K.
A = matrix(c(-1,4,2,1,8,9), nrow = 3, ncol = 2, byrow = T)
A
3 * A
#
#
#   * Producto de un vector por otro
B <- matrix(c(1,3,5), nrow = 1, ncol = 3, byrow = T)
C <- matrix(c(2,4,6), nrow = 3, ncol = 1, byrow = T)
B
C
B %*% C
D <- matrix(c(3,6,2,7,9), nrow = 1, ncol = 5, byrow = T)
E <- matrix(c(2,4,1,6,3), nrow = 5, ncol = 1, byrow = T)
t(D) %*% t(E)
