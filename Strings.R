# Strings

# Librerias
library(tidyverse)

# Fundamentos de los strings

s1 <- "Este es un string"
s2 <- 'Este es un string con otro "string" anidado'

double_quote <- "\""
single_quote <- '\''
backslash <- "\\"
x <- c(double_quote, single_quote, backslash)
mu <- "\u00b5"
writeLines(x)
writeLines(s1)
writeLines(mu)

# Caracteres de escape
#
#   Salto de linea: \n
#   Tabulador: \t
#   Unicode: \u<codigo>  Ejemplo \u00b5

?'"'

str_length(s1) # Longitud de un string
str_length(x)

str_c("a","b") # junta dos o mas cadenas
str_c("a","b", "c", sep = ",")
str_c(c("a","b","c"), collapse = "|") # junta los elementos de un vector de cadenas

y = c("Alto", "Bajo", "Gordo", "Flaco")
str_c(y, collapse = " ó ")

str_sub(string = y, start = 1, end = 3) # Devuelve la string con el numero de caracteres
                                        # especificado. Aplica a cada elemento del vector

str_sub(y,-2, -1) # Opera de izquierda a derecha. -2 equivale a desde las 2 ultimas letras
                  # hasta la ultima letra, que es -1

str_to_lower(y)  # Convierte a minusculas las letras

str_sub(y,2) %>% str_to_upper()

str_to_upper(y) # Convierte a mayusculas las letras

str_to_title(c("alto","bajo", "gordo", "flaco")) # Convierte a mayuscula la primera letra

str_detect(y, "to") # detecta un patron de cadena

y[str_detect(y,"rd|jo")]


str_sort(y) # Ordena las cadenas a partir de la primera letra
str_order(y) # Ordena las cadenas y devuelve la posicion ordenada
y[str_order(y)]


# Regex
#   str_view()
#   str_view_all()
library(htmlwidgets)
library(readr)
df <- read_csv(file = "Data Science/Datasets/titanic_no_nulos.csv")


z <- c("manzana", "banana", "pera", "pomelo")
str_detect(string = z, pattern = "an")
str_view(string = z, pattern = "an")

str_view(z,".a.")  # El punto devuelve caracteres sin salto de linea
                   # en su primera ocurrencia

str_view(z,".o.")
str_view(c("abc", "a.bc","a.c.d"), "\\.")  # busca un punto
str_view(c("2df", "abc"), "[0-9]")
str_view(c("abc", "hola@gmail.com"),"[@]")

# Caracteres ancla
#
#   Inicio del string: ^
#   Final del string: $

str_view(z, "^p") # Inicia con p
str_view(z, "na$") # Termina con na


# Practica con Titanic

str_view(df$home.dest, "^Monte")

df %>%
  select(home.dest) %>%
  filter(str_detect(home.dest," NY"))

df %>%
  separate(home.dest, into = c("home", "dest"), sep = "/") %>%
  separate(home, into = c("home.city", "home.state"), sep = ",") %>%
  separate(dest, into = c("dest.city", "dest.state"), sep = ",") -> df2

df2 %>%
  select(home.city,home.state, dest.city,dest.state) %>%
  View()

df2[,13:16][is.na(df2[,13:16])] <- "NO-ID"
df2[,17][is.na(df2[,17])] <- "NO-ID"

df2 %>%
  separate(name, into = c("last.name", "first.name"), sep = ",") -> df2


df2$first.name <- str_replace(df2$first.name,". ", ", ")

df2 %>%
  separate(first.name, into = c("title", "name"), sep=",") -> df2

str_sub(string = df2$title, start = 2) 

str_sub(string = df2$name, start = 2)

df2 %>%
  relocate(c(title,name), .before = last.name) -> df2

round(df2$age) -> df2$age

#write_csv(x = df2, file = "Data Science/Datasets/titanic_tidy.csv")

#---------------------------

# Filtrado usando regex

s <- df$home.state

s[str_detect(s, "[:lower:]")]

s[str_which(s, "[:lower:]")]

sum(str_count(s, "[:punct:]"))

s[str_which(s,"[:lower:]")]

s[!str_detect(s, "[:punct:]") & !str_detect(s, "[:lower:]")]

df %>%
  filter(!str_detect(s, "[:punct:]") & !str_detect(s, "[:lower:]")) %>%
  View()

df %>%
  filter(pclass == 1,
         survived == 0,
         sex == "male") %>%
  View()


df$title[str_detect(df$title, "^M(a|e)s")]

# Cuantas veces aparece un elemento

str_count(s,"[:lower:]|[:punct:]")

str_count(s,"[:lower:]+")

df$cabin[str_detect(df$cabin,"[:alpha:]")]

#-------------------

# Pares repetidos de letras

str_view(fruit, "(..)\\1", match = T) # repeticiones solouna vez

fruit[str_detect(fruit,"(..)\\1")]

fruit[str_detect(fruit,"[:punct:]")]


# Otras herramientas

str_detect(string = fruit, pattern ="(..)\\1", negate = F) %>%
  fruit[.]

fruit %>%
  str_to_title(string = .) %>%
  str_detect(string = ., pattern = "[:blank:]") %>%
  fruit[.]

words %>%
  str_count(string = ., pattern = "y$") %>%
  sum(.)

words %>%
  str_detect(string = ., pattern = "[aeiou]",negate = T) %>%
  words[.] 


words %>%
  str_detect(string = ., pattern = "^[^aeiou]+$") %>%
  words[.]


# Promedio de vocales en el vector words

words %>%
  str_count(string = ., pattern = "[aeiou]") %>%
  mean(.)  # El vector de palabras tiene en promedio 1.99 vocales por palabra

# Tabla con vocales y consonantes

tibble(word = words,
       vowel = str_count(string = words, pattern = "[aeiou]"),
       consonant = str_count(string = words, pattern = "[^aeiou]")) -> letras
View(letras)

letras %>%
  mutate(letter = str_sub(string = words, start = 1, end = 1)) %>%
  relocate(letter, .before = word) ->letras

#write_csv(x = letras, file = "Data Science/Datasets/letras.csv")

letras %>%
  group_by(letter) %>%
  summarise(vowel.mean = mean(vowel),
            consonant.mean = mean(consonant)) %>%
  View()


letras %>%
  filter(str_detect(string = word, pattern = "^[e|i]")) %>% 
  View()

letras %>%
  group_by(vowels = as.factor(vowel)) %>%
  summarise(total = n())



# ------

head(sentences)

c <- c("red", "blue", "white", "black", "green", "orange", "purple")
d <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

colores <- str_c(c, collapse = "|")
dias <- str_c(d, collapse = "|")


sentences %>%
  str_subset(string = ., pattern = colores)  # Busca elementos de una cadena en otra

sentences %>%
  str_subset(string = ., pattern = colores) -> matches

matches %>%
  str_extract(string = ., pattern = colores) %>%
  .[. == "red"]


nombres <- "([:upper:]+)([^ ]+)"

sentences %>%
  str_subset(nombres) %>%
  str_extract(nombres) %>%
  unique(.)

# Divisiones y busqueda de strings

str_replace_all(string = c("1 carro", "2 motos", "3 llaves"),
                pattern = c("1", "2", "3"),
                replacement = c("un", "dos", "tres"))



str_split(sentences[[1]], " ")


str_locate(sentences[[1]], pattern = "a")
str_locate_all(sentences[[1]], "a")


apples <- c("manzana", "Manzana", "MANZANA")

# String con directorios

apropos("filter")

getwd()
