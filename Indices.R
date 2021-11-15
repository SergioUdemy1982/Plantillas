# Indices

# Numero Indice: numero que expresa el cambio relativo de precio, cantidad o valor
# comparado con un periodo base

#   * Indice Simple
#
#   P = Pt / P0 * 100
#
# donde Pt es el precio de un periodo dado, P0 es el precio base y 100 el valor base
# Al valor de P hay que restarle 100 (valor base) para obtener la variacion del precio.

# Ejemplo
# El precio de un paquete vacacional fue de $450 en el anio 2000, pero aumento 
# a $795 en 2006. ¿Cual es el indice de precios para el anio 2006 usando el anio
# 2000 como base y 100 como valor base?
#
# Pt = 795 ; P0 = 450
795 / 450 * 100
# P = 176.6667
# 176.6667 - 100 = 76.6667% fue el aumento del precio de 2000 a 2006


# Ejemplo

grapas <- tibble(year = c(1995,2000,2001,2002,2006),
                 price = c(18,20,22,23,38))

# Usando como base el anio 2000
b1 <- grapas[2,][[2]]

# Usando como base el periodo 2000-2001
b2 <- mean(grapas[2:3,][[2]])

# Usando como base el periodo 2000-2002
b3 <- mean(grapas[2:4,][[2]])

grapas %>%
  mutate(index2000 = price/b1*100,
         index2000_2001 = price/b2*100,
         index2000_2002 = price/b3*100) -> grapas
View(grapas)

grapas %>%
  pivot_longer(data = ., cols = c(3:5), names_to = "index", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = index)) +
  geom_line() +
  labs(title = "Index Grapas",
       color = "Base",
       y = "Index",
       x = "Year") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                     labels = c("2000", "2000-2001", "2000-2002"))


# Ejemplo

hd <- tibble(year = c(2001:2005),
             profit = c(1.29,1.56,1.88,2.26,2.72))

hd %>%
  mutate(index2001 = profit/hd[1,][[2]] * 100) -> hd

ggplot(data = hd, mapping = aes(x = year, y = index2001)) +
  geom_point(size=2) +
  geom_line()


#  * Promedios no ponderados
#     * Promedio Simple de los indices de precios

items <- c("Bread, 1 pound",
           "Eggs, dozen",
           "Milk, galloon",
           "Apples, 1 pound",
           "Orange Juice, 12 oz",
           "Coffee, 1 pound")

prices_1995 = c(0.77,1.85,0.88,1.46,1.58,4.40)
prices_2005 = c(0.89,1.84,1.01,1.56,1.70,4.62)

meal <- tibble(Price1995 = prices_1995,
               Price2005 = prices_2005)

rownames(meal) <- items

meal %>%
  mutate(SimpleIndex1995 = Price2005/Price1995 * 100) -> meal

meal %>%
  mutate(Variation = SimpleIndex1995 - 100) -> meal

pspr <- mean(meal$SimpleIndex1995)  # Promedio Simple de Precios Relativos

# PSPR = 108.2 indica que la media del grupo umento 8.2% de 1995 a 2005
# En este indice, la unidad de medida no afecta. Por otra parte, no pondera los 
# valores. 

#   * Indice Agregado Simple
#
#   P = sum(Pt) / sum(P0) * 100

ias = sum(meal$Price2005) / sum(meal$Price1995) * 100
# ias = 106.2157 indica que el grupo de precios agregado aumento 6.2157%
# En este indice si afecta la unidad de medida de alguna observacion.

#   * Indices Ponderados
#
#     * Metodo Laspeyres: utiliza ponderaciones en el periodo base. Tiene la ventaja
#                         que los cambios en el indice se atribuyen a los precios.
#
#       P = sum(Pt*Q0) / sum(P0*Q0) * 100

meal %>%
  mutate(Q1995 = c(50,26,102,30,40,12),
         Q2005 = c(55,20,130,40,41,12)) %>%
  relocate(Q1995, .after = Price1995) %>%
  relocate(Q2005, .after = Price2005) -> meal

sumprod <- function(p0,pt,q){
  return(sum(pt*q) / sum(p0*q) * 100)
} 

laspeyres <- sumprod(meal$Price1995,meal$Price2005, meal$Q1995)
laspeyres
# laspeyres = 108.7577 considerando la cantidad de los items para el periodo base
# Esto se puede interpretar como un aumento de 8.7577 ponderando las cantidades.

#
#
#     * Metodo de Paasche: en este metodo, las ponderaciones son las del periodo t
#                          con lo cual se tiene informacion mas actualizada.
#
#       P = sum(Pt*Qt) / sum(P0*Qt) * 100

paasche <- sumprod(p0 = meal$Price1995, pt = meal$Price2005, q = meal$Q2005)
paasche
# paasche = 109.4285, es una variacion del 9.4285% que pondera los habitos actuales
# de consumo al ponderar cantidades actuales.


#-------------------------------------------------------------------------------
#
# Indice de Precios al Consumidor
#
# Mide el cambio de precios de una canasta basica fija de bienes y servicios de 
# un periodo a otro.
#
# Este indice es muy util para calcularel ingreso real
#
# Ingreso Real = Ingreso Monetario / IPC * 100
#
# Ejemplo
realIncome <- data.frame(year = c(2000,2005),
                         income = c(25000,41200),
                         pci1982_1984 = c(170.8, 195.4))

realIncome$real_income <- realIncome$income / realIncome$pci1982_1984 * 100

#
# Ventas Deflacionarias
#
# Ventas Deflacionarias = Ventas Reales / algun indice * 100
#
# Un indice utilizado en la industria y comercio es el Indice de Precios al Productor
# el cual proporciona las variaciones de precios de distintos insumos o materias
# primas.
#
# Ejemplo
companySales <- data.frame(year = c(1982,1995,2000,2004),
                           sales = c(875000,1482000,1491000,1502000),
                           ppi = c(100,127.9,138,148.5))

companySales$constantDollars <- companySales$sales / companySales$ppi * 100

companySales %>%
  pivot_longer(data = .,
               cols = c(2:4),
               names_to = "variables",
               values_to = "values") %>%
  filter(variables != "ppi") %>%
  ggplot(data = ., mapping = aes(x = year,
                                 y = values,
                                 color = variables,
                                 label=round(values,2))) +
  geom_line() +
  geom_point() +
  geom_label() +
  labs(title = "Ventas Deflacionadas",
       subtitle = "PPI 1982 = 100",
       y = "Sales",
       x = "Year",
       color = "Sales")
