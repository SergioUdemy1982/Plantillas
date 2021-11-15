# Clustering con K-Means
#
#
# Librerias
library(tidyverse)
library(FactoMineR)
library(factoextra)


# Dataset
dataset <- read_csv(file = "Datasets/Mall_Customers.csv")
glimpse(dataset)

# Las variables a segmentar son Anual Income y Spending Score
X <- dataset[, 4:5]
colnames(X) <- c("annual.income", "spending.score")

# Metodo del codo para calcular el K optimo
set.seed(6)
wcss <- vector() # Within Cluster Sum of Squares

for (i in 1:10){
  wcss[i] <- sum(kmeans(X,i)$withinss)
}

plot(1:10, wcss,
     type = 'b',
     main = "Metodo del Codo",
     xlab = "Clusters",
     ylab = "WCSS")

# K = 5 es el valor optimo segun la tabla

#
#
# Modelo
#
set.seed(6)

modelo <- kmeans(x = X,
                 centers = 5,
                 iter.max = 300,
                 nstart = 10)

# Modelo en Dataframe
X %>%
  mutate(age = dataset$Age,
         sex = dataset$Genre,
         cluster = modelo$cluster) -> km

# Visualizacion
library(cluster)

# Puntos
plot(x = X$annual.income,
     y = X$spending.score,
     xlab = "Anual Income",
     ylab = "Spending Score",
     main = "Scores Customers")


# Cluspot
clusplot(x = X,
         modelo$cluster, 
         lines=0,
         shade=T,
         labels= 4,
         color=T,
         plotchar = F,
         main = "Scores Clustering with K-Means",
         xlab = "Anual Income",
         ylab = "Spending Score")

#
fviz_cluster(object = modelo,
             data = X,
             geom = c("point"),
             ellipse.type = "euclid") +
  labs(title = "Scores Clustering with K-Means",
       x = "Anual Income",
       y = "Spending Score")


# ggplot2

ggplot(data = km,
       mapping = aes(x = annual.income,
                     y = spending.score,
                     #label=age,
                     color = as.factor(cluster))) +
  geom_point() +
  #geom_text(check_overlap = T) +
  stat_ellipse(aes(fill=as.factor(cluster)),
               size = 1,
               type = "norm",
               geom = "polygon",
               alpha = 0.2) +
  labs(title = "Scores Clustering with K-Means",
       x = "Anual Income",
       y = "Spending Score",
       color = "Cluster",
       label="Age") +
  guides(fill = "none", label=T)
