# Clustering Jerarquico

# Librerias
library(tidyverse)

# Dataset
dataset <- read_csv(file = "Datasets/Mall_Customers.csv")
glimpse(dataset)

# Las variables a segmentar son Anual Income y Spending Score
X <- dataset[, 4:5]
colnames(X) <- c("annual.income", "spending.score")

# Dendograma para seleccionar la cantidad de clusters
distancias <- dist(x = X, method = "euclidean")
dendrogram <- hclust(d = distancias, method = "ward.D")

plot(dendrogram, main = "Dendrograma", xlab = "Customers", ylab = "Distance")

# Ajuste de clustering Jerarquico a los datos
hc <- dendrogram

y_hc <- cutree(tree = hc, k = 5)

# Modelo
X %>%
  mutate(age = dataset$Age,
         sex = dataset$Genre,
         cluster = y_hc) -> clusters

# Graficacion

# ggplot
ggplot(data = clusters,
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
  labs(title = "Scores Clustering with Hyerarchical",
       x = "Anual Income",
       y = "Spending Score",
       color = "Cluster",
       label="Age") +
  guides(fill = "none", label=T)
