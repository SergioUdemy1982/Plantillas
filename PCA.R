# PCA Plantilla

# Librerias
#install.packages(c("FactoMineR", "factoextra"))
library(tidyverse)
library("FactoMineR")
library("factoextra")
library("corrplot")

# Datos
# df

# apply(X = df, MARGIN = 2, FUN = mean)
# apply(X = df, MARGIN = 2, FUN = var)

pca <- prcomp(x = df[,2:5], center = T, scale. = T)
summary(pca)

pca$rotation <- -pca$rotation # imagen especular
pca$x        <- -pca$x

biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))
text(pca, row.names(rn))

#--------------------------------------------

# PCA
res.pca <- PCA(X = df[,2:5], scale.unit = T, graph = F)
summary(res.pca)

# Eigen

eig.val <- get_eigenvalue(res.pca)
eig.val

# Grafica de proporciones de varianzas
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

# PCA resultados de variables
var <- get_pca_var(res.pca)
var

# Correlacion entre variables y los factores
fviz_pca_var(res.pca, col.var = "black")

# Calidad de la representacion
corrplot(var$cos2, is.corr = F)
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# A high cos2 indicates a good representation of the variable on the principal component. 
# In this case the variable is positioned close to the circumference of 
# the correlation circle.

# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its representation 
# on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components.
# A low cos2 indicates that the variable is not perfectly represented by the PCs.

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")


# Contribucion de las variables al PCA

# The contributions of variables in accounting for the variability in a given
# principal component are expressed in percentage.
# Variables that are correlated with PC1 (i.e., Dim.1) and PC2 (i.e., Dim.2)
# are the most important in explaining the variability in the data set.
# Variables that do not correlated with any PC or correlated with the last dimensions
# are variables with low contribution and might be removed to simplify the overall analysis.

head(var$contrib)
corrplot(var$contrib, is.corr=FALSE)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Total contribution
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Agrupamiento de variables
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

# Descripcion de la dimension

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Descripcion de la dimension 1
res.desc$Dim.1
# Descripcion de la dimension 2
res.desc$Dim.2


# Graficas de individuos
ind <- get_pca_ind(res.pca)
ind

# Coordenadas de individuos
head(ind$coord)
# Calidad de individuos
head(ind$cos2)
# Contribuuciones de individuos
head(ind$contrib)

# Calidad y contribucion por individuo
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Evita overlapping de texto
)

fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


# Grafica de barras para calidad de contribuciones individuales
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

# Biplot
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" , # Individuals color
)