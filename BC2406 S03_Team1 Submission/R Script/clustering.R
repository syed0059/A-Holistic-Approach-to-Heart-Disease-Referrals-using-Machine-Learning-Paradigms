library(data.table)
library(tidyverse)
library(factoextra)
library(cluster)

setwd("D:/Work/Projects/bc2406/Project Files")

dt <- fread("heart_disease_uci_norm_kmeans.csv")
dt <- dt[,-c("num.0", "num.1", "num.2", "num.3", "num.4")]

set.seed(123)

fviz_nbclust(dt, kmeans, method = "wss", k.max=15)

fviz_nbclust(dt, kmeans, method = "silhouette", k.max=15)

kmeans10 <- kmeans(dt, 10, iter.max=10)

kmeans10 <- as.data.frame(kmeans10$cluster)
colnames(kmeans10) <- "kmeans10"
fwrite(kmeans10, "heart_disease_uci_kmeans_10.csv")

dt.pca <- prcomp(dt)

summary(dt.pca)

plot(cumsum(dt.pca$sdev^2 / sum(dt.pca$sdev^2)), type="b", ylim=0:1)
dt.pca_basis <- dt.pca$x

fwrite(dt.pca_basis, "heart_disease_uci_pca.csv")


fviz_nbclust(dt.pca_basis, kmeans, method = "wss", k.max=15)

fviz_nbclust(dt.pca_basis, kmeans, method = "silhouette", k.max=15)



hcluster = hclust(dist(dt), method= "complete")
plot(hcluster)

hcluster_tree <- cutree(hcluster, k=8)
hcluster_tree <- as.data.frame(hcluster_tree)
colnames(hcluster_tree) <- "hcluster8"
fwrite(hcluster_tree, "heart_disease_uci_hcluster_8.csv")
