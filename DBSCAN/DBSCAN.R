# Density-based clustering

library(fpc)
library(dbscan)
library(factoextra)

# Iris data
data("iris")
str(iris)
newdata <- iris[,-5]

# Obtaining optimal eps value
kNNdistplot(new, k=3)
abline(h = 0.45, lty=2)

# Density-based clustering with fpc & dbscan 
set.seed(123)
f <- fpc::dbscan(newdata, eps=0.45, MinPts = 4)
d <- dbscan::dbscan(newdata, 0.45, 4)

# Cluster visualization
fviz_cluster(d, new, geom = "point")

# Cluster membership. Noise/outlier observations are coded as 0
# A random subset is shown
f$cluster
d$cluster

