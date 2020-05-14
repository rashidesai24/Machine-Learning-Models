data(iris) 
distance = dist(iris[, 3:4], method = "euclidean") 
# distance = dist(iris[, 3:4], method = "manhattan")
# dist function computes the distance matrix of the instances by using specified distance measure. 
# default distance measure is Euclidean distance
dim(as.matrix(distance))

clusters <- hclust(distance, method = "complete" ) 
# single, complete, average, median, centroid as the agglomeration method
# hclust requires us to provide the data in the form of a distance matrix
clusters

# OR

clusters$method
clusters$dist.method
clusters$height

plot(clusters)

# we cut the clusters to get final clusters

clusterCut <- cutree(clusters, k = 3) # k= 3, cut the dendogram and give us 3 clusters. 6 preferred as well
clusterCut
plot(data, col = col, pch=20, cex=2)
table(clusterCut, iris$Species)

# setosa belongs to cluster 1, versicolor in cluster 2, virginica in cluster 3
#==========================================================

# Average method
clusters <- hclust(dist(iris[, 3:4]), method = "average")
plot(clusters)


