library(datasets)
data("attitude")
View(attitude)
summary(attitude)
?attitude
str(attitude)

# Subset the attitude data to make it two dimensional. Note that we can avoid that
# Depends on the target variables, what variables to subset
data <- attitude[,c(3,4)]
# Plot subset data
plot(data, main = "% of favourable responses to Learning and Privilege", pch =20, cex =2)

# We set seed to keep the clusters same each time we run the model
set.seed(7)
# data, number of clusters, nstart - the number of times we want k-means to consider 100 random clusters
km1 <- kmeans(data, 2, nstart = 100)
km1
# K-means clustering with 2 clusters of sizes 17 points and 13 points
# Cluster means - cluster centroids

# 56.2% quality of clusters, variation between clusters

# To get the clusters of each instance we can look at cluster output:
km1$cluster
# centers returns the centroids of each cluster
km1$centers
km1$withinss # variation within clusters
km1$betweenss # variation between clusters
km1$size  # number of instances in each cluster: 13, 17

plot(data, col =(km1$cluster) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data
mydata <- data
wss <- (nrow(mydata) -1)*sum(apply(mydata, 2, var))
# We can apply as 30 since we have 30 points in data
for(i in 1:15) 
  wss[i] <- sum(kmeans(mydata, centers = i)$withinss)
# gives variations between clusters
# wss[i] <- sum(kmeans(mydata, centers = i, nstart = 100)$withinss)

# Now let’s draw the scree-plot to pick the best value of k
plot(1:15, wss, type = "b", xlab = "Number of clusters", ylab = "Within groyps sum of squares", pch = 20, cex = 2)
# Now, determine a point from the above plot after which the variation becomes flat
# Next, we re-run the kmeans model with the refined number of clusters

# We can now perform the kmeans algorithm with the best k value
set.seed(7)
km2 <- kmeans(data, 6, nstart =  100)
km2
# K-means clustering with 6 clusters of sizes 4, 2, 2, 8, 6, 8

col <- (km2$cluster +1)
plot(data, col = col , main="K-Means result with 6 clusters", pch=20, cex=2)
points(km2$centers, col=col, pch=19, cex=2)
# the bigger points are the centroids of that cluster
Cluster1instances <- data[km2$cluster == 1, ]

# We can use silhouette function to compute the average silhouette width and evaluate the quality of the clusters. 
library(cluster)
?silhouette
avg_sil <- function(k)
{
  km.res <- kmeans(data, centers = k, nstart = 100)
  ss <- silhouette(km.res$cluster, dist(data))
  mean(ss[ ,3])
}

# Average of Silhouette if there are 6 clusters
avg_sil(6)
# Average of Silhouette if there are 2 clusters
avg_sil(2)
# The quality of clusters is good if the mean of silhouette coefficients is close to 1


# NOTE 
# Silhouette measure is between [-1,1]
# It is a measure of how similar an object is to its own cluster compared to other clusters


