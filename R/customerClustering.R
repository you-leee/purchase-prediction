library(NbClust)


plotSoSByClusterNum <- function(data, max_clusters = 15) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:max_clusters) wss[i] <- sum(kmeans(data, centers=i)$withinss)
  plot(1:max_clusters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

fitCluster <- function(data, cluster_num) {
  fit <- kmeans(data, cluster_num)
  print(aggregate(data, by = list(fit$cluster), FUN=mean))
  
  fit
}

