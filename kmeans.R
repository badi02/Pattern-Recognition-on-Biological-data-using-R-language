# import data
require(MASS)
require(ggplot2)
library(cluster)
set.seed(1234)

# function to calculate kmeans
kclustering <- function(data, K = 4, stop_crit = 10e-5) { # nolint
  #Initialisation of clusters
  centroids <- data[sample.int(nrow(data), K), ]
  current_stop_crit <- 1000
  cluster <- rep(0, nrow(data))
  converged <- FALSE
  it <- 1
  while ((current_stop_crit >= stop_crit) & (converged == FALSE)) { # nolint
    it <- it + 1
    if (current_stop_crit <= stop_crit) {
      converged <- TRUE
    }
    old_centroids <- centroids
    ##Assigning each point to a centroid
    for (i in nrow(data)) {
      min_dist <- 10e10
      for (centroid in nrow(centroids)) {
        distance_to_centroid <- sum((centroids[centroid, ] - data[i, ]) ^ 2)
        if (distance_to_centroid <= min_dist) {
          cluster[i] <- centroid
          min_dist <- distance_to_centroid
        }
      }
    }
    ##Assigning each point to a centroid
    for (i in nrow(centroids)) {
      centroids[i, ] <- apply(data[cluster == i, ], 2, mean)
    }
    current_stop_crit <- mean((old_centroids - centroids)^2)
  }
  return(list(data = data.frame(data, cluster), centroids = centroids))
}

#species = as.factor(aquization$Specie) # nolint
#species = levels(species) # nolint
#k = length(species) # nolint
######
# apply kmeans for k=10
kmns <- kclustering(data = result_matrix, K = 10)
#str(kmns) # nolint
centroids <- kmns$centroids
#View(centroids) # nolint
#dissE = daisy(kmns) # nolint
data <- as.data.frame(kmns$data)
clusters <- as.data.frame(data["cluster"])
#View(data) # nolint
#View(clusters) # nolint
library(ggplot2)
#ggplot(aquization, aes(x = aquization[1], y = clusters)) + geom_point() # nolint
#ggplot(clusters)+geom_point(aes(protein, cluster), colour = "red") # nolint

grp <- as.data.frame(cbind(aquization$Name, clusters$cluster))
#str(grp) # nolint
library(ggforce)
#png('visualization10.png') # nolint
ggplot(grp, aes(V1, V2, colour = V1)) + geom_point()
#dev.off() # nolint

write.csv(x = grp, row.names = aquization$ID, file = "groupes.csv")

######
# apply kmeans for k=7
kmns2 <- kclustering(data = result_matrix, K = 7)
#str(kmns2) # nolint
centroids2 <- kmns2$centroids
View(centroids2)
# dissE2 = daisy(kmns2) # nolint
data2 <- as.data.frame(kmns2$data)
clusters2 <- as.data.frame(data2["cluster"])
View(data2)
View(clusters2)
library(ggplot2)
#ggplot(aquization, aes(x = aquization[1], y = clusters)) + geom_point() # nolint
#ggplot(clusters)+geom_point(aes(protein, cluster), colour = "red") # nolint

grp2 <- as.data.frame(cbind(aquization$Name, clusters2$cluster))
#str(grp2) # nolint
library(ggforce)
png("visualization7.png")
ggplot(grp2[200, ], aes(V1, V2, colour = V1)) + geom_point()
write.csv(x = grp2, row.names = aquization$ID, file = "groupes2.csv")
######
# apply kmeans for k=5
kmns3 <- kclustering(data = result_matrix, K = 5)
#str(kmns3) # nolint
centroids3 <- kmns3$centroids
#View(centroids3) # nolint
# dissE3 = daisy(kmns3) # nolint
data3 <- as.data.frame(kmns3$data)
clusters3 <- as.data.frame(data3["cluster"])
#View(data3) # nolint
#View(clusters3) # nolint
library(ggplot2)
#ggplot(aquization, aes(x = aquization[1], y = clusters)) + geom_point() # nolint
#ggplot(clusters)+geom_point(aes(protein, cluster), colour = "red") # nolint

grp3 <- as.data.frame(cbind(aquization$Name, clusters3$cluster))
#str(grp3) # nolint
library(ggforce)
#png('visualization5.png') # nolint
ggplot(grp3, aes(V1, V2, colour = V1)) + geom_point()
#dev.off() # nolint

write.csv(x = grp3, row.names = aquization$ID, file = "groupes3.csv")


#######
identical(clusters, clusters2)
identical(clusters3, clusters2)
identical(clusters3, clusters)

identical(grp, grp2)
identical(grp3, grp2)
identical(grp3, grp)

identical(centroids, centroids2)
identical(centroids3, centroids2)
identical(centroids3, centroids)

identical(data, data2)
identical(data3, data2)
identical(data3, data)

color <- c("#2E9FDF", "#00AFBB", "#E7B800","#C0C0C0","#000000","#FF0000","#FFFF00","#800000","#808000","#00FF00","#008080","#008000","#0000FF","#FF00FF","#800080","#00FFFF","	#800080") # nolint
#library(factoextra) # nolint
#name = as.matrix(grp$V1) # nolint
#result = as.matrix(clusters) # nolint
#fviz_cluster(result, data = grp, palette = color[1:10],
#             geom = "point",
#             ellipse.type = "covex",
#             ggtheme = theme_bw())
# find the optimal number of k
fviz_nbclust(data, kmeans, "wss") + labs(subtitle = "Elbow Method")

# visualize the clustering algorithm result
fviz_cluster(list(data = data, cluster = clusters), palette = color[1:10])