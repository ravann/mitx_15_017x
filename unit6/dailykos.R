setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit6")

dailykos = read.csv("dailykos.csv")

distances = dist(dailykos, method="euclidean")

# Cluster

kosHierClust = hclust(distances, method="ward.D")

plot(kosHierClust)

clusterGroups = cutree(kosHierClust, k = 7)
str(clusterGroups)


cluster1 = subset(dailykos, clusterGroups == 1)
cluster2 = subset(dailykos, clusterGroups == 2)
cluster3 = subset(dailykos, clusterGroups == 3)
cluster4 = subset(dailykos, clusterGroups == 4)
cluster5 = subset(dailykos, clusterGroups == 5)
cluster6 = subset(dailykos, clusterGroups == 6)
cluster7 = subset(dailykos, clusterGroups == 7)

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# K-means

dailykosVec = as.vector(dailykos)

set.seed(1000)
k = 7
KMC = kmeans(dailykosVec, centers = k)
summary(KMC)
KMC$cluster

kcluster1 = subset(dailykos, KMC$cluster == 1)
kcluster2 = subset(dailykos, KMC$cluster == 2)
kcluster3 = subset(dailykos, KMC$cluster == 3)
kcluster4 = subset(dailykos, KMC$cluster == 4)
kcluster5 = subset(dailykos, KMC$cluster == 5)
kcluster6 = subset(dailykos, KMC$cluster == 6)
kcluster7 = subset(dailykos, KMC$cluster == 7)

tail(sort(colMeans(kcluster1)))
tail(sort(colMeans(kcluster2)))
tail(sort(colMeans(kcluster3)))
tail(sort(colMeans(kcluster4)))
tail(sort(colMeans(kcluster5)))
tail(sort(colMeans(kcluster6)))
tail(sort(colMeans(kcluster7)))


table(clusterGroups, KMC$cluster)
(1045) / 3430
