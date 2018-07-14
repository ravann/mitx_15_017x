setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit6")

airlines = read.csv("AirlinesCluster.csv")
str(airlines)

summary(airlines)

library(caret)

# Normalization
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)


# Hierarchical Clustering

distances = dist(airlinesNorm, method="euclidean")

airlinesClust = hclust(distances, method="ward.D")
plot(airlinesClust)

clusterGroups = cutree(airlinesClust, k = 5)
table(clusterGroups)

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)


# k-MEANS Clustering

airlinesNormVec = as.vector(airlinesNorm)
set.seed(88)
KMC = kmeans(airlinesNormVec, centers = 5, iter.max = 1000)

table(KMC$cluster)

lapply(split(airlines, KMC$cluster), colMeans)

