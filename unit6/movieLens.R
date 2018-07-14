setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit6")

movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"", na.strings = "NA")

colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNior", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)


# Remove unnecessary columns
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Make them unique
movies = unique(movies)

str(movies)

table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

# Clustering

distances = dist(movies[2:20])
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)


clusterGroups = cutree(clusterMovies, k = 2)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

cnames = colnames(movies)
len = length(cnames)

df = data.frame(c(0), c(0))

for (i in 2:len) {
  colname = cnames[i]
  colname
  v = tapply(movies[, colname], clusterGroups, mean)
  df = rbind(df, v)
}
df = df[-c(1), ]
rownames(df) = cnames[2:len]
df
colSums(df)
