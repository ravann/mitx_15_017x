setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit4")

boston = read.csv("boston.csv")
str(boston)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col="blue", pch=19)

points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col="red", pch=19)
summary(boston$NOX)

points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col="green", pch=19)



plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=19)


