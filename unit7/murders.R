setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit7")

murders = read.csv("murders.csv")

str(murders)

statesMap = map_data("state")
str(statesMap)
ggplot(statesMap, aes (x = long, y = lat, group = group) ) + geom_polygon(fill = "white", color = "black")

murders$region = tolower(murders$State)

murderMap = merge(statesMap, murders, by="region")
str(murderMap)


ggplot(murderMap, aes (x = long, y = lat, group = group, fill = Murders) ) + geom_polygon(color = "black") + scale_fill_gradient(low="white", high="red", guide="legend")
ggplot(murderMap, aes (x = long, y = lat, group = group, fill = Population) ) + geom_polygon(color = "black") + scale_fill_gradient(low="white", high="red", guide="legend")

murderMap$MurderRate = murderMap$Murders * 100000 / murderMap$Population

ggplot(murderMap, aes (x = long, y = lat, group = group, fill = MurderRate) ) + geom_polygon(color = "black") + scale_fill_gradient(low="white", high="red", guide="legend")

ggplot(murderMap, aes (x = long, y = lat, group = group, fill = MurderRate) ) + geom_polygon(color = "black") + scale_fill_gradient(low="white", high="red", guide="legend", limits = c(0, 10))

