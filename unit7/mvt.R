setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit7")

mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)

mvt$Date = strptime(mvt$Date, "%m/%d/%y %H:%M")

mvt$Weekday = weekdays(mvt$Date)
mvt$hour = mvt$Date$hour

weekdayCounts = as.data.frame(table(mvt$Weekday))

str(weekdayCounts)

library(ggplot2)

weekdayCounts$Var1 = factor(weekdayCounts$Var1, ordered = TRUE, level = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Crimes over days
ggplot(weekdayCounts, aes (x = Var1, y = Freq)) + geom_line(aes(group = 1)) + xlab("Day of the week") + ylab("Thefts")

ggplot(weekdayCounts, aes (x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2) + xlab("Day of the week") + ylab("Thefts")

ggplot(weekdayCounts, aes (x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha = 0.3) + xlab("Day of the week") + ylab("Thefts")


# Heat map of crime hours

DayHourCouts = as.data.frame(table(mvt$Weekday, mvt$hour))
str(DayHourCouts)

DayHourCouts$Var1 = factor(DayHourCouts$Var1, ordered = TRUE, level = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
DayHourCouts$var1 = NULL

DayHourCouts$Hour = as.numeric(as.character(DayHourCouts$Var2))

ggplot(DayHourCouts, aes(x = Hour, y = Freq) ) + geom_line(aes(group = Var1))

ggplot(DayHourCouts, aes(x = Hour, y = Freq) ) + geom_line(aes(group = Var1, color = Var1), size = 2)

ggplot(DayHourCouts, aes(x = Hour, y = Var1) ) + geom_tile(aes(fill = Freq))
ggplot(DayHourCouts, aes(x = Hour, y = Var1) ) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts") + theme (axis.title.y = element_blank())
ggplot(DayHourCouts, aes(x = Hour, y = Var1) ) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts", low="white", high="red") + theme (axis.title.y = element_blank())


# install.packages("maps")
# install.packages("ggmap")
library(maps)
library(ggmap)

chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)

ggmap(chicago) + geom_point(data = mvt[1:100, ], aes(x = Longitude, y = Latitude) )
LatLonCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLonCounts)

LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
str(LatLonCounts)

ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq )) + scale_colour_gradient(low = "yellow", high="red")

ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq ), fill = "red" )


LatLonCountsNZ = subset(LatLonCounts, LatLonCounts$Freq > 0)
ggmap(chicago) + geom_tile(data = LatLonCountsNZ, aes(x = Long, y = Lat, alpha = Freq ), fill = "red" )
