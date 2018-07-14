
crimes <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\mvtWeek1.csv")

max(crimes$ID)

min(crimes$Beat)

sum(as.numeric(crimes$Arrest))

sum(as.numeric(crimes$LocationDescription == "ALLEY"))

summary(crimes$Date)

DateConvert = as.Date(strptime(crimes$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)

crimes$Month = months(DateConvert)
crimes$Weekday = weekdays(DateConvert)

crimes$Date = DateConvert

# 2.4
table(crimes$Month)

# 2.5
table(crimes$Weekday)

ac <- crimes[crimes$Arrest, ]

table(ac$Month)

hist(DateConvert, breaks=100)

# 3.2

crimes$Arrest

boxplot(crimes$Date ~ crimes$Arrest)

#3.3 
crimes$year = as.numeric(as.character(crimes$Date, format = "%Y"))

c2001 <- crimes[crimes$year == 2001, ]

sum(as.numeric(c2001$Arrest)) / length(c2001$Arrest)

# 3.4

c2007 <- crimes[crimes$year == 2007, ]

sum(as.numeric(c2007$Arrest)) / length(c2007$Arrest)

# 3.5

c2012 <- crimes[crimes$year == 2012, ]

sum(as.numeric(c2012$Arrest)) / length(c2012$Arrest)

# 4.1

sort(table(crimes$LocationDescription), decreasing = TRUE)

# 4.2

Top5 <- subset(crimes, crimes$LocationDescription == "STREET" | crimes$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | crimes$LocationDescription == "ALLEY" | crimes$LocationDescription == "GAS STATION" | crimes$LocationDescription == "DRIVEWAY - RESIDENTIAL" )

# 4.3

table(Top5$LocationDescription)

Top5$LocationDescription = factor(Top5$LocationDescription)

nrow(Top5[Top5$LocationDescription == "STREET" & Top5$Arrest, ]) / nrow(Top5[Top5$LocationDescription == "STREET", ])
# 0.07405917

nrow(Top5[Top5$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" & Top5$Arrest, ]) / nrow(Top5[Top5$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)", ])
# 0.1079316

nrow(Top5[Top5$LocationDescription == "ALLEY" & Top5$Arrest, ]) / nrow(Top5[Top5$LocationDescription == "ALLEY", ])
# 0.1078856

nrow(Top5[Top5$LocationDescription == "GAS STATION" & Top5$Arrest, ]) / nrow(Top5[Top5$LocationDescription == "GAS STATION", ])
# 0.2079583

nrow(Top5[Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL" & Top5$Arrest, ]) / nrow(Top5[Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL", ])
# 0.07880597


# 4.4

table(subset(Top5, Top5$LocationDescription == "GAS STATION")$Weekday)

# 4.5

table(Top5[Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL", ]$Weekday)

