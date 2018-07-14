setwd("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge")

getwd()

usda <- read.csv("USDA.csv")

str(usda)

summary(usda)

which.max(usda$Sodium)

usda$Description[265]

mean(usda$Sodium)

usda$Sodium > mean(usda$Sodium, na.rm = TRUE)
as.numeric(usda$Sodium > mean(usda$Sodium, na.rm = TRUE))

# Create variables
usda$HighSodium = as.numeric(usda$Sodium > mean(usda$Sodium, na.rm = TRUE))
usda$HighProtien = as.numeric(usda$Protein > mean(usda$Protein, na.rm = TRUE))
usda$HighFat = as.numeric(usda$TotalFat > mean(usda$Protein, na.rm = TRUE))
usda$HighCarbs = as.numeric(usda$Carbohydrate > mean(usda$Carbohydrate, na.rm = TRUE))

# Summarize data
table(usda$HighSodium)
table(usda$HighSodium, usda$HighFat)
