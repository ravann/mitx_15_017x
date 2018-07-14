setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit4")


letters = read.csv("letters_ABPR.csv")

# Problem 1 - Predicting B or Not B

letters$isB = as.factor(letters$letter == "B")


library(caTools)

set.seed(1000)

spl = sample.split(letters$isB, SplitRatio = 0.5)

Train = subset(letters, spl == TRUE)
Test = subset(letters, spl == FALSE)

table(Train$isB)
1175 / (1175 + 383)

library(rpart)
library(rpart.plot)

CARTb = rpart(isB ~ . - letter, data=Train, method="class")
Predictb = predict(CARTb, newdata = Test, type="class")

table(Test$isB, Predictb)
(1118 + 340) / (1118 + 57 + 43 + 340)


library(randomForest)
set.seed(1000)
ForestB = randomForest(isB ~ . - letter, data=Train)
PredictFb = predict(ForestB, newdata = Test, type="class")
table(Test$isB, PredictFb)

(1165 + 374) / (1165 + 10 + 9 + 374)


# Problem 2 - Predicting the letters A, B, P, R

table(letters$letter)

letters$letter = as.factor( letters$letter )

set.seed(2000)

spl2 = sample.split(letters$letter, SplitRatio = 0.5)

Train2 = subset(letters, spl2 == TRUE)
Test2 = subset(letters, spl2 == FALSE)

table(Test2$letter)
401 / nrow(Test2)

CARTl = rpart(letter ~ . -isB , data=Train2, method="class")
summary(CARTl)
predictl = predict(CARTl, newdata = Test2, type = "class")

table(Test$letter, predictl)
sum(as.matrix(table(Test$letter, predictl)))
(101 + 89 + 98 + 127) / nrow(Test2)

