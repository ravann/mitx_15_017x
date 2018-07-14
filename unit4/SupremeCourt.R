setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit4")


stevens = read.csv("stevens.csv")

str(stevens)

# Create train & test sets

library(caTools)

set.seed(3000)

spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)

# Create model

#install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 25)
prp(StevensTree)
summary(StevensTree)

PredictCart = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCart)
CartAccuracy = (41 + 71) / (41 + 36 + 22 + 71)

table(Test$Reverse)
BaselineAccuracy = 93 / (77 + 93)

library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC


pred = prediction(PredictROC[, 2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

perf = performance(pred, measure = "auc")
perf


StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 100)
prp(StevensTree2)

StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 5)
prp(StevensTree3)


# FORESTS

#install.packages("randomForest")
library(randomForest)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest = predict(StevensForest, newdata = Test)

table(Test$Reverse, PredictForest)

ForestAccuracy = (40 + 74) / (40 + 37 + 19 + 74)


# Quick question on ramdom forest

set.seed(100)

StevensForest1 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest1 = predict(StevensForest1, newdata = Test)
table(Test$Reverse, PredictForest1)
ForestAccuracy1 = (42 + 75) / (42 + 35 + 18 + 75)
ForestAccuracy1

set.seed(200)

StevensForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest2 = predict(StevensForest2, newdata = Test)
table(Test$Reverse, PredictForest2)
ForestAccuracy2 = (43 + 77) / (43 + 34 + 16 + 77)
ForestAccuracy2


## K-fold CROSS VALIDATION

#install.packages("caret")

#install.packages("e1071")

library(caret)
library(e1071)

numFolds = trainControl(method="cv", number=10)

cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class", cp=0.18)
predictCV = predict(StevensTreeCV, newdata=Test, type="class")

table(Test$Reverse, predictCV)
CVAccuracy = (59 + 64) / (59 + 18 + 29 + 64)
CVAccuracy

## Plot the tree that we created using cross-validation. How many splits does it have?
predictCV1 = predict(StevensTreeCV, newdata=Test)

predictionCV = prediction(predictCV1[, 2], Test$Reverse)
performCV = performance(predictionCV, "tpr", "fpr")
plot(performCV)

prp(StevensTreeCV)
