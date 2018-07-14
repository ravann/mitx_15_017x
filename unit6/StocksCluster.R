setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit6")

stocks = read.csv("StocksCluster.csv")

summary(stocks)

cor(stocks)

colMeans(stocks)


# Initial Logistic Regression Model

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)

# Accuracy on train data
predictLogTrain = predict(StocksModel, type = "response")
table(stocksTrain$PositiveDec, predictLogTrain > 0.5)

# Accuracy on test data
predictLogTest = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, predictLogTest > 0.5)

# Baseline accuracy
table(stocksTest$PositiveDec)
1897 / nrow(stocksTest)

# Clustering Stocks

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)

## Normalize the data
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary (normTrain$ReturnJan)
summary (normTest$ReturnJan)


set.seed(144)
km  = kmeans(normTrain, centers = 3)
table(km$cluster)

# install.packages("flexclust")
library(flexclust)

km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

summary(clusterTest)

table(clusterTest)


# Predictions

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30 + 774) / nrow(stocksTest1)

table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388 + 757) / nrow(stocksTest2)

table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49 + 13) / nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)
(467 + 1544) / (467 + 1110 + 353 + 1544)
