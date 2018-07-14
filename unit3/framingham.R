setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")
setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")


framingham = read.csv("framingham.csv")
summary(framingham)
str(framingham)

library(caTools)
set.seed(1000)

split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest = predict(framinghamLog, type = "response", newdata = test)

table(test$TenYearCHD, predictTest > 0.5)
modelAccuracy = (1069 + 11) / (1069 + 6 + 187 + 11)

table(test$TenYearCHD)
baselineAccuracy = (1069 + 6) / (1069 + 6 + 187 + 11)


library(ROCR)

ROCRPred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRPred, "auc")@y.values)
