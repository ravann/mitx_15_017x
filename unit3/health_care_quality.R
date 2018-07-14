setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")
setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")

quality <- read.csv("quality.csv")

str(quality)

# install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics + StartedOnCombination, data=qualityTrain, family=binomial)
summary(QualityLog)

tapply( quality$PoorCare, quality$StartedOnCombination, mean)


# Predict
# Sensitivity = TP / (TP + FN)
# Specificity = TN / (TN + FP)


predictTrain = predict(QualityLog, type="response")
summary(predictTrain)

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
summary(predictTest)

# install.packages("ROCR")
library(ROCR)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

