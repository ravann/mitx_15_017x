setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit4")

Claims = read.csv("ClaimsData.csv")
str(Claims)

summary(Claims$reimbursement2008)
summary(Claims)

table(Claims$bucket2009) / nrow(Claims)

# Split the data

library(caTools)
set.seed(88)

spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)


# Baseline method
## Baseline defined as related to 2008 Claims bucket

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

accuracyBaseline = (110138 + 10721 + 2774 + 1539 + 104) / nrow(ClaimsTest)
accuracyBaseline

PenaltyMatrix = matrix(c(0, 1, 2, 3, 4, 2, 0, 1, 2, 3, 4, 2, 0, 1, 2, 6, 4, 2, 0, 1, 8, 6, 4, 2, 0), nrow = 5, byrow = TRUE)


baseline = as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))
penaltyBaseline = baseline * PenaltyMatrix
penaltyErrorBaseline = sum(penaltyBaseline) / nrow(ClaimsTest)
penaltyErrorBaseline

## Quick questions

newBase = as.vector(table(ClaimsTest$bucket2009))
122978 / nrow(ClaimsTest)

newPM = c(0, 2, 4, 6, 8)
sum(newBase * newPM) / nrow(ClaimsTest)

# Model

library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = ClaimsTrain, method = "class", cp = 0.00005 )
prp(ClaimsTree)

PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTest)

accuracyModel = (114394 + 15954 + 24 + 201 + 0) / nrow(ClaimsTest)
accuracyModel

modelOutput = as.matrix(table(ClaimsTest$bucket2009, PredictTest))
penaltyModel = modelOutput * PenaltyMatrix
penaltyErrorModel = sum(penaltyModel) / nrow(ClaimsTest)

# Model with penalty

PenaltyMatrix
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))
prp(ClaimsTree)
modelOutput = as.matrix(table(ClaimsTest$bucket2009, PredictTest))
penaltyModel = modelOutput * PenaltyMatrix
penaltyErrorModel = sum(penaltyModel) / nrow(ClaimsTest)
modelOutput
