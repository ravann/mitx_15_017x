setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/competition")
setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/competition")


party = read.csv("train2016.csv")
pdata = read.csv("test2016.csv")

str(party)
summary(party)

#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

cartM = rpart( Party ~ ., data = party)
prp(cartM)

# Significant variables
## Q109244, Q115611, Q113181


set.seed(1260)

library(caTools)
spl = sample.split(party, SplitRatio = 0.7)
train = subset(party, spl = TRUE)

cartM = rpart( Party ~ ., data = party)
prp(cartM)

printcp(cartM)


as.vector(as.matrix(table(party$Q109244)))
as.vector(table(party$Q115611))
as.vector(table(party$Q113181))





df = data.frame(v1, v2)

ratios = v1/v2

t = as.data.frame(table(party[, colname], party$Party))
df = data.frame(ratios)


## Build ratios of democrats for each answer
df = data.frame(c(0), c(0), c(0) )
colnames(df) = c("Blank", "No",  "Yes")
rownames(df) = c("Temp")

length = ncol(party)
cnames = colnames(party)
cnames

for (i  in 8:length) {
  colname = cnames[i]
  v1 = (table(party[, colname], party$Party))[, 1]
  v2 = (table(party[, colname], party$Party))[, 2]
  
  df = rbind(df, v1/v2)
}

df = df[-c(1), ]
rownames(df) = cnames[8:108]

summary(df)

subset(df, df$Yes > 1.5)
subset(df, df$Yes < 0.8)

subset(df, df$No > 1.5)
subset(df, df$No < 0.8)

# Split the data

set.seed(111)

library(caTools)
spl = sample.split(party, SplitRatio = 0.7)
train = subset(party, spl == TRUE)


# Build model with the above significant variables

partyCart = rpart( Party ~ Q109244 + Q99716 + Q115611 + Q113181 + Q98197 + Q99480 + Q98869, data = train)
prp(partyCart)

partyLog = glm(Party ~ Q109244 + Q99716 + Q115611 + Q113181 + Q98197 + Q99480 + Q98869, data = train, family = binomial)
summary(partyLog)

# Train set accuracy

predictCart = predict(partyCart)

table(train$Party, predictCart[, 2] > 0.5)
accuracyCart = (1584 + 826) / nrow(train)

predictLog = predict(partyLog, type = "response")
table(train$Party, predictLog > 0.5)
(1492 + 925) / nrow(train)

# Predict on final  data


predictFinalCart = predict(partyCart, newdata = pdata)
predictFinalLog = predict(partyLog, newdata = pdata)

table(predictFinalCart[, 2] > 0.5, predictFinalLog > 0.5)

table(predictFinalCart[, 2] > 0.5)
table(predictFinalLog > 0.5)



pdata$cartres = predictFinalCart[, 2] > 0.5
pdata$logres = predictFinalLog > 0.5

write.csv(pdata, "pdata.csv", row.names=FALSE)

# PredTestLabels = as.factor(ifelse(predictFinalCart[, 2]<0.5, "Democrat", "Republican"))
PredTestLabels = as.factor(ifelse(predictFinalLog < 0.5, "Democrat", "Republican"))


MySubmission = data.frame(USER_ID = pdata$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "Submission.csv", row.names=FALSE)

# Cross validation improvement

library(caret)
library(e1071)

numFolds = trainControl(method="cv", number=20)
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Party ~ ., data = party, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

train(Party ~ Q109244 + Q99716 + Q115611 + Q113181 + Q98197 + Q99480 + Q98869, data = party, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)


partyCartCV = rpart( Party ~ ., data = party, method="class", cp=0.04)
prp(partyCartCV)


