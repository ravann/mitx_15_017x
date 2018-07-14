setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit5")

trails = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)


trails$nchar = nchar(trails$abstract)

nchar(trails$abstract)

trails[which.max(trails$nchar), ]

table(trails$nchar > 0)



trails[which.min(nchar(trails$title)), ]$title


# Preparing the Corpus

library(tm)

corpusTitle = Corpus(VectorSource(trails$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)

corpusAbstract = Corpus(VectorSource(trails$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, sparse = 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, sparse = 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

## What is the most frequent word stem across all the abstracts? 

cols = colSums(dtmAbstract)
which.max(cols)

# Build the model

## Change columns names
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trail = trails$trial

## split into train and test
library(caTools)

set.seed(144)
spl = sample.split(dtm$trail, SplitRatio = 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trail)
730 / nrow(train)

## Model

library(rpart)
library(rpart.plot)
trailCART = rpart(trail ~ ., data = train, method = "class")
prp(trailCART)

predictTrain = predict(trailCART)
summary(predictTrain)

predictTrain = predict(trailCART, type = "class")

table(train$trail, predictTrain)
accuracy = (631 + 441) / nrow(train)
accuracy

## Sensitivity = TP / (TP + FN)
sensitivity = 441 / (131 + 441)
sensitivity

# Specificity = TN / (TN + FP)
specificity = 631 / (631 + 99)
specificity

# Evaluating the model on test set

predictTest = predict(trailCART, newdata = test, type = "class")
table(test$trail, predictTest)
(261 + 162) / nrow(test)

# ROCR
library(ROCR)

predictTest = predict(trailCART, newdata = test)
rocrPred = prediction(predictTest[, 2], test$trail)
rocrperf = performance(rocrPred, measure = "auc")
rocrperf
