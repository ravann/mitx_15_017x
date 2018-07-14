setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit5")

emails = read.csv("emails.csv", stringsAsFactors = FALSE)

summary(emails)
sum(emails$spam)

which.max(nchar(emails$text))
nchar(emails$text[2651])

which.min(nchar(emails$text))

library(tm)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, sparse = 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))

sort(colSums(subset(emailsSparse, emails$spam == 0)))

sort(colSums(subset(emailsSparse, emails$spam == 1)))


emailsSparse$spam = as.factor(emails$spam)
summary(emailsSparse$spam)
str(emailsSparse$spam)

emailsSparse$spam


# Building machine learning models

library(caTools)

set.seed(123)

spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = binomial)
summary(spamLog)


library(rpart)
library(rpart.plot)

spamCART = rpart(spam ~ ., data = train, method = "class")
prp(spamCART)

library(randomForest)

set.seed(123)

spamRF = randomForest(spam ~ ., data = train)


# Predictions - training

predictTrainLog = predict(spamLog, type="response")

sum(predictTrainLog < 0.00001)
sum(predictTrainLog > 0.99999)
sum(predictTrainLog < 0.99999 & predictTrainLog > 0.00001)

summary(spamLog)

prp(spamCART)

# accuracy log model
table(train$spam, predictTrainLog >= 0.5)
(3052 + 954) / nrow(train)

library(ROCR)
predTrainLog = prediction(predictTrainLog, train$spam)
prefTrainLog = performance(predTrainLog, measure = "auc")
prefTrainLog


# accuracy cart model

predictTrainCART = predict(spamCART)[, 2]
table(train$spam, predictTrainCART > 0.5)
(2885 + 894) / nrow(train)

predTrainCART = prediction(predictTrainCART, train$spam)
prefTrainCART = performance(predTrainCART, measure = "auc")
prefTrainCART

# accuracy RF
predictTrainRF = predict(spamRF)
table(train$spam, predictTrainRF)
(3013 + 914) / nrow(train)

predictTrainRF = predict(spamRF, type="prob")[, 2]

predTrainRF = prediction(predictTrainRF, train$spam)
prefTrainRF = performance(predTrainRF, measure = "auc")
prefTrainRF


# Predictions - testing set

# LOG

predictTestLog = predict(spamLog, newdata = test, type="response")

table(test$spam, predictTestLog >= 0.5)
(1257 + 376) / nrow(test)

predTestLog = prediction(predictTestLog, test$spam)
prefTestLog = performance(predTestLog, measure = "auc")
prefTestLog

# CART 

predictTestCART = predict(spamCART, newdata = test)[, 2]
table(test$spam, predictTestCART > 0.5)
(1228 + 386) / nrow(test)

predTestCART = prediction(predictTestCART, test$spam)
prefTestCART = performance(predTestCART, measure = "auc")
prefTestCART


# RF 

predictTestRF = predict(spamRF, newdata = test, type = "prob")[, 2]
table(test$spam, predictTestRF > 0.5)
(1290 + 385) / nrow(test)

predTestRF = prediction(predictTestRF, test$spam)
prefTestRF = performance(predTestRF, measure = "auc")
prefTestRF
