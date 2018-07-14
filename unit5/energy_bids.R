setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit5")

emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)

str(emails)

strwrap(emails$email[1])
emails$responsive[1]

strwrap(emails$email[2])
emails$responsive[2]

table(emails$responsive)

library(tm)

## Preprocessing

corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

corpus = tm_map(corpus, tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english") )

corpus = tm_map(corpus, stemDocument)

strwrap(corpus[[1]])

# Machine learning

dtm = DocumentTermMatrix(corpus)
dtm

dtm = removeSparseTerms(dtm, 0.97)
dtm

labelledTerms = as.data.frame(as.matrix(dtm))
labelledTerms$responsive = emails$responsive

str(labelledTerms)

library(caTools)

set.seed(144)

spl = sample.split(labelledTerms$responsive, 0.7)
train = subset(labelledTerms, spl == TRUE)
test = subset(labelledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ ., data = train, method="class")
prp(emailCART)

pred = predict(emailCART, newdata = test)
pred[1:10, ]

pred.prob = pred[, 2]

table(test$responsive, pred.prob  >= 0.5)
(195 + 25) / nrow(test)


table(test$responsive, pred.prob  >= 0.15)

# ROCR
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)

performance(predROCR, "auc")@y.values
