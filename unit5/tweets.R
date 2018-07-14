setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit5")

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

#install.packages("tm")

library(tm)

#install.packages("SnowballC")

library(SnowballC)


corpus = Corpus(VectorSource(tweets$Tweet))

corpus
summary(corpus)

corpus[[1]]

corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus[1]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpus = tm_map(corpus, PlainTextDocument)


corpus = tm_map(corpus, removePunctuation)

length(stopwords("english"))

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

corpus = tm_map(corpus, stemDocument)
corpus[[1]]

## Word frequency
frequencies = DocumentTermMatrix(corpus)

frequencies

inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)

findFreqTerms(frequencies, lowfreq = 100)


sparse = removeSparseTerms(frequencies, sparse = 0.995)
sparse

tweetsSparse = as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

TrainSparse = subset(tweetsSparse, split == TRUE)
TestSparse = subset(tweetsSparse, split == FALSE)


# Cart Model
library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data = TrainSparse, method="class")

prp(tweetCART)

predictCART = predict(tweetCART, newdata = TestSparse, type="class")

table(TestSparse$Negative, predictCART)
(294 + 18) / nrow(TestSparse)

table(TestSparse$Negative)
300 / nrow(TestSparse)


# Random forest model
library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data = TrainSparse)

predictRF = predict(tweetRF, newdata = TestSparse)

table(TestSparse$Negative, predictRF)
(293 + 21) / nrow(TestSparse)


# Logistic regression model

tweetLog = glm(Negative ~ ., data = TrainSparse, family = binomial)
predictions = predict(tweetLog, newdata=TestSparse, type="response")
table(TestSparse$Negative, predictions)

(253 + 33) / nrow(TestSparse)

