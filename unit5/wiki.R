setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit5")

wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)

table(wiki$Vandal)

library(tm)

## Corpus Added

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded = tm_map(corpusAdded, tolower)

corpusAdded = tm_map(corpusAdded, PlainTextDocument)

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

length(stopwords("english"))

corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

dtmAdded = removeSparseTerms(dtmAdded, 0.997)
dtmAdded


wordsAdded = as.data.frame(as.matrix(dtmAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))


## Corpus Removed

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved = tm_map(corpusRemoved, tolower)

corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

length(stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

dtmRemoved = removeSparseTerms(dtmRemoved, 0.997)
dtmRemoved

wordsRemoved = as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

ncol(wordsRemoved)

# Combine the words

wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Valdal = wiki$Vandal

# Training and testing sets

library(caTools)

set.seed(123)

spl = sample.split(wikiWords$Valdal, SplitRatio = 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# Baseline accuracy
table(test$Valdal)
618 / (618 + 545)

# CART Model

library(rpart)
library(rpart.plot)

wikiCART = rpart(Valdal ~ ., data = train, method = "class")
predictCART = predict(wikiCART, newdata = test, type="class")

table(test$Valdal, predictCART)

# CART Accuracy
(618 + 12) / nrow(test)

prp(wikiCART)

# Problem specific knowledge

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)

wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Valdal ~ ., data = wikiTrain2, method = "class")
prp(wikiCART2)

predictCART2 = predict(wikiCART2, newdata = wikiTest2, type = "class")

table(wikiTest2$Valdal, predictCART2)

# Accuracy
(609 + 57) / nrow(wikiTest2)



# Words added / removed

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmRemoved = DocumentTermMatrix(corpusRemoved)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

wikiCART3 = rpart(Valdal ~ ., data = wikiTrain3, method = "class")
prp(wikiCART3)

predictCART3 = predict(wikiCART3, newdata = wikiTest3, type = "class")

table(wikiTest3$Valdal, predictCART3)

# Accuracy
(514 + 248) / nrow(wikiTest3)


# Using non text data

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCART4 = rpart(Valdal ~ ., data = wikiTrain4, method = "class")

predictCART4 = predict(wikiCART4, newdata = wikiTest4, type="class")

table(wikiTest4$Valdal, predictCART4)

(595 + 241) / nrow(wikiTest4)

prp(wikiCART4)
