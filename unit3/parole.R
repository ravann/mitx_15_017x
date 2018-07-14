setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")
setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")

parole = read.csv("parole.csv")
str(parole)


## How many of the parolees in the dataset violated the terms of their parole?
tapply(parole$violator, parole$violator, sum)
table(parole$violator)

# Problem 2 - Preparing the Dataset

## Which variables in this dataset are unordered factors with at least three levels? Select all that apply.
summary(parole)
old_parole = parole

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(old_parole)
summary(parole)

# Problem 3 - Splitting into a Training and Testing Set

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Problem 4 - Building a Logistic Regression Model

model1 = glm(violator ~ ., data = train, family = binomial)
summary(model1)

## Consider a parolee who is male, of white race, aged 50 years at prison release, 
## from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
## did not commit multiple offenses, and committed a larceny. 

summary(train$state)

Y = -4.2411574 + 0.3869904 * 1 + 0.8867192 * 1 + -0.0001756 * 50 + -0.1238867 * 3 + 0.0802954 * 12 + 0.6837143
exp(Y)

0.1825687 / (1 + 0.1825687)

# Problem 5 - Evaluating the Model on the Testing Set

predict1 = predict(model1, newdata = test, type = "response")

max(predict1)


table(test$violator, predict1 >= 0.5)

# Sensitivity = TP / (TP + FN)
12 / (12 + 11)

# Specificity = TN / (TN + FP)
167 / (167 + 12)

# Accuracy
(167 + 12) / (167 + 12 + 11 + 12)


# What is the accuracy of a simple model that predicts that every parolee is a non-violator?

table(test$violator)

179 / (179 + 23)

# ROCR

# library(caTools)

# install.packages("ROCR")
library(ROCR)

rocrPred = prediction(predict1, test$violator)
rocrperf = performance(rocrPred, measure = "auc")
rocrperf
plot(rocrperf)
