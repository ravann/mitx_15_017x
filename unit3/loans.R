setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")
setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")

loans = read.csv("loans.csv")

# Problem 1 - Preparing the Dataset

## What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.

table(loans$not.fully.paid)

1533 / (8045 + 1533)

## Which of the following variables has at least one missing observation? Select all that apply.

summary(loans)

## COMPLETING THE DATA

library(mice)

set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

loansi = read.csv("loans_imputed.csv")
summary(loans)
summary(loansi)
loans = loansi
rm(loansi)

# Problem 2 - Prediction Models

set.seed(144)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

model1 = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(model1)


## Predictions

predict1 = predict(model1, newdata = test, type = "response")

test$predicted.risk = predict1

## What is the accuracy of the logistic regression model

table(test$not.fully.paid, test$predicted.risk >= 0.5)

(2400 + 3) / (2400 + 13 + 457 + 3)

# Accuracy of baseline
table(test$not.fully.paid)
2413 / (2413 + 460)

# Use the ROCR package to compute the test set AUC.

library(ROCR)

rocrPred = prediction(test$predicted.risk, test$not.fully.paid)
rocrperf = performance(rocrPred, measure = "auc")
rocrperf

# Problem 3 - A "Smart Baseline"

model2 = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(model2)
predict2 = predict(model2, newdata = test, type = "response")
max(predict2)
max(predict1)

rocrPred2 = prediction(predict2, test$not.fully.paid)
rocrperf2 = performance(rocrPred2, measure = "auc")
rocrperf2


# Problem 4 - Computing the Profitability of an Investment

10 * exp(0.06 * 3)

# profit

test$profit = exp(test$int.rate * 3 ) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)

# Problem 6 - An Investment Strategy Based on Risk
highInterest = subset(test, int.rate >= 0.15)

## What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
mean(highInterest$profit)

## What proportion of the high-interest loans were not paid back in full?
table(highInterest$not.fully.paid)
110 / (110 + 327)

## 
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans = subset(highInterest, highInterest$predicted.risk <= sort(highInterest$predicted.risk, decreasing=FALSE)[100])
sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)
