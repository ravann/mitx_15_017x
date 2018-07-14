setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")

polling = read.csv("PollingData.csv")
str(polling)

table(polling$Year)
table(polling$Year, polling$State)

summary(polling)

## Dealing with Missing Data


# install.packages("mice")
library(mice)

simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

table(Train$Republican)

summary(polling)

##### A Sophisticated Baseline Method #####

polling = read.csv("PollingData_Imputed.csv")

Train = subset(polling, polling$Year == 2004 | polling$Year == 2008)
Test  = subset(polling, polling$Year == 2012 )

table(Train$Republican)

table(sign(Train$Rasmussen))

table(Train$Republican, sign(Train$Rasmussen))
cor(Train)
str(Train)
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

## Model 1

mod1 = glm(Republican ~ PropR, data=Train, family = "binomial")
summary(mod1)

pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)

## Model 2
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data=Train, family = "binomial")
summary(mod2)
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5)


##### Test Set Predictions #####

table(Test$Republican, sign(Test$Rasmussen))

TestPrediction = predict(mod2, newdata = Test)

table(Test$Republican, TestPrediction >= 0.5)

## Finding the ones missed
subset(Test, TestPrediction >= 0 & Republican == 0)
