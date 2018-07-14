setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit4")

gerber = read.csv("gerber.csv")
str(gerber)
table(gerber$voting)
108696 / nrow(gerber)

# Problem 1 - Exploration and Logistic Regression

table(gerber$civicduty)
table(gerber$hawthorne)
table(gerber$self)
table(gerber$neighbors)

summary(gerber)
tapply(gerber$voting, gerber$neighbors, mean)

# Logistic regression

model1 = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(model1)

predict1 = predict(model1, type = "response")
table(gerber$voting, predict1 >= 0.3)
accuracy1 = (134513 + 51966) / (134513 + 100875 + 56730 + 51966)
accuracy1

table(gerber$voting, predict1 >= 0.5)
235388 / nrow(gerber)


# Problem 2 - Trees

library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)


# Problem 3 - Interaction Terms

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.34 - 0.296638)

CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)
abs(0.34 - 0.296638)

model2 = glm(voting ~ control + sex, data = gerber, family = binomial)
summary(model2)

# 
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model2, newdata=Possibilities, type="response")

predict(CARTmodel5, newdata=Possibilities)


LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
predict(LogModel2, newdata = Possibilities)


predict(LogModel2, newdata=Possibilities, type="response")
  
