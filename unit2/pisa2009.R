setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit2")

pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")


# Problem 1 - Dataset size
str(pisaTrain)

# Problem 1.2 - Summarizing the dataset
## Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Problem 1.3 - Locating missing values
## Which variables are missing data in at least one observation in the training set? Select all that apply.
summary(pisaTrain)

# Problem 1.4 - Removing missing values

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)


# Problem 2.1 - Factor variables


# Problem 2.2 - Unordered factors in regression models
str(pisaTrain)
str(pisaTrain$raceeth)
factor(pisaTrain$raceeth)
levels(pisaTrain$raceeth)

# Problem 3.1 - Building a model
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Problem 3.1 - Building a model
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

# Problem 3.2 - Computing the root-mean squared error of the model
lmScore$residuals

SSE = sum ( lmScore$residuals^2 ) 
SSE
RMSE = sqrt ( SSE / nrow(pisaTrain) )
RMSE

# Problem 3.3 - Comparing predictions for similar students
lmScore$coefficients["grade"]

# Problem 3.5 - Identifying variables lacking statistical significance
summary(lmScore)

# Problem 4.1 - Predicting on unseen data

predTest <- predict(lmScore, newdata = pisaTest)

## What is the range between the maximum and minimum predicted reading score on the test set?
summary (predTest)
637.7 - 353.2


# Problem 4.2 - Test set SSE and RMSE
SSE = sum( (predTest - pisaTest$readingScore)^2 )
SSE



# Problem 4.3 - Baseline prediction and test-set SSE
## What is the predicted test score used in the baseline model? Remember to compute this value using the training set and not the test set.
mean(pisaTrain$readingScore)









