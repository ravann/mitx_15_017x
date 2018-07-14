setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit2")


FluTrain <- read.csv("FluTrain.csv")

# Problem 1 - Understanding the Data

## which week corresponds to the highest percentage of ILI-related physician visits? Select the day of the month corresponding to the start of this week.
FluTrain[which.max(FluTrain$ILI), ]

## Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain[which.max(FluTrain$Queries), ]

## Let us now understand the data at an aggregate level. Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(FluTrain$ILI)

## In this problem, we will predict the natural log of the ILI variable, which can be computed in R using the log() function.
plot(FluTrain$Queries, log(FluTrain$ILI) )


# Problem 2 - Linear Regression Model

## What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

# For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between the independent and the dependent variables. What is the relationship we infer from our problem? (Don't forget that you can use the cor function to compute the correlation between two variables.)
cor = cor(log(FluTrain$ILI), FluTrain$Queries)
cor^2

# Problem 3.1 - Performance on the Test Set
FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

## What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

## What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012? Note that the relative error is calculated as
ObservedILI <- FluTest$ILI[11]
EstimatedILI = PredTest1[11]

relativeError <- (ObservedILI - EstimatedILI) / ObservedILI
relativeError

## What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?

SSE = sum ((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest) )
RMSE

## RMSE = sqrt(mean((PredTest1-FluTest$ILI)^2))
## RMSE


# Problem 4 - Training a Time Series Model
install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

## How many values are missing in the new ILILag2 variable?
summary(FluTrain)

## Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these two variables?
plot(FluTrain$ILI, FluTrain$ILILag2)


## Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable. Call this model FluTrend2.
## Which coefficients are significant at the p=0.05 level in this regression model? (Select all that apply.)

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)


#Problem 5 - Evaluating the Time Series Model in the Test Set


## Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame. How many missing values are there in this new variable?
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain) - 1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]


## Obtain test set predictions of the ILI variable from the FluTrend2 model, again remembering to call the exp() function on the result of the predict() function to obtain predictions for ILI instead of log(ILI).

predTest2 <- exp(predict(FluTrend2, newdata = FluTest))
SSE = sum( (predTest2 - FluTest$ILI)^2 )
RMSE = sqrt( SSE / nrow(FluTest) )
RMSE



