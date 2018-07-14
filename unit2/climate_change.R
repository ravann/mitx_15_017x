setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit2")

climate_full <- read.csv("climate_change.csv")

climate_training <- subset(climate_full, climate_full$Year <= 2006)

climate_testing <- subset(climate_full, climate_full$Year > 2006)

cm1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_training)
summary(cm1)

cm2 <- lm(Temp ~ CH4 + N2O, data = climate_training)
summary(cm2)

# Problem 2 - Understanding the Model

## Compute the correlations between all the variables in the training set. Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)? Select all that apply.
cor (climate_training)


# Problem 3 - Simplifying the Model

## Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and N2O as independent variables. Remember to use the training set to build the model.
## Enter the coefficient of N2O in this reduced model:
cm3 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_training)
summary(cm3)

# Problem 4 - Automatically Building the Model

step(cm1)

cm4 <- lm(formula = Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_training)
summary(cm4)  

# Problem 5 - Testing on Unseen Data

temp_predict <- predict(cm4, newdata = climate_testing)
SSE <- sum( (temp_predict - climate_testing$Temp)^2 )
SST <- sum( (mean(climate_training$Temp) - climate_testing$Temp)^2 )
R2 <- 1 - SSE / SST
R2
