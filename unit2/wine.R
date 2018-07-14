wine <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\unit2\\wine.csv")

lreg = lm (Price ~ WinterRain + HarvestRain, data = wine)

str(lreg)
lreg$coefficients

summary(lreg)

### 

cor (wine)
