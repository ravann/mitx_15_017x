# Load data
IBM <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\IBMStock.csv")
GE <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\GEStock.csv")
ProcterGamble <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\ProcterGambleStock.csv")
CocaCola <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\CocaColaStock.csv")
Boeing <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\BoeingStock.csv")

# Fix the date
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")


summary(IBM$Date)


# 1.4 What is the mean stock price of IBM over this time period?

mean(IBM$StockPrice)

# Problem 1.5 - Summary Statistics - What is the minimum stock price of General Electric (GE) over this time period?

min(GE$StockPrice)

# 1.6 What is the maximum stock price of Coca-Cola over this time period?

max(CocaCola$StockPrice)

# 1.7 - What is the median stock price of Boeing over this time period?

median(Boeing$StockPrice)

# 1.8 - What is the standard deviation of the stock price of Procter & Gamble over this time period?

sd(ProcterGamble$StockPrice)



# Problem 2.1 - Visualizing Stock Dynamics
## Around what year did Coca-Cola has its highest stock price in this time period?

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

CocaCola[which.max(CocaCola$StockPrice), ]

## Around what year did Coca-Cola has its lowest stock price in this time period?

CocaCola[which.min(CocaCola$StockPrice), ]

# Problem 2.2 - Visualizing Stock Dynamics
## In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 1)

abline(v=as.Date(c("2000-03-01")), lwd=2)

# Problem 2.3 - Visualizing Stock Dynamics
## Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
## In the time period shown in the plot, which stock generally has lower values?

abline(v=as.Date(c("1983-01-01")), lwd=2)
abline(v=as.Date(c("1983-12-31")), lwd=2)



# Problem 3.1 - Visualizing Stock Dynamics 1995-2005
## 

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col = "purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "orange")

abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)

# Problem 4.1 - Monthly Trends

## For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? Select all that apply.
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

# Problem 4.2 - Monthly Trends
## General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?

tapply(GE$StockPrice, months(CocaCola$Date), mean)
mean(GE$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
mean(CocaCola$StockPrice)
