# Problem 1 - Loading and Summarizing the Dataset


poll <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\unit1\\AnonymityPoll.csv")


## How many interviewees responded that they use a smartphone?
length(poll$Smartphone[which(poll$Smartphone == 1)])

## How many interviewees responded that they don't use a smartphone?
length(poll$Smartphone[which(poll$Smartphone == 0)])

## How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
length(poll$Smartphone[which(is.na(poll$Smartphone))])


## Which of the following are states in the Midwest census region? (Select all that apply.)
sort(table(poll[which(poll$Region == "Midwest"), ]$State), decreasing = TRUE)

## Which was the state in the South census region with the largest number of interviewees?
sort(table(poll[which(poll$Region == "South"), ]$State), decreasing = TRUE)


# Problem 2 - Internet and Smartphone Users

## How many interviewees reported not having used the Internet and not having used a smartphone?
str(subset(poll, poll$Internet.Use == 0 & poll$Smartphone == 0, is.na = FALSE))

## How many interviewees reported having used the Internet and having used a smartphone?
str(subset(poll, poll$Internet.Use == 1 & poll$Smartphone == 1, is.na = FALSE))

## How many interviewees reported having used the Internet but not having used a smartphone?
str(subset(poll, poll$Internet.Use == 1 & poll$Smartphone == 0, is.na = FALSE))

## How many interviewees reported having used a smartphone but not having used the Internet?
str(subset(poll, poll$Internet.Use == 0 & poll$Smartphone == 1, is.na = FALSE))


## How many interviewees have a missing value for their Internet use?
summary(poll$Internet.Use)

## How many interviewees have a missing value for their smartphone use?
summary(poll$Smartphone)

## Use the subset function to obtain a data frame called "limited", which is limited to interviewees who reported Internet use or who reported smartphone use. In lecture, we used the & symbol to use two criteria to make a subset of the data. To only take observations that have a certain value in one variable or the other, the | character can be used in place of the & symbol. This is also called a logical "or" operation.
## How many interviewees are in the new data frame?
limited <- subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1, is.na = FALSE)


# Problem 3 - Summarizing Opinions about Internet Privacy

## Which variables have missing values in the limited data frame? (Select all that apply.)
summary(limited)

## What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
mean(limited$Info.On.Internet)

## How many interviewees reported a value of 0 for Info.On.Internet?
nrow(subset(limited, limited$Info.On.Internet == 0))

## How many interviewees reported the maximum value of 11 for Info.On.Internet?
nrow(subset(limited, limited$Info.On.Internet == 11))

## What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet? Note that to compute this proportion you will be dividing by the number of people who answered the Worry.About.Info question, not the total number of people in the data frame.
## ## - not correct -- tapply(limited$Info.On.Internet, is.na(limited$Worry.About.Info), mean)

nrow(subset(limited, !is.na(limited$Worry.About.Info)))

sum(limited$Info.On.Internet)

sum(limited$Info.On.Internet) / nrow(subset(limited, !is.na(limited$Worry.About.Info)))

##### EXPLANATION
##### From table(limited$Worry.About.Info), we see that 386 of interviewees worry about their info, and 404 do not. Therefore, there were 386+404=790 people who answered the question, and the proportion of them who worry about their info is 386/790=0.4886. Note that we did not divide by 792 (the total number of people in the data frame) to compute this proportion.
##### An easier way to compute this value is from the summary(limited) output. The mean value of a variable that has values 1 and 0 will be the proportion of the values that are a 1.


## What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?

table(limited$Anonymity.Possible)
sum(limited$Anonymity.Possible, na.rm = TRUE)
sum(limited$Anonymity.Possible, na.rm = TRUE) / nrow(subset(limited, !is.na(limited$Anonymity.Possible)))
                                            