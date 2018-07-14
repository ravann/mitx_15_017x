cps <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\unit1\\CPSData.csv")

# Problem 1.1 - Loading and Summarizing the Dataset
str(cps)
summary(cps)

sort(table(cps$State)) 

# 1.4
## What proportion of interviewees are citizens of the United States?
ctznCount <- table(cps$Citizenship)
(ctznCount["Citizen, Native"] + ctznCount["Citizen, Naturalized"]) / nrow(cps)

# 1.5
## The CPS differentiates between race (with possible values American Indian, Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity. A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable. For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? (Select all that apply.)
cpsHispanic <- subset(cps, cps$Hispanic == 1)
table(cpsHispanic$Race)



# Problem 2 - Evaluating Missing Values

#2.1
## Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(cps)

#2.2
## Often when evaluating a new dataset, we try to identify if there is a pattern in the missing values in the dataset. We will try to determine if there is a pattern in the missing values of the Married variable. The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for whether the Married variable is missing. We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))

# 2.3
## How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? For this question, treat the District of Columbia as a state (even though it is not technically a state).
## How many states had all interviewees living in a metropolitan area? Again, treat the District of Columbia as a state.
table(cps$State, is.na(cps$MetroAreaCode))

# 2.4
## Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(cps$Region, is.na(cps$MetroAreaCode))

## Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))


#Problem 3 - Integrating Metropolitan Area Data

## 3.1

MetroAreaMap  <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\unit1\\MetroAreaCodes.csv")

CountryMap <- read.csv("C:\\Users\\ravan_000\\OneDrive\\Learn\\Stats\\TheAnalyticsEdge\\unit1\\CountryCodes.csv")

## 3.2 Review the new version of the CPS data frame with the summary() and str() functions. What is the name of the variable that was added to the data frame by the merge() operation?
CPS = merge(cps, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)

sum(is.na(CPS$MetroArea))

## Which of the following metropolitan areas has the largest number of interviewees?
sort(table((subset(CPS, CPS$MetroArea == "Atlanta-Sandy Springs-Marietta, GA" | CPS$MetroArea == "Baltimore-Towson, MD" | CPS$MetroArea == "Boston-Cambridge-Quincy, MA-NH" | CPS$MetroArea == "San Francisco-Oakland-Fremont, CA"))$MetroArea), decreasing = TRUE)

## Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? Hint: Use tapply() with mean, as in the previous subproblem. Calling sort() on the output of tapply() could also be helpful here.
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

## Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))


## None of the interviewees aged 14 and younger have an education value reported, so the mean value is reported as NA for each metropolitan area. To get mean (and related functions, like sum) to ignore missing values, you can pass the parameter na.rm=TRUE. Passing na.rm=TRUE to the tapply function, determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE), decreasing = TRUE)


# Problem 4 - Integrating Country of Birth Data
CPS <- merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)

## What is the name of the variable added to the CPS data frame by this merge operation?
## How many interviewees have a missing value for the new country of birth variable?
summary(CPS$Country)

## Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

## What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? For this computation, don't include people from this metropolitan area who have a missing country of birth.
mc <- CPS[which(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"), ]
res = tapply(mc$Country != "United States", mc$MetroArea, mean, na.rm = TRUE)
res["New York-Northern New Jersey-Long Island, NY-NJ-PA"]

### ANSWER from exercise
##### table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")


## Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
CPSss <- subset(CPS, CPS$MetroArea == "Boston-Cambridge-Quincy, MA-NH" | CPS$MetroArea == "Minneapolis-St Paul-Bloomington, MN-WI" | CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" | CPS$MetroArea == "Washington-Arlington-Alexandria, DC-VA-MD-WV" )
sort(tapply(CPSss$Country =="India", CPSss$MetroArea, sum, na.rm = TRUE))
## Brazil
sort(tapply(CPSss$Country =="Brazil", CPSss$MetroArea, sum, na.rm = TRUE))
## Somalia
sort(tapply(CPSss$Country =="Somalia", CPSss$MetroArea, sum, na.rm = TRUE))




