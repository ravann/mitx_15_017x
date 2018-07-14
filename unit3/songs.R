setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")
setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/unit3")

songs = read.csv("songs.csv")

# Problem 1 - Understanding the Data

## How many observations (songs) are from the year 2010?
nrow(subset(songs,songs$year == 2010))
table(songs$year)

## How many songs does the dataset include for which the artist name is "Michael Jackson"?

nrow(subset(songs,songs$artistname == "Michael Jackson"))

## Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.

mj = subset(songs,songs$artistname == "Michael Jackson")

subset(mj, mj$Top10 == 1)$songtitle

## The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ). What are the values of this variable that occur in our dataset? Select all that apply.
str(songs$timesignature)
table(songs$timesignature)


## Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?

songs[which.max(songs$tempo), ]

# Problem 2 - Creating Our Prediction Model

## Create training and test sets

SongsTrain = subset(songs, songs$year <= 2009)
SongsTest = subset(songs, songs$year > 2009)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTrain) %in% nonvars) ]

model1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(model1)

# Problem 3 - Beware of Multicollinearity Issues!

## What is the correlation between the variables "loudness" and "energy" in the training set?

cor(SongsTrain$loudness, SongsTrain$energy)

## Create Model 2, which is Model 1 without the independent variable "loudness". This can be done with the following command:

model2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(model2)
  
## Create Model 3, which should be exactly like Model 1, but without the variable "energy".

model3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(model3)


# Problem 4 - Validating Our Model

predict1 = predict(model3, newdata = SongsTest, type = "response")

table(SongsTest$Top10, predict1 > 0.45)

accuracy = (309 + 19) / (309 + 5 + 40 + 19)
accuracy

table(SongsTest$Top10)
baseaccuracy = 314 / (314 + 59)
baseaccuracy

# Sensitivity = TP / (TP + FN)
19 / (40 + 19)

# Specificity = TN / (TN + FP)
309 / (309 + 5)

