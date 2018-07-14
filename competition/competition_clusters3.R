setwd("~/Desktop/OneDrive/Learn/Stats/TheAnalyticsEdge/competition")
setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/competition")

# Read the data
party = read.csv("train2016.csv")
pdata = read.csv("test2016.csv")

# Split data into training and testing sets
library(caTools)
set.seed(1260)
spl = sample.split(party$Party, SplitRatio = 0.7)

# Create a dataframe for clustering with only independent variables
indcolnames = c("Q124742", "Q124122", "Q123464", "Q123621", "Q122769", "Q122770", "Q122771", "Q122120", "Q121699", "Q121700", "Q120978", "Q121011", "Q120379", "Q120650", "Q120472", "Q120194", "Q120012", "Q120014", "Q119334", "Q119851", "Q119650", "Q118892", "Q118117", "Q118232", "Q118233", "Q118237", "Q117186", "Q117193", "Q116797", "Q116881", "Q116953", "Q116601", "Q116441", "Q116448", "Q116197", "Q115602", "Q115777", "Q115610", "Q115611", "Q115899", "Q115390", "Q114961", "Q114748", "Q115195", "Q114517", "Q114386", "Q113992", "Q114152", "Q113583", "Q113584", "Q113181", "Q112478", "Q112512", "Q112270", "Q111848", "Q111580", "Q111220", "Q110740", "Q109367", "Q108950", "Q109244", "Q108855", "Q108617", "Q108856", "Q108754", "Q108342", "Q108343", "Q107869", "Q107491", "Q106993", "Q106997", "Q106272", "Q106388", "Q106389", "Q106042", "Q105840", "Q105655", "Q104996", "Q103293", "Q102906", "Q102674", "Q102687", "Q102289", "Q102089", "Q101162", "Q101163", "Q101596", "Q100689", "Q100680", "Q100562", "Q99982", "Q100010", "Q99716", "Q99581", "Q99480", "Q98869", "Q98578", "Q98059", "Q98078", "Q98197", "Q96024")
p = party[,  indcolnames]
for (colname in indcolnames) {
  p[, colname] = as.numeric(p[, colname])
}

# Compute the distances & create cluster
distances = dist(p, method="euclidean")
partyClust = hclust(distances, method="ward.D")
plot(partyClust)

# Create kcluster based on the above output
pVector = as.vector(p)
set.seed(88)
KMC = kmeans(pVector, centers = 4, iter.max = 100000)


# Create 4 p variables
p1 = subset(party, KMC$cluster == 1)
p2 = subset(party, KMC$cluster == 2)
p3 = subset(party, KMC$cluster == 3)
p4 = subset(party, KMC$cluster == 4)

# Build the cart models on these 8 party datasets

library(randomForest)
RF1 = randomForest( Party ~ Q124742 + Q124122 + Q123464 + Q123621 + Q122769 + Q122770 + Q122771 + Q122120 + Q121699 + Q121700 + Q120978 + Q121011 + Q120379 + Q120650 + Q120472 + Q120194 + Q120012 + Q120014 + Q119334 + Q119851 + Q119650 + Q118892 + Q118117 + Q118232 + Q118233 + Q118237 + Q117186 + Q117193 + Q116797 + Q116881 + Q116953 + Q116601 + Q116441 + Q116448 + Q116197 + Q115602 + Q115777 + Q115610 + Q115611 + Q115899 + Q115390 + Q114961 + Q114748 + Q115195 + Q114517 + Q114386 + Q113992 + Q114152 + Q113583 + Q113584 + Q113181 + Q112478 + Q112512 + Q112270 + Q111848 + Q111580 + Q111220 + Q110740 + Q109367 + Q108950 + Q109244 + Q108855 + Q108617 + Q108856 + Q108754 + Q108342 + Q108343 + Q107869 + Q107491 + Q106993 + Q106997 + Q106272 + Q106388 + Q106389 + Q106042 + Q105840 + Q105655 + Q104996 + Q103293 + Q102906 + Q102674 + Q102687 + Q102289 + Q102089 + Q101162 + Q101163 + Q101596 + Q100689 + Q100680 + Q100562 + Q99982 + Q100010 + Q99716 + Q99581 + Q99480 + Q98869 + Q98578 + Q98059 + Q98078 + Q98197 + Q96024, data = p1)
RF2 = randomForest( Party ~ Q124742 + Q124122 + Q123464 + Q123621 + Q122769 + Q122770 + Q122771 + Q122120 + Q121699 + Q121700 + Q120978 + Q121011 + Q120379 + Q120650 + Q120472 + Q120194 + Q120012 + Q120014 + Q119334 + Q119851 + Q119650 + Q118892 + Q118117 + Q118232 + Q118233 + Q118237 + Q117186 + Q117193 + Q116797 + Q116881 + Q116953 + Q116601 + Q116441 + Q116448 + Q116197 + Q115602 + Q115777 + Q115610 + Q115611 + Q115899 + Q115390 + Q114961 + Q114748 + Q115195 + Q114517 + Q114386 + Q113992 + Q114152 + Q113583 + Q113584 + Q113181 + Q112478 + Q112512 + Q112270 + Q111848 + Q111580 + Q111220 + Q110740 + Q109367 + Q108950 + Q109244 + Q108855 + Q108617 + Q108856 + Q108754 + Q108342 + Q108343 + Q107869 + Q107491 + Q106993 + Q106997 + Q106272 + Q106388 + Q106389 + Q106042 + Q105840 + Q105655 + Q104996 + Q103293 + Q102906 + Q102674 + Q102687 + Q102289 + Q102089 + Q101162 + Q101163 + Q101596 + Q100689 + Q100680 + Q100562 + Q99982 + Q100010 + Q99716 + Q99581 + Q99480 + Q98869 + Q98578 + Q98059 + Q98078 + Q98197 + Q96024, data = p2)
RF3 = randomForest( Party ~ Q124742 + Q124122 + Q123464 + Q123621 + Q122769 + Q122770 + Q122771 + Q122120 + Q121699 + Q121700 + Q120978 + Q121011 + Q120379 + Q120650 + Q120472 + Q120194 + Q120012 + Q120014 + Q119334 + Q119851 + Q119650 + Q118892 + Q118117 + Q118232 + Q118233 + Q118237 + Q117186 + Q117193 + Q116797 + Q116881 + Q116953 + Q116601 + Q116441 + Q116448 + Q116197 + Q115602 + Q115777 + Q115610 + Q115611 + Q115899 + Q115390 + Q114961 + Q114748 + Q115195 + Q114517 + Q114386 + Q113992 + Q114152 + Q113583 + Q113584 + Q113181 + Q112478 + Q112512 + Q112270 + Q111848 + Q111580 + Q111220 + Q110740 + Q109367 + Q108950 + Q109244 + Q108855 + Q108617 + Q108856 + Q108754 + Q108342 + Q108343 + Q107869 + Q107491 + Q106993 + Q106997 + Q106272 + Q106388 + Q106389 + Q106042 + Q105840 + Q105655 + Q104996 + Q103293 + Q102906 + Q102674 + Q102687 + Q102289 + Q102089 + Q101162 + Q101163 + Q101596 + Q100689 + Q100680 + Q100562 + Q99982 + Q100010 + Q99716 + Q99581 + Q99480 + Q98869 + Q98578 + Q98059 + Q98078 + Q98197 + Q96024, data = p3)
RF4 = randomForest( Party ~ Q124742 + Q124122 + Q123464 + Q123621 + Q122769 + Q122770 + Q122771 + Q122120 + Q121699 + Q121700 + Q120978 + Q121011 + Q120379 + Q120650 + Q120472 + Q120194 + Q120012 + Q120014 + Q119334 + Q119851 + Q119650 + Q118892 + Q118117 + Q118232 + Q118233 + Q118237 + Q117186 + Q117193 + Q116797 + Q116881 + Q116953 + Q116601 + Q116441 + Q116448 + Q116197 + Q115602 + Q115777 + Q115610 + Q115611 + Q115899 + Q115390 + Q114961 + Q114748 + Q115195 + Q114517 + Q114386 + Q113992 + Q114152 + Q113583 + Q113584 + Q113181 + Q112478 + Q112512 + Q112270 + Q111848 + Q111580 + Q111220 + Q110740 + Q109367 + Q108950 + Q109244 + Q108855 + Q108617 + Q108856 + Q108754 + Q108342 + Q108343 + Q107869 + Q107491 + Q106993 + Q106997 + Q106272 + Q106388 + Q106389 + Q106042 + Q105840 + Q105655 + Q104996 + Q103293 + Q102906 + Q102674 + Q102687 + Q102289 + Q102089 + Q101162 + Q101163 + Q101596 + Q100689 + Q100680 + Q100562 + Q99982 + Q100010 + Q99716 + Q99581 + Q99480 + Q98869 + Q98578 + Q98059 + Q98078 + Q98197 + Q96024, data = p4)


Tpredict1 = predict(RF1)
Tpredict2 = predict(RF2)
Tpredict3 = predict(RF3)
Tpredict4 = predict(RF4)

table(p1$Party, Tpredict1)
table(p2$Party, Tpredict2)
table(p3$Party, Tpredict3)
table(p4$Party, Tpredict4)


# Predict the clusters for pdata
Ppdata = pdata[,  indcolnames]
for (colname in indcolnames) {
  Ppdata[, colname] = as.numeric(Ppdata[, colname])
}
## install.packages("clue")
library(clue)
KMCpdata = clue::cl_predict(KMC, newdata = Ppdata)

# predict the outcome for the pdata based on above models
pdata1 = subset(pdata, KMCpdata == 1)
pdata2 = subset(pdata, KMCpdata == 2)
pdata3 = subset(pdata, KMCpdata == 3)
pdata4 = subset(pdata, KMCpdata == 4)

predict1 = predict(RF1, newdata = pdata1)
predict2 = predict(RF2, newdata = pdata2)
predict3 = predict(RF3, newdata = pdata3)
predict4 = predict(RF4, newdata = pdata4)

predict1

predictFinal = c(predict1, predict2, predict3, predict4)
predictFinal

PredTestLabels = as.factor(ifelse(predictFinal == 1, "Democrat", "Republican"))


# Write results to file
MySubmission = data.frame(USER_ID = pdata$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "Submission.csv", row.names=FALSE)
