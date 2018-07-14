setwd("C:/Users/ravan_000/OneDrive/Learn/Stats/TheAnalyticsEdge/unit7")

WHO = read.csv("WHO.csv")

plot(WHO$GNI, WHO$FertilityRate)

library(ggplot2)

# Create plot
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate) )
scatterplot + geom_point(color = "blue", size = 2, shape = 17)
# Save plot
fertilityGNIPlot = scatterplot + geom_point(color = "darkred", size = 2, shape = 8) + ggtitle("Fertility Rate vs. GNI")
fertilityGNIPlot
# Print plot
pdf("FvsGNI.pdf")
print(fertilityGNIPlot)
dev.off()

ggplot(WHO, aes(x = GNI, y = FertilityRate, color=Region) ) + geom_point()


ggplot(WHO, aes(x = GNI, y = FertilityRate, color=LifeExpectancy) ) + geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15) ) + geom_point()

ggplot(WHO, aes(x = log(FertilityRate), y = Under15) ) + geom_point()

model = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model)

ggplot(WHO, aes(x = log(FertilityRate), y = Under15) ) + geom_point() + stat_smooth(method = "lm")
ggplot(WHO, aes(x = log(FertilityRate), y = Under15) ) + geom_point() + stat_smooth(method = "lm", level = 0.99)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15) ) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15) ) + geom_point() + stat_smooth(method = "lm", se = FALSE, color = "orange")


ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette="Dark2")



