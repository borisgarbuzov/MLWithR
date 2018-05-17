##### Chapter 6: Regression Methods -------------------

wine <- read.csv("data/whitewines.csv")

# examine the wine data
str(wine)

# the distribution of quality ratings
hist(wine$quality)

# summary statistics of the wine data
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

install.packages("Cubist", dependencies = T)
library(Cubist)

wine_train$quality <- as.numeric(wine_train$quality)

modelTree <- cubist(y = wine_train$quality,
                    x = wine_train[ , 1:11],
                    committees = 10)
x <- summary(modelTree)


predQuality <- predict(modelTree, wine_test[, 1:11])

summary(wine_train$quality)
summary(predQuality)




