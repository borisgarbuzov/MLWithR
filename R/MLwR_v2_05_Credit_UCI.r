# install.packages("rpart")
# install.packages("rpart.plot")
pkgs <- c("party", "randomForest", "e1071")
install.packages(pkgs, depend = TRUE)

library(party)
library(randomForest)
library(e1071)

creditcard_data <- read.csv("data/UCI_Credit_Card.csv")
View(creditcard_data)

creditcard_data$default.payment.next.month <- as.factor(as.character(creditcard_data$default.payment.next.month))

set.seed(1234)

training_data_size <- floor(0.75 * nrow(creditcard_data))
training_data_size
creditcard_train_index <- sample(1:nrow(creditcard_data), training_data_size)
#Training data
creditcard_train <- creditcard_data[creditcard_train_index, ]
# head(creditcard_train)
#Testing data
creditcard_test <- creditcard_data[-creditcard_train_index, ]
# head(creditcard_test)

# Decision Tree

library(rpart)

# grow tree

fit <-  rpart(default.payment.next.month ~ ., data = creditcard_train, method = "class")
print(fit)
summary(fit)

fit$cptable
printcp(fit)
plotcp(fit)

fit.pruned <- prune(fit, cp = 0.01)

library(rpart.plot)

prp(fit.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
fit.pred <- predict(fit.pruned, creditcard_test, type="class")
fit.perf <- table(creditcard_test$default.payment.next.month, fit.pred,
                  dnn=c("Actual", "Predicted"))
fit.perf

(5539 + 580)/(5539 + 580 + 1144 + 237)



# % prediction accuracy = 0.8158667
# The most important variable on the basis of which customers are
# classified into defaulters and non-defaulters is PAY_0.

# Random Forest

library(randomForest)
set.seed(1234)

# Grows the forest

fit.forest <- randomForest(default.payment.next.month~., data = creditcard_train, na.action=na.roughfix,
                           importance=TRUE)
fit.forest

# Determine variable importance

importance(fit.forest, type=2)

# Classify new cases

forest.pred <- predict(fit.forest, creditcard_test)
forest.perf <- table(creditcard_test$default.payment.next.month, forest.pred,
                     dnn=c("Actual", "Predicted"))
forest.perf

(forest.perf[1] + forest.perf[4])/ (forest.perf[1] + forest.perf[2] + forest.perf[3] + forest.perf[4])

# prediction accuracy is 0.8129333
