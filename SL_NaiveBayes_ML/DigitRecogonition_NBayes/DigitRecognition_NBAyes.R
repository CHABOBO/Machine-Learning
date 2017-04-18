## Author: Febin Zachariah
## Implement Naive Bayes algorithm for Optical recognition for handwritten digits dataset###

##We have tested with 3 different libraries to implement naive byes algorithm########

### Loading the training data and test data

train_data <- read.csv("optdigits_raining.csv", header = FALSE)
train_data$V65 <- as.factor(train_data$V65)
test_data <- read.csv("optdigits_test.csv", header = FALSE)
test_data$V65 <- as.factor(test_data$V65)

## Implementation using e1071 library

library(e1071)

model_e1071 <- naiveBayes(train_data[,-65], train_data[,65],laplace = 5)
table(predict(model_e1071, test_data[,-65]), test_data[,65])
pred_e1071 <- predict(model_e1071, test_data, type = "class")
mean(pred_e1071 == test_data$V65)*100


## Implementation using caret library
library(caret)

model_caret<- train(train_data[,-65],train_data$V65,'nb',trControl=trainControl(method='cv',number=10))
table(predict(model_caret, test_data[,-65]), test_data[,65])
pred_caret <- predict(model_caret, test_data, type = "raw")
mean(pred_caret == test_data$V65)*100

## Implementation using naiveBayes library

library(naivebayes)
model_nBayes <- naive_bayes(train_data[,-65], train_data[,65], laplace = 5)
table(predict(model_nBayes, test_data[,-65]), test_data[,65])
pred <- predict(model_nBayes, test_data, type = "class")
mean(pred == test_data$V65)*100

maxidx <- function(arr) {
  return(  which(arr == max(arr)) )
}

model <- naive_bayes(train_data[,-65], train_data[,65], laplace = 1, usekernel = F, prior = NULL)
pred <- predict(model, test_data, type = "class", threshold = 0.2, eps = 0.00000000000000000000001)
idx <- apply(pred, c(1), maxidx)
mean(idx-1 == test_data$V65)


idx <- apply(pred, c(1), maxidx)
confusionMatrix(idx-1, test_data$V65)



