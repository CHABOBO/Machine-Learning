
# Naive Bayes algorithm implementation for amazon baby product review dataset

###loading the required libraries

library(lattice)
library(ggplot2)
library(naivebayes)

library(caret)

## Loading training and test set
train_data = read.csv(file="sentiment_train.csv",header = TRUE)
train_data$rating = as.factor(train_data$rating)
test_data = read.csv(file="sentiment_test.csv",header = TRUE)
test_data$rating = as.factor(test_data$rating)

### Creating the model
model <- naive_bayes(rating~negative_score+positive_score+neutral_score+compound_value, data = train_data, 
                    laplace = 5, usekernel = F, prior = NULL)
###Prediction performed on test data

pred <- predict(model, test_data, type = "class", threshold = 0.0001, eps = 0.00000000000000000000001)
mean(pred == test_data$rating)*100


confusionMatrix(pred, test_data$rating)
