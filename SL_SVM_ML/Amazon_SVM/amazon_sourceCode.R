## Implemntation of SVM ALgorithm in Amazon Dataset#########

##Install the e1071 package if it is not installed######

#install.packages("e1071")

#load e1071 library
library(e1071)

#Load the trining and test dataset.
train_data = read.csv(file="sentiment_train.csv",header = TRUE)
test_data = read.csv(file="sentiment_test.csv",header = TRUE)

sample_data = sample(1:nrow(train_data), 0.15*nrow(train_data))


training_data <- train_data[sample_data, ]

##Create the SVM model by using the trianing data.##

model <- svm(rating~negative_score+positive_score+neutral_score+compound_value, 
                 data = training_data, kernel = "linear", type = "C-classification", 
                 cross = 10, cost = 0.01, gamma = 100)
model$tot.accuracy

##using the above created model predict the output class for test data

pred_test <- predict(model, test_data)

##calculate the accuracy
mean(pred_test == test_data$rating)*100

plot(model, train_data, positive_score ~ compound_value)
qplot(negative_score, compound_value, colour = rating, data = test_data, shape = pred_test)
qplot(positive_score, compound_value, colour = rating, data = test_data, shape = pred_test)