
### class library is required to perform knn in R##########
require(class)

##Loading the training data & test data
train_data <- read.csv("optdigits_raining.csv", header = FALSE)
test_data <- read.csv("optdigits_test.csv", header = FALSE)

## Obtaining the class variables of both training set and test data
train_class_variable <- train_data$V65
test_class_variable <- test_data$V65

digit_accuracy <- numeric() 

knnTest <- knn(train_data, test_data, train_class_variable, k = 60,prob = TRUE,l = 20,use.all = FALSE)
accuracy <- mean(knnTest == test_class_variable)
digit_accuracy <- c(digit_accuracy, accuracy)
print(paste("Accuracy at k = ", 1, " is: ", accuracy), quote = F)



for(i in seq(1, 100, 1))
{
        knnTest <- knn(train_data, test_data, train_class_variable, k = i)
        #value <- (sum(test_class_variable == knnTest)/1797)*100
        accuracy <- mean(knnTest == test_class_variable)
        digit_accuracy <- c(digit_accuracy, accuracy)
        print(paste("Accuracy at k = ", i, " is: ", accuracy), quote = F)
}

#plottingnthe required Grpahs

plot(1 - digit_accuracy, type = "l", ylab = "Error Rate", 
     xlab = "K", main = "Error Rate with K values")

plot(digit_accuracy, type = "l", ylab = "Accuracy", 
     xlab = "K", main = "Accuracy with K values")