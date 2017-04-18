
##Loading the required library
library(class)

#getting the train and test data for calculated sentimet score
train_data = read.csv(file="sentiment_train.csv",header = TRUE)
test_data = read.csv(file="sentiment_test.csv",header = TRUE)


sample_data = sample(1:nrow(train_data), 0.25*nrow(train_data))

##Extracting the features and target values of both training data and testing data

training_features <- train_data[sample_data, 3:6]
training_def <- train_data[sample_data, 2]

testing_features <- test_data[, 3:6] 
testing_def <- test_data[, 2]

accuracy <- numeric()
#Performing the knn operation 

for (i in 1:3)
{
        #Generating the model and output for the testing data
        knnTest <- knn(train = training_features, test = testing_features, cl = training_def[], k = 1,prob = FALSE,l=1,use.all = TRUE);
        value <- (sum(testing_def == knnTest) / 36707) * 100
        accuracy <- c(accuracy, mean(knnTest == testing_target))
        
        accuracy <- mean(knnTest == test_class_variable)
        digit_accuracy <- c(digit_accuracy, accuracy)
        print(paste("Accuracy at k = ", i, " is: ", accuracy), quote = F)
      
}

#plotting the required Graphs
plot(accuracy,type="l",ylab="Accuracy",
     xlab="K",main="Accuracy for Amazon data With Varying K")

plot(1-accuracy,type="l",ylab="Error Rate",
     xlab="K",main="Error Rate for Amazon data With Varying K")