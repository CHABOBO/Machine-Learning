
## Implemntation of SVM ALgorithm in Digit Dataset#########

##Install the e1071 package if it is not installed######

#install.packages("e1071")

#load e1071 library
library(e1071)

#Load the trining and test dataset.

train_data <- read.csv("optdigits_raining.csv", header = FALSE)
test_data <- read.csv("optdigits_test.csv", header = FALSE)


##Create the SVM model by using the trianing data.##
#model <- svm(V65~., data = train_data, kernel = "linear", type = "C-classification", cross = 10 )

model<-svm(V65~., data = train_data, kernel = "linear", type = "C-classification", 
    cross = 10, cost = 0.0001, gamma = 1000)


#model$tot.accuracy

##using the above created model predict the output class for test data

prediction <- predict(model, test_data)

##calculate the accuracy
mean(prediction == test_data$V65)*100

plot(model, train_data, V53 ~ V6)

