##############################

####Author:Febin zachariah####
####Date:01/29/2017###########

#######################################
## packages needed for implementation:#
## install.packages("rpart")###########
## install.packages("rattle")##########
## install.packages("rpart.plot")######
## install.packages("RColorBrewer")####
#######################################

######Importing the libraries used#####

library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("party")
library("partykit")
library("caret")

#######Loading the required File for creating the decision tree######### 
train_data <- read.csv("optdigits_raining.csv", header = FALSE)
train_data <- train_data[, c(65, 1:64)]

###Creating the formula for showing the dependeny of input attributes to output attribute#####
##############################################################################################
#V65 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + 
##V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + 
##  V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + 
##  V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + 
##  V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + 
##  V52 + V53 + V54 + V55 + V56 + V57 + V58 + V59 + V60 + V61 + 
##  V62 + V63 + V64###########################################

xnam <- paste(colnames(train_data[2:65]), sep="", collapse = "+")
fmla <- as.formula(paste(paste(colnames(train_data[1]), " ~ ", sep = ""), xnam))

##Splitting the data training data into sets,one containg 80% rowns and another contianing 20%#####

train <- sample(1:nrow(train_data), 0.5*nrow(train_data))
input_trainData <- train_data[train, ]
input_testData <- train_data[-train, ]

## Decision Tree creation using rpart package without pruning by using information gain.##############################
rpartMod <- rpart(fmla, data = input_trainData, method = "class",parms = list(split = 'information'), minsplit = 2, minbucket = 1)
plot(rpartMod)
text(rpartMod, pretty = 400)

##Tree has been pruned by using the prune method in rpart package##########################################
treePruned_inputtrain <- prune(rpartMod,cp = -1)
plot(treePruned_inputtrain)
text(treePruned_inputtrain, pretty = 400)

#####################################################################################################
######Running the decision tree obtianed on the cross-validation data(remaining 20% of the data)#####
######Output is stored on the file  file = "input_test_result.csv"###################################

out <- predict(treePruned_inputtrain, newdata = input_testData, type="class") #Returns the prediction
input_testData$Output <- out
input_testData <- input_testData[, c(66, 1:65)]
write.csv(input_testData, file = "input_test_result.csv")


##############Now Testing the Data on the actual test data###########################################
################Output is stored on the file test_result.csv#########################################
test_data <- read.csv("optdigits_test.csv", header = FALSE)
test_data <- test_data[, c(65, 1:64)]
outtest <- predict(treePruned_inputtrain, newdata = test_data, type="class") #Returns the predicted class
test_data$Output <- outtest
test_data <- test_data[, c(66, 1:65)]
write.csv(test_data, file = "final_test_result.csv")
pred_test.response <- colnames(outtest)[max.col(outtest, ties.method = c("random"))]


###################################End of the Program#########################################################################
