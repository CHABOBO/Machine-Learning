
####### Following Packages are required for performing the neural netwrok algorithm########

##########################################
########install.packages("neuralnet")#####
#######install.packages("nnet")##########
#########################################

############## Loading the required packages###########

library(nnet)
library(neuralnet)

########## Load the training data from the required files
digit_training_data = read.csv(file="optdigits_raining.csv",header = FALSE)

##### Data is Splitted in order to created the cross validation set and training set#############
##### Splitted data in the ratio 80:20 ###########################################################

set.seed(5)
train_Data = sample(1:nrow(digit_training_data), 0.8*nrow(digit_training_data))

#### trianing and cross-validation  dataset is created############################################# 
training_data = digit_training_data[train_Data, ]
testing_data = digit_training_data[-train_Data, ]

###########trainning data is added with one column for each class variable(0,1,2,3,4,5,6,7,8,9)##################################
trainset  <- cbind(training_data[, 1:64], class.ind(training_data$V65))
colnames(trainset)[65:74] <- c("output0","output1","output2","output3","output4","output5","output6","output7","output8","output9")

#############Creating the formula which needs to used in creating the neural network################################
############################output0 + output1 + output2 + output3 + output4 + output5 + output6 + ##################
############################output7 + output8 + output9 ~ V1 + V2 + V3 + V4 + V5 + V6 + ###########################
############################V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 +    ###########################
###########################V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 +  ###########################
###########################V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 +  ###########################
############################ V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + #########################
##########################V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 +   ###########################
##########################  V57 + V58 + V59 + V60 + V61 + V62 + V63 + V64###########################################

n <- names(trainset)
fmla <- as.formula(paste("output0+output1+output2+output3+output4+output5+output6+output7+output8+output9 ~", paste(n[!n %in% c("output0","output1","output2","output3","output4","output5","output6","output7","output8","output9")], collapse = " + ")))

######creating the neuralnetwok model based on the training data################################################
nn_model <- neuralnet(fmla, data = trainset, hidden=10, threshold = 0.01, linear.output = F)
output = nn_model$net.result
plot(nn_model)

#### validating the created neural network using the cross validation data########################################
testset  <- cbind(testing_data[, 1:64], class.ind(testing_data$V65))
colnames(testset)[65:74] <- c("output0","output1","output2","output3","output4","output5","output6","output7","output8","output9")
out = compute(nn_model, testset[,1:64])

####Folllowing fucntion is used to calucalte the accuracy of the computed data######################################

##maxidx <- function(arr) {
  ##return(  which(arr == max(arr)) )
###}
#idx <- apply(out$net.result, c(1), maxidx)
#prediction <- c("output0","output1","output2","output3","output4","output5","output6","output7","output8","output9")[idx]
#table(prediction, testing_data$V65)
#mean(idx - 1 != testing_data$V65)

####Testing the actual test data on the derived neural network#########################################################
testdata <- read.csv(file="optdigits_test.csv",header = FALSE)
test_set  <- cbind(testdata[, 1:64], class.ind(testdata$V65))

###########testing data is added with one column for each class variable(0,1,2,3,4,5,6,7,8,9)##################################
colnames(test_set)[65:74] <- c("output0","output1","output2","output3","output4","output5","output6","output7","output8","output9")
n <- names(test_set)

################################## Applying the neural netwok on actual testing data###############################################
outtest = compute(nn_model, test_set[,1:64])