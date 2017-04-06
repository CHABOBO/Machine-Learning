##############################

####Author:Febin Zachariah####
####Date:02/03/2017###########

#######################################
## packages needed for implementation:#
## install.packages("rpart")###########
## install.packages("plyr")##########
## install.packages("stringr")######
#######################################

######Importing the libraries used#####

library("rpart")
library("plyr")
library("stringr")

##Following function is used to find the sentiment score of each review beased on the number of postivie and negative words######
##Postive Words are stored in the file named:positive-words.txt##################################################################
##Negative Words are stored in the file named:negative-words.txt#################################################################

score.sentiment <- function(sentences, pos.words, neg.words, .progress = 'none')
{
        require(plyr)
        require(stringr)
        scores <- laply(sentences, function(sentence, pos.words, neg.words){
        sentence <- gsub('[[:punct:]]', "", sentence)
        sentence <- gsub('[[:cntrl:]]', "", sentence)
        sentence <- gsub('\\d+', "", sentence)
        sentence <- tolower(sentence)
        word.list <- str_split(sentence, '\\s+')
        words <- unlist(word.list)
        pos.matches <- match(words, pos.words)
        neg.matches <- match(words, neg.words)
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)
        score <- sum(pos.matches) - sum(neg.matches)
        return(c(score, sum(pos.matches), sum(neg.matches)))
  }, pos.words, neg.words, .progress = .progress)
  scores.df <- data.frame(score = scores[,1], text = sentences, pos_count = scores[,2], neg_count = scores[,3])
  return(scores.df)
}

##Reading the positive and negative words from textfiles#######################
positive_words <- scan('positive-words.txt', what='character', comment.char=';')
negative_words <- scan('negative-words.txt', what='character', comment.char=';')

##Reading the training Data#########################################################
amazon_train_dataset <- read.csv('amazon_baby_train.csv', header = TRUE)
amazon_train_dataset$review <- as.factor(amazon_train_dataset$review)
##finding the sentiment score ,number of positive words,number of negative words by calling the score.sentiment function#####
scores <- score.sentiment(amazon_train_dataset$review, positive_words, negative_words, .progress='text')
amazon_train_dataset$score <- scores[,1]
amazon_train_dataset$pos_count <- scores[,3]
amazon_train_dataset$neg_count <- scores[,4]

##Splitting the data to get cross validation dataset#####################################################################
train <- sample(1:nrow(amazon_train_dataset), 0.8*nrow(amazon_train_dataset))
input_trainData <- amazon_train_dataset[train, ]
input_testData <- amazon_train_dataset[-train, ]

####creating the decision tree by using rpart library
rpartMod <- rpart(rating ~ score + pos_count + neg_count, data = input_trainData, method = "class",parms = list(split = 'information'), minsplit = 2, minbucket = 1)
treePruned_inputtrain <- prune(rpartMod,cp = -1)

##Running the decision tree on the cross-validation data
out <- predict(treePruned_inputtrain, newdata = input_testData, type="class")
input_testData$Output <- out
write.csv(input_testData, file = "amazon_traintestdata_result.csv")


##Running the decision tree learning on the actual dataset
amazon_test_data <- read.csv("amazon_baby_test.csv", header = TRUE)
amazon_test_data$review <- as.factor(amazon_test_data$review)
test_scores <- score.sentiment(amazon_test_data$review, positive_words, negative_words, .progress='text')
amazon_test_data$score <- test_scores[,1]
amazon_test_data$pos_count <- test_scores[,3]
amazon_test_data$neg_count <- test_scores[,4]
outtest <- predict(treePruned_inputtrain, newdata = amazon_test_data, type="class")
amazon_test_data$Output <- outtest
write.csv(amazon_test_data, file = "amazon_testdata_result.csv")