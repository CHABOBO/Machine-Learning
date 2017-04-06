
##############install the follwinf packages#################################
################install.packages("plyr")####################################
##################install.packages("dplyr")#################################
####################install.packages("stringr")#############################
#####################install.packages("tm")#################################
#####################install.packages("neuralnet")##########################
######################install.packages("nnet")##############################

############# Required Libraries for implementing neural network in amazon dataset#############

library(plyr)
library(dplyr)
library(stringr)
library(tm)
library(neuralnet)
library(nnet)


##Following function is used to find the sentiment score of each review beased on the number of postivie and negative words######
##Postive Words are stored in the file named:positive-words.txt##################################################################
##Negative Words are stored in the file named:negative-words.txt#################################################################
score.sentiment <- function(sentences, pos.words, neg.words, .progress = 'text')
{
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

### Reading the data from the file 
amazon_train_data <- read.csv('amazon_baby_train.csv', header = TRUE)


### Stemming is done on the reviews column inorder to get a better polarity of the reviews##############
train_reviews <- Corpus(VectorSource(amazon_train_data$review))
train_reviews <- tm_map(train_reviews,PlainTextDocument)
train_reviews <- tm_map(train_reviews,removeWords,stopwords (kind = "en"))
train_reviews <- tm_map(train_reviews,stemDocument)
train_reviews <- tm_map(train_reviews, content_transformer(tolower))
data <- sapply(train_reviews, as.character)

positive_words <- scan('positive-words.txt', what='character', comment.char=';')
negative_words <- scan('negative-words.txt', what='character', comment.char=';')

## finding the sentiment score ,number of positive words,number of negative words by calling the score.sentiment function#####

s <- score.sentiment(data, positive_words, negative_words, .progress = 'text')
amazon_train_data$score <- s[,1]
amazon_train_data$pos_count <- s[,3]
amazon_train_data$neg_count <- s[,4]

##### Data is Splitted in order to created the cross validation set and training set#############
##### Splitted data in the ratio 80:20 ###########################################################
set.seed(5)
train <- sample(1:nrow(amazon_train_data), 0.8*nrow(amazon_train_data))
input_trainData <- amazon_train_data[train, ]
input_testData <- amazon_train_data[-train, ]

###########trainning data is added with one column for each class variable(1,2,3,4,5)##################################
trainset  <- cbind(input_trainData[,c(1,2,4,5,6)], class.ind(input_trainData$rating))
colnames(trainset)[6:10] <- c("rating1","rating2","rating3","rating4","rating5")

#######formula is created#####################################################################
################rating1 + rating2 + rating3 + rating4 + rating5 ~ score + pos_count + neg_count
n <- names(trainset)
fmla <- as.formula(paste("rating1+rating2+rating3+rating4+rating5 ~", paste(n[!n %in% c("rating1","rating2","rating3","rating4","rating5","name","review")], collapse = " + ")))

######creating the neuralnetwok model based on the training data################################################
model <- neuralnet(fmla, data = trainset, hidden=2, threshold = 0.01, linear.output = F)
plot(model)

#### validating the created neural network using the cross validation data########################################
testset  <- cbind(input_testData[, c(1,2,4,5,6)], class.ind(input_testData$rating))
colnames(testset)[6:10] <- c("rating1","rating2","rating3","rating4","rating5")
out <- compute(model, testset[,3:5])

####Testing the actual test data on the derived neural network#########################################################
amazontest <- read.csv('amazon_baby_test.csv', header = TRUE)
amazontest$review <- as.factor(amazontest$review)

####Stemming is performed on the data and setiment scores are calculated
test_review<-Corpus(VectorSource(amazontest$review))
test_review<-tm_map(test_review,PlainTextDocument)
test_review<-tm_map(test_review,removeWords,stopwords (kind = "en"))
test_review<-tm_map(test_review,stemDocument)
test_review <- tm_map(test_review, content_transformer(tolower))
test_data<-sapply(test_review, as.character)
s <- score.sentiment(test_data, positive_words, negative_words, .progress = 'text')
amazontest$score <- s[,1]
amazontest$pos_count <- s[,3]
amazontest$neg_count <- s[,4]

###########testing data is added with one column for each class variable(1,2,3,4,5)##################################
test_set <- cbind(amazontest[, c(1,2,4,5,6)], class.ind(amazontest$rating))
colnames(test_set)[6:10] <- c("rating1","rating2","rating3","rating4","rating5")

################################## Applying the neural netwok on actual testing data###############################################
out <- compute(model, test_set[,3:5])

####Folllowing fucntion is used to calucalte the accuracy of the computed data######################################

#maxidx <- function(arr) {
# return(  which(arr == max(arr)))
#}
#idx <- apply(out$net.result, c(1), maxidx)
#prediction <- c("rating1","rating2","rating3","rating4","rating5")[idx]
#table(prediction, input_testData$rating)
#mean(idx - 1 != input_testData$rating)