library(class)
library(caret)
library(caretEnsemble)

Amazon_train_data <- read.csv(file = "sentiment_train.csv", header = TRUE)
Amazon_train_data$rating <- as.factor(Amazon_train_data$rating)
Amazon_test_data <- read.csv(file = "sentiment_test.csv", header = TRUE)
Amazon_test_data$rating <- as.factor(Amazon_test_data$rating)

sample_data <- sample(1:nrow(Amazon_train_data), 0.35 * nrow(Amazon_train_data))

control <- trainControl(method="repeatedcv", number=5, repeats=3)
# C5.0
model_c50 <- train(rating ~ negative_score + positive_score + neutral_score + compound_value, 
                   data = Amazon_train_data, method = "C5.0", metric = "Accuracy", trControl = control)
# Gradient Boosting
model_gbm <- train(rating ~ negative_score + positive_score + neutral_score + compound_value, 
                   data = Amazon_train_data, method = "gbm", metric = "Accuracy", trControl = control, verbose = FALSE)
# summarize results
boosting_results <- resamples(list(c5.0 = model_c50, gbm = model_gbm))
summary(boosting_results)
dotplot(boosting_results)
pr.c50 <- predict(model_c50, Amazon_test_data)
mean(Amazon_test_data$rating == pr.c50)
pr.gbm <- predict(model_gbm, Amazon_test_data)
mean(Amazon_test_data$rating == pr.gbm)