
#Loading the required libraries
install.packages("C50")
require(class)
require(caret)
require(caretEnsemble)
require(C50)

train_data = read.csv(file="optdigits_raining.csv",header = FALSE)
train_data$V65 = as.factor(train_data$V65)

test_data = read.csv(file="optdigits_test.csv",header = FALSE)
test_data_target = test_data$V65

control <- trainControl(method="repeatedcv", number=5, repeats=3,)
# C5.0
model_c50 <- train(V65~., data=train_data, method="C5.0", metric="Accuracy", trControl=control)
# Gradient Boosting
model_gbm <- train(V65~., data=train_data, method="gbm", metric="Accuracy", trControl=control, verbose=FALSE)

# summarize results
boosting_results <- resamples(list(c5.0=model_c50, gbm=model_gbm))
summary(boosting_results)
dotplot(boosting_results)

predict.c50 <- predict(model_c50, test_data)
mean(test_data_target == predict.c50)

predict.gbm <- predict(model_gbm, test_data)
mean(test_data_target == predict.gbm)

#modelCor(boosting_results)
splom(boosting_results)