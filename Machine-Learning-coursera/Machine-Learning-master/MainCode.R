library(naivebayes)
library(e1071)
library(randomForest)
library(VIM)
library(neuralnet)
library(nnet)
library(psych)
library(FactoMineR)
library(factoextra)
library(doParallel)

maxidx <- function(arr) {
        return(  which(arr == max(arr)) )
}

classify <- function(x) {
        value = ""
        if (is.na(x))
        {
                value = x
        }
        else
        {
                x = as.numeric(x)
                if (x >= 0 && x < 10) {
                        value = "[0-10)"
                }
                else if (x >= 10 && x < 20) {
                        value = "[10-20)"
                }
                else if (x >= 20 && x < 30) {
                        value = "[20-30)"
                }
                else if (x >= 30 && x < 40) {
                        value = "[30-40)"
                }
                else if (x >= 40 && x < 50) {
                        value = "[40-50)"
                }
                else if (x >= 50 && x < 60) {
                        value = "[50-60)"
                }
                else if (x >= 60 && x < 70) {
                        value = "[60-70)"
                }
                else if (x >= 70 && x < 80) {
                        value = "[70-80)"
                }
                else if (x >= 80 && x < 90) {
                        value = "[80-90)"
                }
                else if (x >= 90 && x < 100) {
                        value = "[90-100)"
                }
        }
        value
}

df <- read.csv("database.csv", header = T, na.strings = c("", '998'))
summary(df)

df$Agency.Code <- factor(df$Agency.Code, labels = seq(1:12003), levels = unique(df$Agency.Code))
df$Agency.Name <- factor(df$Agency.Name, labels = seq(1:9216), levels = unique(df$Agency.Name))
df$Agency.Type <- factor(df$Agency.Type, labels = seq(1:7), levels = unique(df$Agency.Type))
df$City <- factor(df$City, labels = seq(1:1782), levels = unique(df$City))
df$State <- factor(df$State, labels = seq(1:51), levels = unique(df$State))
df$Month <- factor(df$Month, labels = seq(1:12), levels = unique(df$Month))
df$Crime.Type <- factor(df$Crime.Type, labels = seq(1:2), levels = unique(df$Crime.Type))
df$Crime.Solved <- factor(df$Crime.Solved, labels = seq(1:2), levels = unique(df$Crime.Solved))
df$Victim.Sex <- factor(df$Victim.Sex, labels = seq(1:3), levels = unique(df$Victim.Sex))
df$Victim.Race <- factor(df$Victim.Race, labels = seq(1:5), levels = unique(df$Victim.Race))
df$Victim.Ethnicity <- factor(df$Victim.Ethnicity, labels = seq(1:3), levels = unique(df$Victim.Ethnicity))
df$Perpetrator.Sex <- factor(df$Perpetrator.Sex, labels = seq(1:3), levels = unique(df$Perpetrator.Sex))
df$Perpetrator.Race <- factor(df$Perpetrator.Race, labels = seq(1:5), levels = unique(df$Perpetrator.Race))
df$Perpetrator.Ethnicity <- factor(df$Perpetrator.Ethnicity, labels = seq(1:3), levels = unique(df$Perpetrator.Ethnicity))
df$Relationship <- factor(df$Relationship, labels = seq(1:28), levels = unique(df$Relationship))
df$Weapon <- factor(df$Weapon, labels = seq(1:16), levels = unique(df$Weapon))
df$Record.Source <- factor(df$Record.Source, labels = seq(1:2), levels = unique(df$Record.Source))
df$Victim.Age <- sapply(df$Victim.Age, classify)
df$Victim.Age <- factor(df$Victim.Age, labels = seq(1:10), levels = unique(df$Victim.Age))
df$Perpetrator.Age <- sapply(df$Perpetrator.Age, classify)
df$Perpetrator.Age <- factor(df$Perpetrator.Age, labels = seq(1:10), levels = unique(df$Perpetrator.Age))
df$Perpetrator.Sex <- as.factor(df$Perpetrator.Sex)
df$Perpetrator.Age <- as.factor(df$Perpetrator.Age)

impute.df <- kNN(df)
df <- impute.df[,1:24]

part <- sample(1:nrow(df), 0.3*nrow(df))
part.df <- df[part,]

train <- sample(1:nrow(part.df), 0.8*nrow(part.df))
train_data <- part.df[train,]
test_data <- part.df[-train,]

forpca <- sapply(df, as.numeric)
pca_3 <- PCA(forpca[,-c(1)])
#The amount of variation retained by each PC is called eigenvalues. 
#The first PC corresponds to the direction with the maximum amount of variation in the data set.
eigenval <- pca_3$eig 

#Visualize PCA
fviz_screeplot(pca_3, ncp=23)
fviz_pca_contrib(pca_3,choice="var",axes=1:5, color = "lightgreen", fill = "lightgreen")

## Naive Bayes
model.nb.sex <- naive_bayes(train_data[,c(2,3,5,6,11,14,18,17,20)], train_data$Perpetrator.Sex)
pred.nb.sex <- predict(model.nb.sex, test_data, type = "prob")
idx.nb.sex <- apply(pred.nb.sex, c(1), maxidx)
mean(idx.nb.sex == test_data$Perpetrator.Sex)

model.nb.age <- naive_bayes(train_data[,c(2,3,5,6,11,14,18,16,20)], train_data$Perpetrator.Age)
pred.nb.age <- predict(model.nb.age, test_data, type = "prob")
idx.nb.age <- apply(pred.nb.age, c(1), maxidx)
mean(idx.nb.age == test_data$Perpetrator.Age)
## Naive Bayes

## Not working
model.svm.sex <- svm(Perpetrator.Sex~., data = train_data[,c(2,3,5,6,11,14,16,18,17,20)], kernel = "linear", type = "C-classification")
pred.svm.sex <- predict(model.nb.sex, test_data, type = "prob")
idx.nb.sex <- apply(pred.nb.sex, c(1), maxidx)
mean(idx.nb.sex == test_data$Perpetrator.Sex)
## Not working

## Neural Networks
testset.p.sex  <- cbind(test_data[, c(2,3,5,6,11,14,17,18,20)], class.ind(test_data$Perpetrator.Sex))
colnames(testset.p.sex)[10:12] <- c("P.Sex_1", "P.Sex_2", "P.Sex_3")
trainset.p.sex  <- cbind(train_data[, c(2,3,5,6,11,14,17,18,20)], class.ind(train_data$Perpetrator.Sex))
colnames(trainset.p.sex)[10:12] <- c("P.Sex_1", "P.Sex_2", "P.Sex_3")
n <- names(trainset.p.sex[,-c(10,11,12)])
f <- as.formula(paste("P.Sex_1 + P.Sex_2 + P.Sex_3 ~", paste(n, collapse = " + ")))
trainset.p.sex <- sapply(trainset.p.sex, as.numeric)
testset.p.sex <- sapply(testset.p.sex, as.numeric)
clus<-makeCluster(spec=8,type="PSOCK")
registerDoParallel(clus)
model.nn.sex <- neuralnet(f, data = trainset.p.sex, hidden=10)
stopCluster(clus)
plot(model.nn.sex)
pred.nn.sex <- compute(model.nn.sex, testset.p.sex[,-c(11,12,13)])
idx.nn.sex <- apply(pred.nn.sex$net.result, c(1), maxidx)
mean(idx.nn.sex == test_data$Perpetrator.Sex)

testset.p.age <- cbind(test_data[, -c(1,2,3,4,17)], class.ind(test_data$Perpetrator.Age))
colnames(testset.p.age)[20:29] <- c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6", "P.7", "P.8", "P.9", "P.10")
trainset.p.age  <- cbind(train_data[, -c(1,2,3,4,17)], class.ind(train_data$Perpetrator.Age))
colnames(trainset.p.age)[20:29] <- c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6", "P.7", "P.8", "P.9", "P.10")
n <- names(trainset.p.age[,-c(20:29)])
f <- as.formula(paste("P.1+P.2+P.3+P.4+P.5+P.6+P.7+P.8+P.9+P.10 ~", paste(n, collapse = " + ")))
trainset.p.age <- sapply(trainset.p.age, as.numeric)
testset.p.age <- sapply(testset.p.age, as.numeric)
model.nn.age <- neuralnet(f, data = trainset.p.age[sample(1:nrow(trainset.p.age), 0.3*nrow(trainset.p.age)),], hidden=10, threshold = 0.01, linear.output = F)
plot(model.nn.age)
pred.nn.age <- compute(model.nn.age, testset.p.age[,-c(20:29)])
idx.nn.age <- apply(pred.nn.age$net.result, c(1), maxidx)
mean(idx.nn.age == test_data$Perpetrator.Age)
## Neural Networks

## Generalised Logistic Regression
model.logit.sex <- glm(Perpetrator.Sex~., data=train_data[,-c(1,2,3,4)], family=binomial(link='logit'))
