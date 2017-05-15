###Hospital Readmissions###

##Importing the file
library(readr)
Diabetes <- read.csv("C:/Users/welcome/Downloads/10kDiabetes.csv", header = TRUE, strip.white = TRUE, na.strings = c("NA", "?"," ",".")
                     )
View(Diabetes)

##Let's view the structure of the data
str(Diabetes)
#We've got 10,000 rows with 52 variables, we convert the datatypes now. 

##Let's observe the missing values in each variable
sapply(Diabetes, function(x){sum(is.na(x))})

#Top 3 misisng values variables are:
#weight: 9592; payer_code: 5341; medical_specialty: 4100. We shall remove these
Diabetes$weight <- NULL
Diabetes$payer_code <- NULL
Diabetes$medical_specialty <- NULL

dim(Diabetes)#49 variables left!

##Converting datatypes, and subsequent imputation
attach(Diabetes)

#1.rowID is a character, we shall leave it alone

#2.race can be factorised. Let's look at the number of levels
unique(race)
#There are 6 levels, including NA's. We impute the NA's with "Other"
Diabetes$race[is.na(Diabetes$race)] <- "Other"
unique(Diabetes$race) #We see that the NA's have been replaced by 'Other'
#We now convert race into a factor variable
Diabetes$race <- as.factor(Diabetes$race)
class(Diabetes$race) #Job done!

#3.Let's observe gender
summary(Diabetes$gender)
#Only female and male, let's factorise it
Diabetes$gender <- as.factor(Diabetes$gender)
class(Diabetes$gender) #Yep

#4.age is interesting, it is given as intervals. We shall first convert it to character, and then do the replacements
Diabetes$age <- as.character(Diabetes$age)
class(Diabetes$age)

Diabetes$age <- as.numeric(substr(Diabetes$age,2,3)) + 5
#We get 31 NA's, let's replace them with the median
Diabetes$age[is.na(Diabetes$age)] <- median(Diabetes$age, na.rm = TRUE)
sum(is.na(Diabetes$age)) #0 NA's, on with the march

#5.admission_type_id has 7 levels. We shall let it be. No missing values, either

#6.discharge_disposition_id has 22 levels, and it isn't a key variable for predicting readmissions. Let's remove it
Diabetes$discharge_disposition_id <- NULL
dim(Diabetes) #Job done

#7.same with the admission source id
Diabetes$admission_source_id <- NULL

#8.time in hospital is a key variable
class(Diabetes$time_in_hospital)
sum(is.na(time_in_hospital)) #No NA's too, we proceed

#9-14 are numeric, and have no missing values. We let them be

#15-17, diagnoses are nominal variables, and they have a few missing values. We shall impute them first, and encode them
Diabetes$diag_1[is.na(Diabetes$diag_1)] <- Diabetes$diag_1[1]
Diabetes$diag_2[is.na(Diabetes$diag_2)] <- Diabetes$diag_2[1]
Diabetes$diag_3[is.na(Diabetes$diag_3)] <- Diabetes$diag_3[1]

#18.number of diagnoses is important
class(Diabetes$number_diagnoses)
sum(is.na(Diabetes$number_diagnoses))#No missing values either, brilliant

#Henceforth, all are categorical variables, except the last three descriptions. Let's see
View(Diabetes)
sapply(Diabetes[,16:44], class)
#We can see that all are factors, and that too with less than 10 levels. Except readmitted, which is logical
Diabetes$readmitted <- as.factor(Diabetes$readmitted)
#We now encode this as 1 and 0. 1 for readmitted, and 0 for not readmitted
Diabetes$readmitted <- ifelse(Diabetes$readmitted == Diabetes$readmitted[1], 0,1)

#The final 3 variables are charcaters, let's encode them that way
dim(Diabetes)
Diabetes[,45] <- as.character(Diabetes[,45])
Diabetes[,46] <- as.character(Diabetes[,46])
Diabetes[,47] <- as.character(Diabetes[,47])

#Now we are done with the basic cleaning, let's see how many are remaining with NA values
sum(is.na(Diabetes))
sapply(Diabetes, function(x){sum(is.na(x))})

#Only the 3 diagnoses ate to be worked on, we shall encode them according to the ICD codes, later.
#https://en.wikipedia.org/wiki/List_of_ICD-9_codes

#Fucntion to encode them

classify <- function(x) {
  value = -1
  if (startsWith(x, "E"))
  {
    value = 19
  }
  else if (startsWith(x, "V"))
  {
    value = 20
  }
  else
  {
    x = as.numeric(x)
    if (x < 140) {
      value = 1
    }
    else if (x >= 140 && x < 240) {
      value = 2
    }
    else if (x >= 240 && x < 280) {
      value = 3
    }
    else if (x >= 280 && x < 290) {
      value = 4
    }
    else if (x >= 290 && x < 320) {
      value = 5
    }
    else if (x >= 320 && x < 360) {
      value = 6
    }
    else if (x >= 360 && x < 390) {
      value = 7
    }
    else if (x >= 390 && x < 460) {
      value = 8
    }
    else if (x >= 460 && x < 520) {
      value = 9
    }
    else if (x >= 520 && x < 580) {
      value = 10
    }
    else if (x >= 580 && x < 630) {
      value = 11
    }
    else if (x >= 630 && x < 680) {
      value = 12
    }
    else if (x >= 680 && x < 710) {
      value = 13
    }
    else if (x >= 710 && x < 740) {
      value = 14
    }
    else if (x >= 740 && x < 760) {
      value = 15
    }
    else if (x >= 760 && x < 780) {
      value = 16
    }
    else if (x >= 780 && x < 800) {
      value = 17
    }
    else if (x >= 800 && x < 1000) {
      value = 18
    }
  }
  value
}

Diabetes$diag_1 <- as.factor(sapply(as.character(Diabetes$diag_1), classify))
Diabetes$diag_2 <- as.factor(sapply(as.character(Diabetes$diag_2), classify))
Diabetes$diag_3 <- as.factor(sapply(as.character(Diabetes$diag_3), classify))

train <- sample(1:nrow(Diabetes), 0.8*nrow(Diabetes)) # Split the data into 80:20 ratio for cross validation
train_data <- Diabetes[train,] # Training data
test_data <- Diabetes[-train,] # Test data

#Task 1
#Model 1: Generalised Logistic Regression

model.logit <- glm(readmitted~., data=train_data[,-c(1,24,32,34,35,39,40,41,45,46,47)], family=binomial(link='logit'))
model.logit$xlevels[["acarbose"]] <- union(model.logit$xlevels[["acarbose"]], levels(test_data$acarbose))
pred.logit <- predict(model.logit,test_data,type="response")
pred <- ifelse(pred.logit > 0.5, 1, 0)
table(pred.logit,test_data$readmitted)
mean(pred.logit==test_data$readmitted)

#Task 2
# Model 2: Linear regression
model.lm.steps <- step(lm(time_in_hospital~., data=train_data[,-c(1,24,32,34,35,39,40,41,45,46,47)]), direction = "both")
model.lm <- lm(time_in_hospital ~ race + age + admission_type_id + num_lab_procedures + 
                       num_procedures + num_medications + number_outpatient + number_emergency + 
                       number_inpatient + diag_1 + diag_2 + diag_3 + number_diagnoses + 
                       max_glu_serum + A1Cresult + metformin + nateglinide + glipizide + 
                       glyburide + pioglitazone + rosiglitazone + acarbose + insulin + 
                       glipizide.metformin, data=train_data[,-c(1,24,32,34,35,39,40,41,45,46,47)])
model.lm$xlevels[["acarbose"]] <- union(model.lm$xlevels[["acarbose"]], levels(test_data$acarbose))
pred.lm <- predict(model.lm,test_data)
rmse <- function(error)
{
        sqrt(mean(error^2))
}
rmse(test_data$time_in_hospital-pred.lm)