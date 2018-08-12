# Case Study: Support Vector Machines
# Loading Necessary Packages
library(boot)
library(e1071)
library(gdata)
library(dplyr)
library(caret)
library(ROCR)

# reading the dataset
Cancer_data <- read.csv("E:/Studies/Data_Analytics_Courses/Jigsaw_Courses/Machine_Learning/Support Vector Machines/T3/CaseStudy/Breast_Cancer_Dataset.csv")
str(Cancer_data)
summary(Cancer_data)
head(Cancer_data)
Cancer_data$Class <- as.factor(Cancer_data$Class)

# Data Partitioning


# Exploratory Data Analysis
# Lets plot the variables in pairs
 
pairs(Cancer_data[,-10], col= ifelse(Cancer_data$Class == '2', 'black', 'red' ), pch = 19)

# Missing Value Imputation
# The Variable Bare Nuclei has 16 missing values. 

summary(Cancer_data$Bare_Nuclei)
table(Cancer_data$Bare_Nuclei, Cancer_data$Class)

# Lets impute them by replacing them with the category corresponding to the highest Class
Cancer_data[Cancer_data$Bare_Nuclei == "?" & Cancer_data$Class == "2",6] = 1
Cancer_data[Cancer_data$Bare_Nuclei == "?" & Cancer_data$Class == "4",6] = 10

Cancer_data$Bare_Nuclei <- as.factor(as.character(Cancer_data$Bare_Nuclei))
levels(Cancer_data$Bare_Nuclei)

## set a seed for reproducibility
set.seed(123123)

# Divide The data into training and testing set
trainInd <- sample(x = nrow(Cancer_data), size =  0.7 * nrow(Cancer_data), replace =  FALSE)
train <- Cancer_data[trainInd, ]
test <- Cancer_data[-trainInd, ]


#-----------
cost = c(1e-6, 1e-4, 1e-3,0.01, 0.1, 1, 10, 100, 1e3, 1e4)
gamma <- gamma =  c(1e-4, 1e-3, 1e-2, 0.1, 1)

svm.linear.tune <- tune(svm, Class ~., data = train, kernel='linear',ranges = list(cost = cost))
svm.linear.tune

plot(svm.linear.tune)


final.svm <- svm( Class ~ ., data = train, kernel = 'linear', cost = 1)
summary(final.svm)


## predict on the unseen testing split. 
outOfSample.predictions.svm <- predict(final.svm, newdata = test)


cat('SVM Error')
#confusion matrix. 
confusionMatrix(data = outOfSample.predictions.svm, reference = test$Class, positive= '4')

## the percentage misclassification. 
100 - round(100 * mean(outOfSample.predictions.svm == test$Class), 2)


# Non linear polynomial SVM
svm.polynomial.tune <- tune(svm, Class ~., data = train, kernel='polynomial',ranges = list(cost = cost, gamma = gamma))
svm.polynomial.tune

final.svm2 <- svm( Class ~ ., data = train, kernel = 'polynomial', cost = 1000, gamma = 0.1)
summary(final.svm2)

## predict on the unseen testing split. 
outOfSample.predictions.svm2 <- predict(final.svm2, newdata = test)

cat('SVM Error')
#confusion matrix. 
confusionMatrix(data = outOfSample.predictions.svm2, reference = test$Class, positive= '4')

## the percentage misclassification. 
100 - round(100 * mean(outOfSample.predictions.svm2 == test$Class), 2)