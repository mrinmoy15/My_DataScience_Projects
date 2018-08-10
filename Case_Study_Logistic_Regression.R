## Loading the required packages
loadpackages<- function(x){
  require(x,character.only = TRUE)
}

pckgs<-c("gains","dplyr","irr","caret","ggplot2","ROCR")
for(i in 1: length(pckgs)){
  loadpackages(pckgs[[i]])
}

## Reading the dataset
goodforu<-read.csv("C:/Jig12673/GradedAssignments/T10/Files/goodforu-class12.csv")
head(goodforu)
str(goodforu)
summary(goodforu)
dim(goodforu)
colnames(goodforu)


#### --------------------------------------------------------------------------------------------------------
## EDA: 
## No missing values 
## No outliers


## creating the desired dataframe:

goodforu_for_model<-goodforu[,c("X2","X23","X9","X16","X30")]

## Converting the dependent variable X23 to a binary variable, named as Target:
goodforu_for_model$Target=ifelse(goodforu_for_model$X23>5,1,0)
goodforu_for_model <- goodforu_for_model[,-2]
summary(goodforu_for_model)


## Model Building
## Spliting the data into training and testing set
set.seed(200)
index<-sample(nrow(goodforu_for_model),0.70*nrow(goodforu_for_model), replace = FALSE)
training<-goodforu_for_model[index,]
testing<-goodforu_for_model[-index,]


log_model<-glm(Target~.,data = training,  family = "binomial")
summary(log_model)
coefficients(log_model)



## Prediction using the model
p<-predict(log_model,type = "response", newdata = testing)
table(goodforu_for_model$Target)/nrow(goodforu_for_model)

## considering a threshold of 0.7461
pred<-ifelse(p >= 0.7461, 1,0)

## Confusion matrix
confusionMatrix(pred,testing$Target,positive = "1")


## gain chart
gains(testing$Target,predict(log_model, type = "response", newdata = testing, groups = 10))

## ROC Curve:
pr<-prediction(p,testing$Target)
prf<-performance(pr,measure = "tpr", x.measure= "fpr")
plot(prf, col="red", lty=3,lwd=3)
title("ROC Curve")
abline(0,1)


# AUC:
auc<-performance(pr,measure = "auc")
auc<-auc@y.values[[1]]
auc
