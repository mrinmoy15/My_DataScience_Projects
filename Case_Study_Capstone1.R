# Capstone Project1: Predicting likelihood of heart disease occurance

# Loading Necesary packages
library(dplyr)
library(caret)
library(gains)
library(irr)
library(ggplot2)
library(ROCR)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

# Reading the dataset
HD<-read.csv("E:/Studies/Data_Analytics_Courses/Jigsaw_Courses/R_dataScience/T10/CaseStudy/CaseStudyExample/heart_disease_data.csv")
str(HD)
summary(HD)
HD$Sex<-as.factor(HD$Sex)
HD$cp<-as.factor(HD$cp)
HD$fbs<-as.factor(HD$fbs)
HD$restecg<-as.factor(HD$restecg)
HD$exang<-as.factor(HD$exang)
HD$slope<-as.factor(HD$slope)
HD$DV<-as.factor(HD$DV)


## Exploratory Data Analysis
## Understanding univariate analysis
## Histogram
HD<-read.csv("C:/Users/Mrinmoy/Studies/Jigsaw_Courses/R_dataScience/T6/T6.2/CaseStudy/CaseStudyExample/case_study_heart_disease_data_set.csv",header = T)
head(HD,5)
str(HD)
ggplot(HD,aes(x=trestbps))+geom_histogram(aes(fill=as.factor(DV)),position = "dodge")+facet_grid(Sex+thal~cp+exang)

ggplot(HD,aes(x=chol))+geom_histogram(aes(fill=as.factor(DV)),position = "dodge")+facet_grid(Sex+thal~cp+exang)

ggplot(HD,aes(x=thalach))+geom_histogram(aes(fill=as.factor(DV)),position = "dodge")+facet_grid(Sex+thal~cp+exang)

ggplot(HD,aes(x=oldpeak))+geom_histogram(aes(fill=as.factor(DV)),position = "dodge")+facet_grid(Sex+thal~cp+exang)

## Boxplot
##  Relation of: Resting blood pressure across Gender and chest pain type
ggplot(HD,aes(y=trestbps,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)

## Relation of :ST depression induced by exercise relative to rest across Gender and chest pain type.
ggplot(HD,aes(y=oldpeak,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)

## Relation of maximum heart rate achieved across Gender and Chest pain
ggplot(HD,aes(y=thalach,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)

## Relation of cholestoral level across Gender and Chest pain
ggplot(HD,aes(y=chol,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)

## Bivariate analysis
# Understanding the relationship between age and the maximum heart rate
p5<-ggplot(HD,aes(x=Age,y=thalach,colour=as.factor(DV)))
p5+geom_point(lwd=2)+facet_grid(.~DV)

p5+geom_point()+facet_grid(thal~Sex+fbs)

# Understanding the relation between age and cholestaral level
p6<-ggplot(HD,aes(x=Age,y=chol,colour=as.factor(DV)))
p6+geom_point(lwd=1)+facet_grid(.~DV)


# Model Building:

# Logistic Regression:
# Create the training and testing dataset
index<-sample(nrow(HD),0.7*nrow(HD),replace = F)
train<-HD[index,]
test<-HD[-index,]

modal1<-glm(formula =as.factor(DV)~Sex+Age+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+cp+slope+ca+thal,data = train,family="binomial")
summary(modal1)

## Prediction with test dataset
p<-predict(modal1,type = "response",newdata = test)
HD$DV=as.factor(HD$DV)
table(HD$DV)/nrow(HD)

pred<-ifelse(p>=0.4587459,1,0)

## ROCR Curve
table(test$DV,pred)
kappa2(data.frame(test$DV,pred))
confusionMatrix(test$DV,pred,positive = "1")

pr<-prediction(p,test$DV)
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)

auc<-performance(pr, measure = "auc")
auc<-auc@y.values[[1]]
## OR
auc<-performance(pr, measure = "auc")
auc<-unlist(slot(auc,"y.values"))
auc
summary(modal1)
mod2<-glm(data=train,formula = as.factor(DV)~Sex+trestbps+cp+slope,family = "binomial")
summary(mod2)



## Model Buliding: Decision Tree
#dummy variables
HD$ca2<-ifelse(HD$ca==2,1,0)
HD$cp4<-ifelse(HD$cp==4,1,0)
HD$slope2<-ifelse(HD$slope==2,1,0)

set.seed(100)
sampling<-sort(sample(nrow(HD), nrow(HD)*.7))

length(sampling)
#Select training sample
train<-HD[sampling,]
test<-HD[-sampling,]

#method=annova ;regression models

#new one
fit<-rpart(DV ~ Sex+thalach+trestbps+
             cp4+slope2+ca2,
           method="class", 
           data=HD,
           control=rpart.control(cp=0.01, minsplit=2),
           parms=list(split="gini"))

printcp(fit)
plotcp(fit)

fit<-rpart(as.factor(DV) ~ Sex+thalach+trestbps+
             cp4+slope2+ca2,
           method="class", 
           data=HD, 
           control=rpart.control(cp=0.02, minsplit=2),
           parms=list(split="gini"))

# The complexity parameter (cp) is used to control the size of the decision tree and to select 
# the optimal tree size. If the cost of adding another variable to the decision tree 
# from the current node is above the value of cp, then tree building does not continue.
# We could also say that tree construction does not continue unless it would decrease 
# the overall lack of fit by a factor of cp. 

printcp(fit)

summary(fit)

#Plot tree 
plot(fit, margin=0.1, main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.7)

#extracting rules
asRules(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

#Prune the tree using the cp value
#fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
ptree<- prune(fit,cp=0.04 )

fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")

#Predictions
test$p<-predict(fit,newdata=test,type="class")

#Confusion matrix
table(test$p,as.numeric(test$DV))

#Metrics to check 
#VAlidation techniques in R
#Plotting ROC Curve
library(ROCR)
?performance
# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions with actual values

pred<-prediction(as.numeric(test$p),as.numeric(test$DV))
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N

plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
