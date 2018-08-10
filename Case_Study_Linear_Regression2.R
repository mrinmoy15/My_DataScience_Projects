# loading Necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)
#------------------------------------------------------------------------------------------------
##Loading Data
prices<-read.csv("C:/Jig12673/NonGradedAssingments/T9/files/boston_prices.csv",header=TRUE,stringsAsFactors=FALSE)
summary(prices)
str(prices)
head(prices)
#-------------------------------------------------------------------------------------------
# Basic Sanity Check
summary(prices$CRIM)
prices%>%filter(CRIM>=1)%>%summarise(rows=n())

summary(prices$ZN)

summary(prices$INDUS)

summary(prices$Charles.River.dummy.variable)

summary(prices$nitric.oxides.concentration)

summary(prices$X.rooms.dwelling)

summary(prices$AGE)

summary(prices$DIS)

summary(prices$RAD)

summary(prices$TAX)

summary(prices$PTRATIO)

summary(prices$B)

summary(prices$LSTAT)

summary(prices$MEDV)
#-------------------------------------------------------------------------------------------------------------------
## Missing Value and outlier treatment:
# Missing Value
colSums(is.na(prices))
#MEDV has a lot of missing values
## but MEDV here is the dependent variable, so we simply delete these observations and not impute them.
summary((prices$MEDV))
index <- which (is.na(prices$MEDV))
prices <-prices[-index,]  
#------------------------------------------------------------------------------------------------------
## Outlier treatment
par(mfrow=c(2,7))
list<-names(prices)
class(list)
list=list[-4]
for(i in 1:length(list))
{
  boxplot(prices[,list[i]],main=list[i])
}

#Restore the par parameters to normal
dev.off()

for(i in 1: length(list))
{
  x<-boxplot(prices[,list[i]])
  out<-x$out
  index<-which(prices[,list[i]] %in% x$out)
  prices[index,list[i]]<-mean(prices[,list[i]],na.rm = T)
  rm(x)
  rm(out)
}
#--------------------------------------------------------------------------------------------------
# Exploratory Data Analysis

# look at the correlation between each IDV and the DV
ggplot(prices,aes(x=MEDV,y=LSTAT)) +geom_point()
ggplot(prices,aes(x=MEDV,y=DIS)) +geom_point()
ggplot(prices,aes(x=MEDV,y=AGE)) +geom_point()

#Inorder to quicken the process, lets write a function :
#Below is a function that gives you the correlation values between all IDV's and the DV
#Simply taking a look at the output of this function, you can quickly shortlist 
#Which all IDV's are correlated to the DV

#Function to get the list of correlations between : DV and the IDV's
list1<-list[-13]
for(i in 1:length(list1))
{
  x<-cor(prices$MEDV,prices[list[i]])
  print(x)
}

#Significant variables are : B LSTAT AGE X.rooms.dwelling nitric.oxides.concentration INDUS

#You can also try to use data transformations

#Log transformations
#Create the log transformation for all variables
prices$log_CRIM<-log(prices$CRIM)
prices$log_ZN<-log(prices$ZN)
prices$log_NOX<-log(prices$nitric.oxides.concentration)
prices$log_RM<-log(prices$X.rooms.dwelling)
prices$log_AGE<-log(prices$AGE)
prices$log_DIS<-log(prices$DIS)
prices$log_RAD<-log(prices$RAD)
prices$log_TAX<-log(prices$TAX)
prices$log_PTRATIO<-log(prices$PTRATIO)
prices$log_B<-log(prices$B)
prices$log_LSTAT<-log(prices$LSTAT)
prices$log_MEDV<-log(prices$MEDV) #DV
prices$log_INDUS<-log(prices$INDUS)


#Refer to the profiling excel sheet to see all the correlations documented

#Function to get the list of correlations between : log_DV and log of IDV's

list_log<-names(prices)[c(15:25,27)]
for(i in 1:length(list_log))
{
  xlog<-cor(prices$log_MEDV,prices[list_log[i]])
  print(xlog)
}

#Function to get the list of correlations between : log_DV and IDV's

list_log_DV<-names(prices)[1:13]
list_log_DV<-list_log_DV[-4]
for(i in 1:length(list_log_DV))
{
  xlogdv<-cor(prices$log_MEDV,prices[list_log_DV[i]])
  print(xlogdv)
}

# ---------------------------------------------------------------------------------------
# Model Building
# #Select training and testing sample
sampling<-sort(sample(nrow(prices), nrow(prices)*.7))

train<-prices[sampling,]
test<-prices[-sampling,]

# Building SimpLe Linear Regression Model
# We choose the log transformed dependent variable

Reg<-lm(log_MEDV~CRIM+INDUS+RAD+TAX+B+
          Charles.River.dummy.variable+
          DIS+ZN+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)

summary(Reg)

#Getting the formula
formula(Reg)

#Remove insignificant variables :

Reg1<-lm(log_MEDV~
           Charles.River.dummy.variable+
           DIS+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)
summary(Reg1)



#Reg2 : remove insignificant values

Reg2 <- lm(log_MEDV ~CRIM+INDUS+RAD+TAX+B+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+X.rooms.dwelling+nitric.oxides.concentration, data=train)
summary(Reg2)


#Reg3 _ remove insignificant values
Reg3 <- lm(log_MEDV ~CRIM+RAD+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+nitric.oxides.concentration, data=train)
summary(Reg3)


#The best model happens to be : Reg3

##-------------------------------------------------------------------------------------

##Getting predicted values
predicted<-predict(Reg3)
plot(predicted)
length(predicted)

##Finding Residuals
residuals<-resid(Reg3)
plot(residuals)
length(residuals)

hist(Reg3$residuals)
qqPlot(Reg3$residuals)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity
##There should be no trend between predicted values and residual values
plot(predicted,residuals,abline(0,0))

#You can notice that there seems to be an inverse pattern for some points

#So this model may not be the preferred model.

#atttching predicted values to test data
predicted<-predict(Reg3,newdata=test)
length(predicted)
test$p<-predicted

#Calculating error in the test dataset - (Actual- predicted)/predicted values
test$error<-(test$log_MEDV-test$p)/test$log_MEDV
mean(test$error)*100 #you get to know the average error in the given dataset



##Plotting actual vs predicted values
plot(test$p,col="blue",type="l")
lines(test$log_MEDV,col="red",type="l")

#checking for Correlation between variables
library(car)
vif(Reg3)