# Loading the required packages
library(dplyr)
library(ggplot2)
library(car)

# Reading the data
data1<-read.csv("E:/Studies/Data_Analytics_Courses/Jigsaw_Courses/R_dataScience/T9/CaseStudy/CaseStudyExample/DirectMarketing.csv")
head(data1)

##-----------------------------------------------------------------------------------------------
## Do exploratory analysis
plot(data1$Age,data1$AmountSpent, col="red", main = "Amount spent vs Age", xlab = "Age", ylab = "Amount Spent(Rs.)")

## Since the middle and old age customers behave similarlly in terms of amount spent
## We combine middle and old levels together
data1$Age1<-ifelse(data1$Age!="Young","Middle_Old",as.character(data1$Age))
data1$Age1<-as.factor(data1$Age1)
summary(data1$Age1)

plot(data1$Age1,data1$AmountSpent, col="red")

## Gender
plot(data1$Gender,data1$AmountSpent, col="red")

#Own House
summary(data1$OwnHome)
plot(data1$OwnHome,data1$AmountSpent, col="red")

#Married or not married
summary(data1$Married)
plot(data1$Married,data1$AmountSpent, col="red")

#Location
summary(data1$Location)
plot(data1$Location,data1$AmountSpent, col="red")

# Salary
summary(data1$Salary)
plot(data1$Salary,data1$AmountSpent)

# Children
summary(data1$Children)
plot(as.factor(data1$Children),data1$AmountSpent, col="red")

## Combine 2 and 3 children group
data1$Children1<-ifelse(data1$Children==2|data1$Children==3,"2-3", as.character(data1$Children))
data1$Children1<-as.factor(data1$Children1)
summary(data1$Children1)
plot(data1$Children1,data1$AmountSpent,col="red")

## History
summary(data1$History)
ind<-which(is.na(data1$History))
mean(data1[ind,"AmountSpent"])
tapply(data1$AmountSpent,data1$History,mean)

## Create a catagory called missing
data1$History1<-ifelse(is.na(data1$History)==T,"missing",as.character(data1$History))
data1$History1<-as.factor(data1$History1)
summary(data1$History1)
plot(data1$History1,data1$AmountSpent,col="red")

# Catalog
summary(data1$Catalogs)

# Remove the old variables which we have transformed.
data2<-data1[,-c(1,7,8)]


# Linear Regression model
mod1<-lm(AmountSpent~. ,data = data2)
summary(mod1)

## Creating Dummy Variables:
data2$Male_d=ifelse(data2$Gender=="Male",1,0)
data2$Female_d=ifelse(data2$Gender=="Female",1,0)
data2$Missing_d=ifelse(data2$History1=="missing",1,0)
data2$Low_d=ifelse(data2$History1=="Low",1,0)
data2$Medium_d=ifelse(data2$History1=="Medium",1,0)
data2$High_d=ifelse(data2$History1=="High",1,0)

mod2<-lm(formula = AmountSpent~Male_d+Location+Salary+Catalogs+Children1+Medium_d+Low_d, data = data2)
summary(mod2)
mod2_data <- mod2$coefficients %>% melt() %>% data.frame()
names(mod2_data) <- "Coefficient Value"
write.csv(mod2_data, "mod2.csv")
## Sign of the variables

tapply(data2$AmountSpent,data2$History1,mean)
data2%>%dplyr::filter(History1!="Medium"|History1!="Low")%>%summarise(Mean=mean(AmountSpent))
tapply(data2$AmountSpent,data2$Location,mean)


### Assumption Check
hist(mod2$residuals)
qqPlot(mod2$residuals)

## Checking multicolinearity
vif(mod2)

# Vif less then 10 so no problem of multi-vcolinearity.

## Constatn Variance check
plot(mod2$fitted.values,mod2$residuals)  ## Funnel shape


## Remedies: Apply log transform to y variable
mod3<-lm(formula = log(AmountSpent)~Location+Salary+Catalogs+Children1+Medium_d+Low_d, data=data2)

summary(mod3)

### Assumption check
qqPlot(mod3$residuals)
plot(mod3$fitted.values,mod3$residuals) ## Still funnel


## Apply square root transform
mod4<-lm(formula = sqrt(AmountSpent)~Location+Salary+Catalogs+Children1+Medium_d+Low_d, data=data2)
summary(mod4)

mod4$coefficients


## Assumption check
qqPlot(mod4$residuals)
plot(mod4$fitted.values,mod4$residuals) # Still Funnel

vif(mod4)

## Predicted and actual values comparision
predicted<-mod4$fitted.values
actual<-sqrt(data2$AmountSpent)

dat<-data.frame(predicted,actual)
p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),color="black") +labs(title = "Predicted vs Actual",x = "actual", y = "predicted")