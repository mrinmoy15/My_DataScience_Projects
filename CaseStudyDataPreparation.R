library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

options(scipen = 999)
## Reading the data
## 1. Campaign
campaign<-read.table("C:/Data Science with R/Assignments/Graded Assignments/Topic 8.2 -  Data Preparation/Campaign_File.txt",sep = "\t", header = T)
head(campaign)
str(campaign)
summary(campaign)
campaign$Campaign_Responce<-as.factor(campaign$Campaign_Responce)   

## 2. Customers
customers<-read.table("C:/Data Science with R/Assignments/Graded Assignments/Topic 8.2 -  Data Preparation/Customers_File.txt",sep = "\t", header = T)
head(customers)
str(customers)
summary(customers)

## 3. Products
products<-read.table("C:/Data Science with R/Assignments/Graded Assignments/Topic 8.2 -  Data Preparation/Products_File.txt",sep = "\t", header = T)
head(products)
str(products)
summary(products)

## 4. Transactions
transactions<-read.table("C:/Data Science with R/Assignments/Graded Assignments/Topic 8.2 -  Data Preparation/Transactions_File.txt",sep = "\t", header = T)
head(transactions)
str(transactions)
summary(transactions)

# Basic Sanity check is done:
quantile(transactions$Product_Code,p=(1:100)/100)
quantile(transactions$Transaction_ID,p=(1:100)/100)
quantile(transactions$Items_Number,p=(1:10)/10)
transactions%>%dplyr::filter(transactions$Product_Code==1)%>%summarize(n())
transactions%>%dplyr::filter(transactions$Items_Number>1)%>%summarize(n())



##---------------------------------------------------------------------------------------------------------------------------------------------------
# Answer for Q1
merged_data1=merge(x=products,y=transactions,by="Product_Code", all = T)
head(merged_data1)
summary(merged_data1)
quantile(merged_data1$Items_Amount,p=(1:100)/100)
merged_data1%>%group_by(Product_Category)%>%summarise(ItemsAmount=sum(Items_Amount))%>%ungroup()%>%arrange(-ItemsAmount)%>%data.frame()->Amount_Summary

p=ggplot(Amount_Summary,aes(x=Product_Category,y=ItemsAmount,size=ItemsAmount,color=ItemsAmount))
p+geom_point()+theme_light()+xlab("Product Category")+ylab("Amount in $")+
  ggtitle("Amount vs Product Category")
## Thus the product category Entertainment dominates



##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Answer for Q2
merged_data2<-merge(x=customers,y=transactions,by="Card_ID",all.y=T)
head(merged_data2,5)
str(merged_data2)
summary(merged_data2)
## Conversion from factor to date
merged_data2$Birth_Date=as.Date(merged_data2$Birth_Date)
merged_data2$Registration_Date=as.Date(merged_data2$Registration_Date)
merged_data2$Timestamp=as.POSIXct(merged_data2$Timestamp, format="%Y-%m-%d %H:%M:%S")

merged_data2%>%mutate(age=as.numeric(difftime(Sys.Date(),Birth_Date, units="days"))/365.25)->merged_data2
merged_data2%>%mutate(age_quantile=ntile(age,10))%>%group_by(age_quantile)%>%summarise(ItemsAmount=sum(Items_Amount))%>%
  ungroup()%>%arrange(-ItemsAmount)%>%data.frame()->AmountByAge
head(AmountByAge,10)

quantile(merged_data2$age,p=(1:10)/10)
# so it is seen that the age group 41-46 has the most contribution in terms of amount spent.


##-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Answer for Q3
merged_data3<-merge(x=campaign, y= customers, by="Card_ID",all.y=T)
summary(merged_data3)
str(merged_data3)
# Removing the missing values in the dependent variable
index<-which(is.na(merged_data3$Campaign_Responce)==T)
merged_data3<-merged_data3[-index,]

merged_data3$Birth_Date=as.Date(merged_data3$Birth_Date)
merged_data3$Registration_Date=as.Date(merged_data3$Registration_Date)
merged_data3%>%mutate(age=as.numeric(difftime(Sys.Date(),Birth_Date,units="days")/365.25))->merged_data3

merged_data3%>%mutate(age_quantile=ntile(age,10))%>%group_by(age_quantile)%>%summarise(Total=n())%>%ungroup()%>%data.frame()->data1
merged_data3%>%mutate(age_quantile=ntile(age,10))%>%group_by(age_quantile)%>%dplyr::filter(Campaign_Responce=="TRUE")%>%summarise(PositiveResponse=n())%>%ungroup()%>%data.frame()->data2
data<-merge(data1,data2,by="age_quantile")
head(data)
summary(data)
data$Response_Rate=data$PositiveResponse/data$Total

which(data$Response_Rate== max(data$Response_Rate))
quantile(merged_data3$age,p=(1:10)/10)
# Hence maximum response rate is coming from the age group belong to the 9th decile(64-70 years of age)

age_bin<-melt(quantile(merged_data3$age,p=(1:10)/10))
class(age_bin)
names(age_bin)<-c("age")
data<-cbind(data,age_bin)
p1<-ggplot(data,aes(x=age,y=Response_Rate,fill=Response_Rate))
p1+geom_bar(stat = "identity",alpha=0.5)+theme_light()+ggtitle("Response Rate vs age ")

# There is no consistent trend in the data.



##------------------------------------------------------------------------------------------------------------------------------------------------
## Answer for Q4
merged_data4<-merge(x=campaign, y= customers, by="Card_ID",all=T)
summary(merged_data4)
str(merged_data4)

# Removing the missing values in the dependent variable
index<-which(is.na(merged_data4$Campaign_Responce)==T)
merged_data4<-merged_data4[-index,]
merged_data4$Birth_Date=as.Date(merged_data4$Birth_Date)
merged_data4$Registration_Date=as.Date(merged_data4$Registration_Date)
reference_date<-as.Date("31/12/2002","%d/%m/%Y")
merged_data4%>%mutate(Tenure=as.numeric(difftime(reference_date,Registration_Date,units="days"))/365.25)->merged_data4
merged_data4%>%mutate(Age=as.numeric(difftime(Sys.Date(),Birth_Date,units="days")/325.25))->merged_data4
merged_data4%>%mutate(Tenure_quantile=ntile(Tenure,10))%>%group_by(Tenure_quantile)%>%summarise(Total=n())%>%ungroup()%>%data.frame->data3
merged_data4%>%mutate(Tenure_quantile=ntile(Tenure,10))%>%group_by(Tenure_quantile)%>%dplyr::filter(Campaign_Responce=="TRUE")%>%summarise(PositiveResponse=n())%>%ungroup()%>%data.frame()->data4
data_cumulated<-merge(data3,data4,by="Tenure_quantile")
data_cumulated$Response_Rate=data_cumulated$PositiveResponse/data_cumulated$Total
which(data_cumulated$Response_Rate== max(data_cumulated$Response_Rate))
quantile(merged_data4$Tenure,p=(1:10)/10)

# Hence maximum response rate is coming from the Tenure group belong to the 9th decile(4.3 yrs-4.7 years of Tenure)
tenure_bin<-melt(quantile(merged_data4$Tenure,p=(1:10)/10))
names(tenure_bin)<-"Tenure"
data_cumulated<-cbind(data_cumulated,tenure_bin)
p<-ggplot(data_cumulated,aes(x=Tenure,y=Response_Rate,color=Response_Rate,size=Response_Rate))
p+geom_jitter()+stat_smooth(method = "lm")+theme_light()+
  ylab("Response Rate")+xlab("Tenure in years")+
  ggtitle("ResponseRate vs Tenure")+guides(size=F)
# There is a very weak relation between Response Rate and tenure.Roughly with increasing tenure Response is good to an extent.

###-----------------------------------------------------------------------------------------------------------------------------------------------
## Answer to Q5
head(data_cumulated)
head(data)
xtabs(Response_Rate~Tenure,data = data_cumulated)
xtabs(Response_Rate~age,data = data)


##--------------------------------------------------------------------------------------------------------------------------------
## Answer to Q6
transactions%>%group_by(Payment_Method)%>%summarise(count=n())%>%arrange(-count)
### credit card payment is the most popular
transactions$Timestamp=as.POSIXct(transactions$Timestamp, format="%Y-%m-%d %H:%M:%S")
transactions%>%mutate(Hour_info=hour(Timestamp))%>%group_by(Hour_info,Payment_Method)%>%summarise(Count=n())%>%ungroup()%>%data.frame()->Timewise_payment_method
head(Timewise_payment_method)

xtabs(Count~Hour_info+Payment_Method,data = Timewise_payment_method)
p6<-Timewise_ggplot(Timewise_payment_method,aes(x=as.factor(Payment_Method),y=Count))
p6+geom_bar(stat = "identity")+facet_grid(.~Hour_info)

## From the plot as well as tabulation it is seen that the time of the day has not much impact on the payment method.
## Apperantly at all the hours credit cars transactions are the dominant mode of payment. Only after 18:00 hrs the amount of transaction 
## drops drastically.


##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Answer to Q7:
## we can use merged_data2 which is obtained by merging transactions and Customers dataset
head(merged_data2)
merged_data2%>%mutate(Age_quantile=ntile(age,10))%>%group_by(Gender,Age_quantile)%>%summarise(Amount_Spent=sum(Items_Amount))%>%ungroup()%>%data.frame()->data5
data5
xtabs(Amount_Spent~Gender+Age_quantile,data = data5)
p7<-ggplot(data5,aes(x=as.factor(Age_quantile),y=Amount_Spent))
p7+geom_bar(stat = "identity")+facet_grid(.~Gender)+xlab("Age quantile")+ylab("Amount Spent in $")

# From both cross table and the bar chart it is clear that for both male and female falling in the age bucket
# 2 and 3 i.e. age group 41 -50 years have the highest amount spent.



##--------------------------------------------------------------------------------------------------------------------------------
## Answer to Q8
merged_data4%>%filter(Gender=="M")->male_tenure
merged_data4%>%filter(Gender=="F")->female_tenure
head(male_tenure)
p8.1<-ggplot(male_tenure,aes(x=Tenure))
p8.1+geom_histogram(aes(fill="red",color="red"),alpha=0.5,position = "dodge")+
                     labs(x="Tenure in years",y="Frequency count",title="Male Customer Tenure")+
                     guides(fill=F,colour=F)

p8.2<-ggplot(female_tenure,aes(x=Tenure))
p8.2+geom_histogram(aes(fill="red",color="red"),alpha=0.5,position = "dodge")+
  labs(x="Tenure in years",y="Frequency count",title="Female Customer Tenure")+
  guides(fill=F,colour=F)