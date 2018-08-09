# Required Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)

#Reading the data
contribution<-read.csv("C:/Jig12673/NonGradedAssingments/T8.1/Files/contribution.csv",header = T)
head(contribution)


options(scipen = 999)

# Create a variable for analysing the contributions across all years by the alumni.
contribution %>% mutate(Contributions=FY04Giving+FY03Giving+FY02Giving+FY01Giving+FY00Giving)->contribution
head(contribution)

#Finding out how total contributions are distributed by Gender, Batch, Marital Status etc

# 1.Gender

gender_wise_contribution <- contribution %>% group_by(Gender) %>% 
  summarise(Count=n(),Percentage_Count=Count/nrow(contribution),Total_Contribution=sum(Contributions),Percentage_Contribution=Total_Contribution/1205454,Average=mean(Contributions))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%data.frame()

#Males appear to be the majority contributors while being as many in number as females

#-----------------------------------------------------------------------------------------------------------
# 2.Batch
contribution%>%group_by(Class.Year)%>%summarise(Count=n(),Percentage_Count=Count/nrow(contribution),Total_Contribution=sum(Contributions),Percentage_Contribution=Total_Contribution/1205454,Average=mean(Contributions))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%data.frame()%>%head(10)
##This result is expected, alumni who have passed a long time ago are the ones who are contributing the most. Also notice that they comprise only 10% of the student base
#----------------------------------------------------------------------------------------------------------------

# 3.Marital Status
contribution%>%group_by(Marital.Status)%>%summarise(Count=n(),Percentage_Count=Count/nrow(contribution),Total_Contribution=sum(Contributions),Percentage_Contribution=Total_Contribution/1205454,Average=mean(Contributions))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%data.frame()%>%head(10)

##Married people contribute the most and are also the largest group by count
#---------------------------------------------------------------------------------------------------------------------

# 4.Major
contribution%>%group_by(Major)%>%summarise(Count=n(),Percentage_Count=Count/nrow(contribution),Total_Contribution=sum(Contributions),Percentage_Contribution=Total_Contribution/1205454,Average=mean(Contributions))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%data.frame()
##History majors are the highest contributors followed by Mathematics and Econonmics
#-----------------------------------------------------------------------------------------------------------------------------------------

# 5.Next Degree
contribution%>%group_by(Next.Degree)%>%summarise(Count=n(),Percentage_Count=Count/nrow(contribution),Total_Contribution=sum(Contributions),Percentage_Contribution=Total_Contribution/1205454,Average=mean(Contributions))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%data.frame()%>%head(6)
##People who didn't go on to pursue any degree further are the largest contributors
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# 5.Attendence event
contribution%>%group_by(AttendenceEvent)%>%summarise(Count=n(),Percentage_Count=Count/nrow(contribution),Total_Contribution=sum(Contributions),Percentage_Contribution=Total_Contribution/1205454,Average=mean(Contributions))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%data.frame()%>%head(6)

# This result is again expected, people who come to fundraising events they contribute the most.
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Now we will look at how total contributions are changing over the years
contribution%>%summarize(FY04=sum(FY04Giving),FY03=sum(FY03Giving),FY02=sum(FY02Giving),FY01=sum(FY01Giving),FY00=sum(FY00Giving))%>%melt->tot_cont
names(tot_cont)<-c("Year","Contribution")
tot_cont$Per_Cont<-round(tot_cont$Contribution/1205454,2)
tot_cont$Cumu_Contr<-cumsum(tot_cont$Per_Cont)
summary(tot_cont)
head(tot_cont)

p<-ggplot(tot_cont,aes(x=Year,y=Contribution,fill=Year))

p+geom_bar(stat="identity",alpha=0.7)+geom_text(aes(label=Per_Cont,colour=Per_Cont),vjust=-0.3)+theme_classic()+scale_fill_discrete(c=50,h=c(1,250),h.start =50 )+guides(color=FALSE)

# It seems like that total contributions are variable and there is no predictable trend.
#-----------------------------------------------------------------------------------------
# We will now look at total contribution year wise by:
#   
# 1.Gender
# 2.Batch
# 3.Marital Status
# 4.Attendence at fundrasing events
# 5.Major
# We will focus on the contributors only.

#1. Gender:
#The contribution break up year wise according to gender along with count and percentage count is as follows:
contribution%>%filter(FY04Giving>0)%>%group_by(Gender)%>%summarise(Count=n(),Percentage_Count=n()/507,Total_Contribution=sum(FY04Giving),Percentage_Contribution=round(Total_Contribution/196061.8,2),Average=mean(FY04Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year=rep("FY04",2))%>%data.frame()->GFY04

contribution%>%filter(FY03Giving>0)%>%group_by(Gender)%>%summarise(Count=n(),Percentage_Count=n()/531,Total_Contribution=sum(FY03Giving),Percentage_Contribution=round(Total_Contribution/297013.8,2),Average=mean(FY03Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year=rep("FY03",2))%>%data.frame()->GFY03

contribution%>%filter(FY02Giving>0)%>%group_by(Gender)%>%summarise(Count=n(),Percentage_Count=n()/548,Total_Contribution=sum(FY02Giving),Percentage_Contribution=round(Total_Contribution/164153.8,2),Average=mean(FY02Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year=rep("FY02",2))%>%data.frame()->GFY02

contribution%>%filter(FY01Giving>0)%>%group_by(Gender)%>%summarise(Count=n(),Percentage_Count=n()/600,Total_Contribution=sum(FY01Giving),Percentage_Contribution=round(Total_Contribution/340130.6,2),Average=mean(FY01Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year=rep("FY01",2))%>%data.frame()->GFY01

contribution%>%filter(FY00Giving>0)%>%group_by(Gender)%>%summarise(Count=n(),Percentage_Count=n()/539,Total_Contribution=sum(FY00Giving),Percentage_Contribution=round(Total_Contribution/208093.6,2),Average=mean(FY00Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year=rep("FY00",2))%>%data.frame()->GFY00

Gender<-rbind(GFY00,GFY01,GFY02,GFY03,GFY04)
Gender

p<-ggplot(Gender,aes(x=Contribution.Year,y=Total_Contribution,fill=Gender))
Contrib<-p+geom_bar(stat="identity",position="stack",alpha=0.5)+
  geom_text(aes(label=Percentage_Contribution),position="stack",vjust=2)+
  theme_classic()+
  scale_fill_discrete(c=50,h =c(1,100),h.start = 50,direction = -1)+
  ggtitle("Contribution by Gender")

l<-ggplot(Gender,aes(x=Contribution.Year,y=Percentage_Count,group=Gender,colour=Gender))
PerCount<-l+geom_line()+theme_classic()+
  ggtitle("Percentage Number of Males and Females")+
  scale_color_discrete(c=50,h =c(1,100),h.start = 50,direction = -1)+
  xlab("Contribution Year")+ylab("Percent Count")

TotCount<-l+geom_line(aes(y=Count))+theme_classic()+
  ggtitle("Total Number of Males and Females")+
  scale_color_discrete(c=50,h =c(1,100),h.start = 50,direction = -1)+
  xlab("Contribution Year")+ylab("Total Count")
grid.arrange(Contrib,PerCount,TotCount)

#Clearly male alumni are the major contributors and the proportion of contribution split is relatively less volatile
#---------------------------------------------------------------------------------------------------------------
#Batch
#The contribution break up year wise according to batch along with count and percentage count is as follows:
contribution%>%filter(FY04Giving>0)%>%group_by(Class.Year)%>%
  summarise(Count=n(),Percentage_Count=n()/507,Total_Contribution=sum(FY04Giving),Percentage_Contribution=round(Total_Contribution/196061.8,2),Average=mean(FY04Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year=rep("FY04",5))%>%data.frame()->CLY04

contribution%>%filter(FY03Giving>0)%>%group_by(Class.Year)%>%
  summarise(Count=n(),Percentage_Count=n()/531,Total_Contribution=sum(FY03Giving),Percentage_Contribution=round(Total_Contribution/297013.8,2) ,Average=mean(FY03Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY03")%>%data.frame()->CLY03

contribution%>%filter(FY02Giving>0)%>%group_by(Class.Year)%>%
  summarise(Count=n(),Percentage_Count=n()/548,Total_Contribution=sum(FY02Giving),Percentage_Contribution=round(Total_Contribution/164153.8,2) ,Average=mean(FY02Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY02")%>%data.frame()->CLY02

contribution%>%filter(FY01Giving>0)%>%group_by(Class.Year)%>%
  summarise(Count=n(),Percentage_Count=n()/600,Total_Contribution=sum(FY01Giving),Percentage_Contribution=round(Total_Contribution/340130.6,2),Average=mean(FY01Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY01")%>%data.frame()->CLY01

contribution%>%filter(FY00Giving>0)%>%group_by(Class.Year)%>%
  summarise(Count=n(),Percentage_Count=n()/539,Total_Contribution=sum(FY00Giving),Percentage_Contribution=round(Total_Contribution/208093.6,2 ),Average=mean(FY00Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY00")%>%data.frame()->CLY00

Class<-rbind(CLY00,CLY01,CLY02,CLY03,CLY04)
Class$Class.Year<-as.factor(Class$Class.Year)

p<-ggplot(Class,aes(x=Contribution.Year,y=Total_Contribution,fill=Class.Year))
Contrib<-p+geom_bar(stat="identity",position="stack",alpha=0.8)+
  theme_light()+scale_fill_discrete(c=50,h=c(1,300),h.start = 50)+
  ggtitle("Contribution By Batch")+
  ylab("Total Contribution in $")+xlab("Contribution Year")

l<-ggplot(Class,aes(x=Contribution.Year,y=Percentage_Count,group=Class.Year,colour=Class.Year))
PerCount<-l+geom_line()+theme_classic()+
  ggtitle("Percentage Number of Alumni By Batch")+
  xlab("Contribution Year")+ylab("Percent Count")+
  scale_color_discrete(c=50,h=c(1,300),h.start = 50)

TotCount<-l+geom_line(aes(y=Count))+theme_classic()+
  ggtitle("Total Number of Alumni By Batch")+
  ylab("Total Count")+xlab("Contribution Year")+
  scale_color_discrete(c=50,h=c(1,300),h.start = 50)

grid.arrange(Contrib,PerCount,TotCount)

#The proportion of alumni from the batch of 1957 is low but we can see that the contribution by this batch is disproportionately more than others.
#-----------------------------------------------------------------------------------------------------
# Marital Status
#The contribution break up year wise according to marital status along with count and percentage count is as follows:

contribution%>%filter(FY04Giving>0)%>%group_by(Marital.Status)%>%
  summarise(Count=n(),Percentage_Count=n()/507,Total_Contribution=sum(FY04Giving),Percentage_Contribution=round(Total_Contribution/196061.8,2),Average=mean(FY04Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY04")%>%data.frame()->MSY04

contribution%>%filter(FY03Giving>0)%>%group_by(Marital.Status)%>%
  summarise(Count=n(),Percentage_Count=n()/531,Total_Contribution=sum(FY03Giving),Percentage_Contribution=round(Total_Contribution/297013.8,2) ,Average=mean(FY03Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY03")%>%data.frame()->MSY03

contribution%>%filter(FY02Giving>0)%>%group_by(Marital.Status)%>%
  summarise(Count=n(),Percentage_Count=n()/548,Total_Contribution=sum(FY02Giving),Percentage_Contribution=round(Total_Contribution/164153.8,2) ,Average=mean(FY02Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY02")%>%data.frame()->MSY02

contribution%>%filter(FY01Giving>0)%>%group_by(Marital.Status)%>%
  summarise(Count=n(),Percentage_Count=n()/600,Total_Contribution=sum(FY01Giving),Percentage_Contribution=round(Total_Contribution/340130.6,2),Average=mean(FY01Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY01")%>%data.frame()->MSY01

contribution%>%filter(FY00Giving>0)%>%group_by(Marital.Status)%>%
  summarise(Count=n(),Percentage_Count=n()/539,Total_Contribution=sum(FY00Giving),Percentage_Contribution=round(Total_Contribution/208093.6,2 ),Average=mean(FY00Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY00")%>%data.frame()->MSY00

Marital<-rbind(MSY00,MSY01,MSY02,MSY03,MSY04)


p<-ggplot(Marital,aes(x=Contribution.Year,y=Total_Contribution,fill=Marital.Status))
Contrib<-p+geom_bar(stat="identity",alpha=0.6)+theme_light()+
  scale_fill_discrete(c=50,h=c(1,250),h.start =50)+
  ylab("Total Contribution in $")+xlab("Contribution Year")+
  ggtitle("Contribution by Marital Status")

l<-ggplot(Marital,aes(x=Contribution.Year,y=Percentage_Count,group=Marital.Status,colour=Marital.Status))
PerCount<-l+geom_line()+theme_classic()+
  ggtitle("Percentage Number of Alumni by Marital Status")+
  xlab("Contribution Year")+ylab("Percent Count")+
  scale_color_discrete(c=50,h=c(1,250),h.start =50)

TotCount<-l+geom_line(aes(y=Count))+theme_classic()+
  ggtitle("Total Number of Alumni by Marital Status")+
  ylab("Total Count")+xlab("Contribution Year")+
  scale_color_discrete(c=50,h=c(1,250),h.start =50)

grid.arrange(Contrib,PerCount,TotCount)

# Married people are major contributors. And the trend is persistent, 
# its the married people who are contributing a lot. 
# But we can also observe that although divorced alumni are lesser in number 
# compared to single alumni but the dollar amount contributed is similar to the group of people who are single.
#-----------------------------------------------------------------------------------------------------------------------------------
# Attendence in a fund rising event
# The contribution break up year wise according to attendence along with count and percentage count is as follows:

contribution%>%filter(FY04Giving>0)%>%group_by(AttendenceEvent)%>%
  summarise(Count=n(),Percentage_Count=n()/507,Total_Contribution=sum(FY04Giving),Percentage_Contribution=round(Total_Contribution/196061.8,2),Average=mean(FY04Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY04")%>%data.frame()->ATY04

contribution%>%filter(FY03Giving>0)%>%group_by(AttendenceEvent)%>%
  summarise(Count=n(),Percentage_Count=n()/531,Total_Contribution=sum(FY03Giving),Percentage_Contribution=round(Total_Contribution/297013.8,2) ,Average=mean(FY03Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY03")%>%data.frame()->ATY03


contribution%>%filter(FY02Giving>0)%>%group_by(AttendenceEvent)%>%
  summarise(Count=n(),Percentage_Count=n()/548,Total_Contribution=sum(FY02Giving),Percentage_Contribution=round(Total_Contribution/164153.8,2) ,Average=mean(FY02Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY02")%>%data.frame()->ATY02


contribution%>%filter(FY01Giving>0)%>%group_by(AttendenceEvent)%>%
  summarise(Count=n(),Percentage_Count=n()/600,Total_Contribution=sum(FY01Giving),Percentage_Contribution=round(Total_Contribution/340130.6,2),Average=mean(FY01Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY01")%>%data.frame()->ATY01

contribution%>%filter(FY00Giving>0)%>%group_by(AttendenceEvent)%>%
  summarise(Count=n(),Percentage_Count=n()/539,Total_Contribution=sum(FY00Giving),Percentage_Contribution=round(Total_Contribution/208093.6,2 ),Average=mean(FY00Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY00")%>%data.frame()->ATY00


Attendence<-rbind(ATY00,ATY01,ATY02,ATY03,ATY04)
Attendence$AttendenceEvent<-as.factor(Attendence$AttendenceEvent)

p<-ggplot(Attendence,aes(x=Contribution.Year,y=Total_Contribution,fill=AttendenceEvent))
Contrib<-p+geom_bar(stat="identity",position="stack",alpha=0.8)+
  theme_light()+scale_fill_discrete(c=50,h=c(1,100),h.start=50)+
  ylab("Total Contribution in $")+xlab("Contribution Year")+
  ggtitle("Contribution by Attendence Status")   

l<-ggplot(Attendence,aes(x=Contribution.Year,y=Percentage_Count,group=AttendenceEvent,colour=AttendenceEvent))
PerCount<-l+geom_line(size=1.2)+theme_classic()+
  ggtitle("Percentage Number of Alumni by Attendence Status")+
  xlab("Contribution Year")+ylab("Percent Count")+
  scale_color_discrete(c=50,h=c(1,100),h.start=50)

TotCount<-l+geom_line(aes(y=Count),size=1.2)+theme_classic()+
  ggtitle("Total Number of Alumni by Attendence Status")+
  ylab("Total Count")+xlab("Contribution Year")+
  scale_color_discrete(c=50,h=c(1,100),h.start=50)

grid.arrange(Contrib,PerCount,TotCount)

# There is a persistent trend in amount contributed by people who attend events. 
# The count of people attending the events is also same across all the years.

#--------------------------------------------------------------------------------------------------------

# Major
#The contribution break up year wise according to Major along with count and percentage count is as follows: 
#(For this analysis we chose only those students who contributed more than 8% in a given year

contribution%>%filter(FY04Giving>0)%>%group_by(Major)%>%
  summarise(Count=n(),Percentage_Count=n()/507,Total_Contribution=sum(FY04Giving),Percentage_Contribution=round(Total_Contribution/196061.8,2),Average=mean(FY04Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY04")%>%data.frame()->MY04

contribution%>%filter(FY03Giving>0)%>%group_by(Major)%>%
  summarise(Count=n(),Percentage_Count=n()/531,Total_Contribution=sum(FY03Giving),Percentage_Contribution=round(Total_Contribution/297013.8,2) ,Average=mean(FY03Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY03")%>%data.frame()->MY03

contribution%>%filter(FY02Giving>0)%>%group_by(Major)%>%
  summarise(Count=n(),Percentage_Count=n()/548,Total_Contribution=sum(FY02Giving),Percentage_Contribution=round(Total_Contribution/164153.8,2) ,Average=mean(FY02Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY02")%>%data.frame()->MY02

contribution%>%filter(FY01Giving>0)%>%group_by(Major)%>%
  summarise(Count=n(),Percentage_Count=n()/600,Total_Contribution=sum(FY01Giving),Percentage_Contribution=round(Total_Contribution/340130.6,2),Average=mean(FY01Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY01")%>%data.frame()->MY01

contribution%>%filter(FY00Giving>0)%>%group_by(Major)%>%
  summarise(Count=n(),Percentage_Count=n()/539,Total_Contribution=sum(FY00Giving),Percentage_Contribution=round(Total_Contribution/208093.6,2 ),Average=mean(FY00Giving))%>%
  ungroup()%>%arrange(-Total_Contribution)%>%mutate(Contribution.Year="FY00")%>%data.frame()->MY00

Major<-rbind(MY00,MY01,MY02,MY03,MY04)

p<-ggplot(Major%>%filter(Percentage_Contribution>0.08),aes(x=Contribution.Year,y=Total_Contribution,fill=Major))
Contrib<-p+geom_bar(stat="identity",position="stack",alpha=0.6)+
  theme_classic()+
  ggtitle("Contribution break up by Major: Mninmum contribution of 8%")+
  scale_fill_discrete(h.start=50,c=50)+
  ylab("Total Contribution")+xlab("Contribution Year")

l<-ggplot(Major%>%filter(Percentage_Contribution>0.08),aes(x=Contribution.Year,y=Percentage_Count,group=Major,colour=Major))
PerCount<-l+geom_line()+theme_classic()+
  ggtitle("Percentage Number of Alumni by Major")+
  xlab("Contribution Year")+ylab("Percent Count")+
  geom_point(size=5)+scale_color_discrete(h.start=50,c=50)

TotCount<-l+geom_line(aes(y=Count))+theme_classic()+
  ggtitle("Total Number of Alumni by Major")+
  ylab("Total Count")+xlab("Contribution Year")+
  geom_point(aes(y=Count),size=5)+scale_color_discrete(h.start=50,c=50)

grid.arrange(Contrib,PerCount,TotCount)
# Most contributions come from History majors and there seems to be some persistence in the trend. 
# The large contribution in FY01 by a Mathematics-Physics major seems like a one off affair.