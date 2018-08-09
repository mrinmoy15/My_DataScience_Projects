library(XML)
library(RCurl)

## Getting the URL
myUrl<-getURL("file:///C:/Data%20Science%20with%20R/Assignments/Graded%20Assignments/Topic%206.2%20-%20%20Data%20Visualization%20in%20R/The%20World's%20Most%20Valuable%20Brands%20List%20-%20Forbes.html",ssl.verifypeer=F)

data <- readHTMLTable(myUrl)$the_list
class(data)
str(data)
head(data)
summary(data)
#------------------------------------------------------------------------------------------------

# Data Cleaning
summary(data) # No missing Values
data$Rank <- as.numeric(gsub("#","",data$Rank))
data$`Brand Value`<- gsub("$", "", data$`Brand Value`, fixed = TRUE)
data$`Brand Value`<- gsub("B", "", data$`Brand Value`)
data$Brand_Value_in_Billion_Dollars <- as.numeric(data$`Brand Value`)

data$`Brand Revenue` <- gsub("B", "", data$`Brand Revenue`)
data$`Brand Revenue` <- gsub("$", "", data$`Brand Revenue`, fixed = TRUE)
data$Brand_Revenue_in_Billion_Dollars <- as.numeric(data$`Brand Revenue`)

data$`Company Advertising` <- gsub("$", "" ,data$`Company Advertising`, fixed = TRUE)

index_B <- grepl("B", data$`Company Advertising`[])
index_M <- grepl("M", data$`Company Advertising`[])
data$`Company Advertising`[index_B] <- as.numeric(gsub("B","",data$`Company Advertising`[index_B]))
data$`Company Advertising`[index_M] <- as.numeric(gsub("M","",data$`Company Advertising`[index_M]))*0.001
data$ Company_Add_in_Bilion_Dollar <- as.numeric(data$`Company Advertising`)
data <- data.frame(data[,-1])

final_data <- data[,c(1,2,7,8,9,10)]
head(final_data)
#---------------------------------------------------------------------------------------------------------

## Plotting the results
library(ggplot2)

## plot1
myDataframe.Technology<-myDataframe[myDataframe$Industry=="Technology",]
p1<-ggplot(myDataframe.Technology,
           aes(x=CompanyAdvertising_in_billion_dollars,
               y=BrandRevenue_in_billion_dollars,colour=Brand, 
               size=BrandValue_in_billion_dollars))
p1+geom_jitter(na.rm = T)+theme(legend.key=element_rect(fill = "lightblue"),
                                panel.grid.major=element_line(colour = "Gray", size = 0.702),
                                panel.grid.minor=element_line(color = "lightGray", size = 0.01),
                                panel.background=element_rect(fill = "white"),
                                panel.border=element_rect(fill=NA,color = "Gray"),
                                plot.title=element_text(size = 20,face="bold",hjust = 0.7))+
geom_text(aes(label=Brand),na.rm = T,nudge_y =-3 )+guides(colour=F)+
labs(title="Technology",
     x="Company Advertising in Billions of $",
     y="Brand Revenue in Billions of $")+scale_size("Brand Value in $ (Billions)",c(30,50,100),range = c(3,6))
 


## Plot2
myDataframe.Luxury<-myDataframe[myDataframe$Industry=="Luxury",]
p2<-ggplot(myDataframe.Luxury,
           aes(x=CompanyAdvertising_in_billion_dollars,
               y=BrandRevenue_in_billion_dollars,colour=Brand,
               size=BrandValue_in_billion_dollars),na.rm=T)

p2+geom_jitter(na.rm = T)+
  geom_text(aes(label=Brand),na.rm = T,nudge_y =-0.3)+guides(colour=FALSE)+
  labs(title="Luxury",
       x="Company advertising in Billions of $",
       y="Brand Revenue in Billions of $")+
  theme(legend.key=element_rect(fill = "lightblue"),
        panel.grid.major=element_line(colour = "grey70", size=0.702),
        panel.grid.minor=element_line(color = "grey90", size = 0.003),
        panel.background=element_rect(fill = "white"),
        plot.title=element_text(size=20,face="bold",hjust = -0.04,vjust = -0.08),
        panel.border=element_rect(fill=NA,color = "gray", size = 1))+
  scale_size("Brand Value in $ (Billions)",c(10.0,20.1),range=c(3,6))+
  scale_x_continuous(breaks=seq(0.1,4.8,0.1))+
  scale_y_continuous(breaks=seq(4,10,2))+
  scale_color_manual(values=c(NA,NA,"#FF6699",NA,"#33CC00","#00CCCC","#CC66CC",NA))


## Plot3
myDataframe.Financial<-myDataframe[myDataframe$Industry=="Financial Services",]
p3<-ggplot(myDataframe.Financial,
               aes(x=CompanyAdvertising_in_billion_dollars,
                   y=BrandRevenue_in_billion_dollars,
                   size=BrandValue_in_billion_dollars))
p3+geom_jitter(na.rm = T,aes(colour=Brand))+geom_text(aes(label=Brand,color=Brand),na.rm = T,nudge_y =-1)+guides(colour=FALSE)+
  labs(title="Financial",
       x="Company advertising in Billions of $",
       y="Brand Revenue in Billions of $")+
  theme(legend.key=element_rect(fill = "lightblue"),
        panel.grid.major=element_line(colour = "grey70", size=0.702),
        panel.grid.minor=element_line(color = "grey90", size = 0.003),
        panel.background=element_rect(fill = "white"),
        plot.title=element_text(size=20,face="bold",hjust = -0.04,vjust = -0.08),
        panel.border=element_rect(fill=NA,color = "gray", size = 1))+
  scale_size("Brand Value in $ (Billions)",c(7.0,12.0,23.4),range = c(2,5))+
  scale_x_continuous(breaks=seq(0.6,3.4,0.1))+
  scale_y_continuous(limits = c(7,92), breaks=seq(10,90,10))+
  scale_color_manual(values = c("American Express"="#FF66CC","Visa"="#CC33CC","HSBC"=NA,"Wells Fargo"="#FF66CC","MasterCard"="#3399CC","J.P. Morgan"="#00FFCC","Santander"="#6699CC","Chase"="#CCFF66","Bank of America"="#CC9900","Citi"="#006600","RBC"=NA,"Allianz"=NA,"Goldman Sachs"=NA))
  
  

## Plot4
myDataframe.Automotive<-myDataframe[myDataframe$Industry=="Automotive",]
p4<-ggplot(myDataframe.Automotive,
           aes(x=CompanyAdvertising_in_billion_dollars,
               y=BrandRevenue_in_billion_dollars,
               size=BrandValue_in_billion_dollars))
p4+geom_jitter(na.rm = T,aes(colour=Brand))+geom_text(aes(label=Brand,color=Brand),na.rm = T,nudge_y =-4)+guides(colour=FALSE)+
  labs(title="Automotive",
       x="Company advertising in Billions of $",
       y="Brand Revenue in Billions of $")+
theme(legend.key=element_rect(fill = "lightblue"),
      panel.grid.major=element_line(colour = "grey70", size=0.702),
      panel.grid.minor=element_line(color = "grey90", size = 0.003),
      panel.background=element_rect(fill = "white"),
      plot.title=element_text(size=20,face="bold",hjust = -0.04,vjust = -0.08),
      panel.border=element_rect(fill=NA,color = "gray", size = 1))+
  scale_size("Brand Value in $ (Billions)",c(6.2,20.0,37.8),range = c(3,6))+
  scale_x_continuous(breaks=seq(0.8,5.4,0.1))+
  scale_y_continuous(limits = c(36,175), breaks=seq(40,170,10))
  