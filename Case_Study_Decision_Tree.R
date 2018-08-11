## Readin the data
BH<-read.csv("C:/Jig12673/GradedAssignments/T11.2/BH.csv")
head(BH)
str(BH)
summary(BH)
##-------------------------------------------------------------------------------------------
## Loading required packages
load_package<-function(x){
  require(x, character.only = TRUE)
}
packages<-c("dplyr","irr","rpart","caret","rattle","rpart.plot","RColorBrewer")

for(i in 1: length(packages)){
  load_package(packages[i])
}

##---------------------------------------------------------------------------------------------
## EDA
## Minimal Data preparation
## Missing value treatment

## deleting the observations where target variable is missing
index<-which(is.na(BH$TARGET))
BH<-BH[-index,]
summary(BH)
colnames(BH)

## Decision tree model
set.seed(345)
tree<-rpart(TARGET~cus_employername, data = BH, method = "class", control= rpart.control(cp=0.001, maxdepth = 7), parms = list(split="gini"))
fancyRpartPlot(tree)
prp(tree,extra=1)

## Bad rate computation for the two groups resulting from the tree
BH1<-BH[tree$where==2,]
BH2<-BH[tree$where==3,]

BH1$GOOD_BAD_d<-ifelse(BH1$GOOD_BAD=="BAD",1,0)
bad_rate_BH1<-sum(BH1$GOOD_BAD_d)/nrow(BH1)

BH2$GOOD_BAD_d<-ifelse(BH2$GOOD_BAD=="BAD",1,0)
bad_rate_BH2<-sum(BH2$GOOD_BAD_d)/nrow(BH2)

## bad_rate_BH1 is very high  and bad_rate_BH2 is very low.


## Creating a Dummy Variable column called Group in the original BH dataset where Group=Group1 for 
## high bad rate and Group2 for low bad rate

Group<-ifelse(tree$where==2,"Group1", "Group2")
Group<-as.factor(Group)

head(Group)

BH_modified<-cbind(BH,Group)
head(BH_modified)
