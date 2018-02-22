# Setting up the environment variables:
Sys.setenv("HADOOP_CMD"="/usr/local/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.6.5.jar")
Sys.setenv("HADOOP_CONF"="/usr/local/hadoop/conf")

##Importing data directly to HDFS using rhdfs package
library(rhdfs)
hdfs.init() #Initializes rhdfs package


#Copying Netbook_Reviews.txt data from Local to HDFS path
hdfs.put('/home/hduser/RHadoop/Dataset/Netbook_Reviews.txt','/user/hduser/Chapter17/CaseStudy') #Copying file to HDFS
hdfs.ls('/user/hduser/Chapter17/CaseStudy') #Checking the file details

#Loading the rmr2 package
library(rmr2)

#Looking at the input data object in HDFS
check<-from.dfs("/user/hduser/Chapter17/CaseStudy/Netbook_Reviews.txt",format="text")
mode(check) #list object
mode(check$key) #NULL object
mode(check$val) #list object
head(check$val) #Checking values under val part of input object


# Q1. Write a mapreduce program to compute the general sentiment expressed in 
# the netbook reviews? Positive and Negative word lists are shared in the 
# server path under the file names positive-words.txt and negative-words.txt 
# respectively.

##Compute the general sentiment expressed in the netbook reviews
#Loading positive and negative word lists

# Using the positive and negative word lists
pos.words <- scan("/home/hduser/RHadoop/Dataset/positive-words.txt", "")
neg.words <- scan("/home/hduser/RHadoop/Dataset/negative-words.txt", "")

#Defining the map function
map <- function(k,v) {
  key<-1:length(v) #Adding row number for each review
  reviews<-tolower(v)
  words<-strsplit(reviews,"\\s") #Output would be list
  return(keyval(key,words))
}

#Defining the reduce function
reduce <- function(k,vv) {
  pos.score<-sapply(vv,function(x){sum(!is.na(match(x,pos.words)))})
  neg.score<-sapply(vv,function(x){sum(!is.na(match(x,neg.words)))})
  score = pos.score - neg.score
  return(keyval(k,score))
}


#Running the mapreduce function
out<-mapreduce(input = '/user/hduser/Chapter17/CaseStudy/Netbook_Reviews.txt',
               input.format = make.input.format("text"),
               map = map,
               reduce = reduce)


#Copying HDFS output to R console
score.df<-as.data.frame(from.dfs(out))
dim(score.df)
head(score.df)
summary(score.df$val)
hist(score.df$val) #Plotting sentiment score as histogram
