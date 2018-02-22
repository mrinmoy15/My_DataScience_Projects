#####Class 16 - Text Analysis using RHadoop--------
###Part A
###Data Loading into HDFS

# Setting up the environment variables:
Sys.setenv("HADOOP_CMD"="/usr/local/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.6.5.jar")
Sys.setenv("HADOOP_CONF"="/usr/local/hadoop/conf")

##Importing input data directly to HDFS using rhdfs package
library(rhdfs)
hdfs.init() #Initializes rhdfs package

#Creating new folder under root HDFS directory
hdfs.mkdir("/user/hduser/Chapter17/CaseStudy")
hdfs.ls("/user/hduser/Chapter17")
hdfs.put("/home/hduser/RHadoop/Dataset/HBR_Abstracts.txt", "/user/hduser/Chapter17/CaseStudy")
hdfs.ls("/user/hduser/Chapter17/CaseStudy")

#Loading the rmr2 and plyrmr packages
library(rmr2)
library(plyrmr)


#Declaring the variable names and respective data types of input data
col.classes = 
  c(Id = "character", Title = "character", Source = "character", 
    Pub_Date = "character", Volume = "character", Issue = "character", 
    Page_Count = "numeric", Doc_Type = "character", Abstract = "character")


#Defining custom input format for input data
data.format =
  make.input.format(
    "csv", 
    sep = "\t",
    quote="",
    comment.char="",
    fill=TRUE,
    col.names = names(col.classes),
    colClasses = col.classes)


#Looking at the input data object in HDFS
check<-from.dfs("/user/hduser/Chapter17/CaseStudy/HBR_Abstracts.txt",format=data.format)
mode(check) #list object
mode(check$key) #NULL object
mode(check$val) #list object
head(check$val) #Checking values under val part of input object


#Validating the dimensions of HDFS dat
input("/user/hduser/Chapter17/CaseStudy/HBR_Abstracts.txt",format=data.format) %|% dim



# Q1. Write a mapreduce program to compute the distribution of articles 
# by various levels of document_type attribute? Also, find out average 
# length of article for each of the category present in document_type attribute?

##Distribution of articles by various levels of document_type
count(input("/user/hduser/Chapter17/CaseStudy/HBR_Abstracts.txt", format = data.format), Doc_Type)

##Average length of article for each of the category present in document_type attribute
input("/user/hduser/Chapter17/CaseStudy/HBR_Abstracts.txt",format=data.format) %|%
  group(Doc_Type) %|%
  transmute(avg.length = mean(Page_Count,na.rm=T))



# Q2. Write a mapreduce program to compute word frequencies from the text present 
# in the Abstract attribute? Using the final output, plot a word cloud 
# visualization showcasing the most frequent words being used in the Abstract text.

##Defining the map function
map <- function(k,v) {
  abstracts = gsub("@\\w+", "", v$Abstract) #Removing people names
  abstracts = gsub("[[:punct:]]", "", abstracts) #Removing punctuations
  abstracts = gsub("[[:digit:]]", "", abstracts) #Removing numbers
  abstracts = gsub("http\\w+", "", abstracts) #Removing html links
  abstracts = gsub("[ \t]{2,}", "", abstracts) #Removing white spaces
  abstracts = gsub("^\\s+|\\s+$", "", abstracts) #Removing tabs
  
  words<-unlist(strsplit(abstracts,"\\s"))
  words = sapply(words, tolower) #Converting to lower case
  
  keyval(words,1)
}



##Defining the reduce function
reduce <- function(k,vv) {
  keyval(k,sum(vv))
}


##Running the mapreduce function
out<-mapreduce(input = '/user/hduser/Chapter17/CaseStudy/HBR_Abstracts.txt',
               input.format = data.format,
               map = map,
               reduce = reduce)

##Copying output to data frame object
abstract.df<-as.data.frame(from.dfs(out))
abstract.df<-abstract.df[order(-abstract.df$val),]
head(abstract.df)
dim(abstract.df)



##Removing stop words from the final word list
##Loading stop words into R working directory
con<-file("/home/hduser/RHadoop/Dataset/StopWords.txt", "r")
stopWords<-readLines(con)

abstract.df<-abstract.df[!(abstract.df$key %in% stopWords),]
dim(abstract.df)
head(abstract.df)

##Creating a wordcloud using the final output
library("wordcloud")
library("RColorBrewer")
pal2<-brewer.pal(8,"Dark2")
wordcloud(abstract.df$key,abstract.df$val,min.freq=500, random.order=T, colors=pal2)




# Q.3 Write a mapreduce program to analyze how the usage of words in article 
# abstracts has been changing over the years? For quantifying usage, please 
# consider only the top 5 highly occurring words amongst the articles for a 
# particular year. Please plot the final output representing the changing 
# trend of words over the years.


# Analyze how the usage of words in article abstracts has been changing over the years

# Removing duplicate articles
#Defining the map function
map <- function(k,v) {
  year<-substr(v$Pub_Date,1,4)
  keyval(year,v$Abstract)
}

#Defining the reduce function
reduce <- function(k,vv) {
  keyval(k, unique(vv))
}

#Running the mapreduce function
out<-mapreduce(input = "/user/hduser/Chapter17/CaseStudy/HBR_Abstracts.txt",
               input.format = data.format,
               output = "/user/hduser/CaseStudy/UniqueAbstracts",
               map = map,
               reduce = reduce)

##Reading the outputs from HDFS
UniqueAbstracts<-data.frame(from.dfs(out))
dim(UniqueAbstracts) #No of records down to 11928 from 12749
head(UniqueAbstracts)

##Splitting words from abstrcats at year level
#Defining the map function
map <- function(k,v) {
  
  abstracts = tolower(v)
  abstracts = gsub("@\\w+", "", abstracts) #Removing people names
  abstracts = gsub("[[:punct:]]", "", abstracts) #Removing punctuations
  abstracts = gsub("[[:digit:]]", "", abstracts) #Removing numbers
  abstracts = gsub("http\\w+", "", abstracts) #Removing html links
  abstracts = gsub("[ \t]{2,}", "", abstracts) #Removing white spaces
  abstracts = gsub("^\\s+|\\s+$", "", abstracts) #Removing tabs
  words<-strsplit(abstracts,"\\s")
  
  keyval(k,words)
}

#Defining the reduce function
reduce <- function(k,vv) {
  words<-unlist(vv)
  keyval(k, words)
}

#Running the mapreduce function
out<-mapreduce(input = "/user/hduser/CaseStudy/UniqueAbstracts",
               map = map,
               reduce = reduce)


##Reading the outputs from HDFS
wordList<-data.frame(from.dfs(out))
dim(wordList)
head(wordList)


##Removing stop words from the final word list
##Loading stop words into R working directory
con<-file("/home/hduser/RHadoop/Dataset/StopWords.txt", "r")
stopWords<-readLines(con)

wordList<-wordList[!(wordList$val %in% stopWords),]
dim(wordList)
colnames(wordList)<-c("Year","Word")
head(wordList)

##Getting frequency distributions at year-word level using sqldf package
install.packages("sqldf", dependencies = TRUE)
library(sqldf)
yearWordsFreq<-sqldf("select Year,Word,count(*) as Freq from wordList
                      group by Year,Word")
dim(yearWordsFreq)
head(yearWordsFreq)
# Sorting by ascending year and decreasing word Freq
yearWordsFreq<-yearWordsFreq[order(yearWordsFreq$Year,-yearWordsFreq$Freq),]

##Selecting first 5 rows across various years
#Creating seqeunce row number across year
yearWordsFreq$rowNum<-ave(yearWordsFreq$Freq, 
                          yearWordsFreq$Year, 
                          FUN = seq_along)

yearWordsFreq<-yearWordsFreq[yearWordsFreq$rowNum %in% c(1,2,3,4,5),]
dim(yearWordsFreq)
yearWordsFreq[1:50,]

##Plotting the top 5 words output across years
##For ease of visibility, we shall consider only words between 1990 and 2010 years
library(ggplot2)
yearWordsFreq$Year<-as.numeric(as.character(yearWordsFreq$Year))
yearWords<-yearWordsFreq[yearWordsFreq$Year>1989 & yearWordsFreq$Year<2011,]
ggplot(yearWords, aes(x=Year,y=Freq,fill=Word)) + geom_bar(stat ="identity")