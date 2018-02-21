# Exploratory Data Analysis using Plyrmr

# Setting up the hadoop environment variables:
Sys.setenv("HADOOP_CMD"="/usr/local/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.6.5.jar")
Sys.setenv("HADOOP_CONF"="/usr/local/hadoop/conf")

##Importing input data directly to HDFS using rhdfs package
library(rhdfs)
hdfs.init() #Initializes rhdfs package

#Creating new folder under root HDFS directory
#You should be using your own HDFS path based on username
hdfs.mkdir('/user/hduser/CaseStudy') 
hdfs.put('/home/hduser/RHadoop/Dataset/Home_Sales.txt','/user/hduser/CaseStudy') #Copying file to HDFS
hdfs.ls('/user/hduser/CaseStudy/') #Checking the file details

#Loading the rmr2 and plyrmr packages
library(rmr2)
library(plyrmr)


col.classes = 
  c(city = "character", zip = "numeric", 
    date = "character", price = "numeric", 
    bedrooms = "numeric", squarefeet = "numeric", 
    lotsize = "numeric", builtyear = "numeric", 
    neighbourhood = "character")


#Defining custom input format for input data
data.format =
  make.input.format(
    "csv", 
    sep = ",",
    col.names = names(col.classes),
    colClasses = col.classes)



#Looking at the input data object in HDFS
check<-from.dfs("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format)
mode(check) #list object
mode(check$key) #NULL object
mode(check$val) #list object
head(check$val) #Checking values under val part of input object


## Q1. 1. Write a mapreduce program to compute number of rows and columns present 
## in the given dataset. Additionally, find out the distribution of homes present 
## across different neighborhoods present in the dataset.

# Using plyrmr
input("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format) %|% dim
input("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format) %|% nrow
input("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format) %|% ncol

# using rmr2
#using rmr2
map <- function(k,v) {
  keyval(ncol(v), nrow(v))
}

reduce <- function(k,v) {
  keyval(k, sum(v))
}

out<-mapreduce(input = "/user/hduser/CaseStudy/Home_Sales.txt",
               input.format = data.format,
               map = map,
               reduce = reduce)
from.dfs(out)


##Find out the distribution of homes present across different neighborhoods
count(input("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format),neighbourhood) #works like table function in base R


# Q2. Write a mapreduce program to compute quantile values of all the numeric 
# variables present in the dataset like Price, Bedroom, Squarefeet and Lotsize.
# Looking at the quantile outputs, please provide comments on the present outlier 
# values if any.

##Compute quantile values of all the numeric variables present in the dataset like Price, Bedroom, Squarefeet and Lotsize
select(input("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format), price, bedrooms, squarefeet, lotsize) %|% quantile

##Looking at the final output, we can conclude all the variables
##seems to have extreme values present. This needs to be further
##validated from the business whether presence of such variables
##is expected in the data


# Q3. Write a mapreduce program to identify the number of missing values present 
# across all the variables in the dataset. Additionally, please perform a missing 
# value treatment on the “neighbourhood” variable if you notice any missing values. 
# For the missing value treatment, note that plyrmr package functions are not 
# supported and you need to write a custom mapreduce program using rmr2 syntax.


##Identify the number of missing values present across all the variables in the dataset
missing = function(x) colSums(is.na(x))

gapply(input("/user/hduser/CaseStudy/Home_Sales.txt",format=data.format), 
       missing)


##Perform a missing value treatment on the neighbourhood variable 
##mapreduce code
#Defining the map function
#Identify missing row values in neighbourhood and replace them with highest 
# occuring category i.e South of Market


map <- function(k,v) {
  key<-1:nrow(v)
  keyval(key, v)
}

#Defining the reduce function
reduce <- function(k,vv) {
  vv[is.na(vv$neighbourhood),"neighbourhood"]<-"South of Market"
  keyval(k, vv)
}



#Running the mapreduce function
out<-mapreduce(input = "/user/hduser/CaseStudy/Home_Sales.txt",
               input.format = data.format,
               map = map,
               reduce = reduce)


##Reading the outputs from HDFS
homeSalesNew<-data.frame(from.dfs(out))
colnames(homeSalesNew) = 
  c("sno","city","zip","date","price","bedrooms",
    "squarefeet","lotsize","builtyear","neighbourhood")
head(homeSalesNew)
colSums(is.na(homeSalesNew))
