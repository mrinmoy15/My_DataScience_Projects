# Exploratory Data Analysis using Plyrmr

# Setting up the hadoop environment variables:
Sys.setenv("HADOOP_CMD"="/usr/local/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.6.5.jar")
Sys.setenv("HADOOP_CONF"="/usr/local/hadoop/conf")


##Importing airlines data directly to HDFS using rhdfs package
library(rhdfs)
hdfs.init() #Initializes rhdfs package

#Copying Airlines.txt data from Local to HDFS path
hdfs.put('/home/hduser/RHadoop/Dataset/Airlines.txt','/user/hduser/CaseStudy/') #Copying file to HDFS
hdfs.ls('/user/hduser/CaseStudy/') #Checking the file details

#Loading the rmr2 package
library(rmr2)

#Declaring the variable names and respective data types of Airlines.txt data
col.classes = 
  c(Year = "numeric", Month = "numeric", DayofMonth = "numeric", DayofWeek = "numeric",
    DepTime = "character", CRSDepTime = "character", ArrTime = "character", CRSArrTime = "character", 
    UniqueCarrier = "character", FlightNum = "character", TailNum = "character", 
    ActualElapsedTime = "character", CRSElapsedTime = "character", AirTime = "numeric", 
    ArrDelay = "numeric", DepDelay = "numeric", Origin="character", Dest = "character", 
    Distance = "numeric", TaxiIn = "character", TaxiOut = "character", Cancelled = "numeric", 
    CancellationCode = "character", Diverted = "numeric", CarrierDelay = "numeric",
    WeatherDelay = "numeric", NASDelay = "numeric", SecurityDelay = "numeric",
    LateAircraftDelay = "numeric")

##Defining custom input format for Airlines.txt data
airline.format =
  make.input.format(
    "csv", 
    sep = ",",
    col.names = names(col.classes),
    colClasses = col.classes)


#Looking at the input data object in HDFS
check<-from.dfs("/user/hduser/CaseStudy/Airlines.txt",format=airline.format)
mode(check) #list object
mode(check$key) #NULL object
mode(check$val) #list object
head(check$val) #Checking values under val part of input object


# Q. Write a mapreduce program to compute the total number of flights distributed 
# across each day of the month. Plot the final output using ggplot2 package 
# functions. HINT: Every row is unique at a flight level. Make use of 
# "Dayof Month" variable.

daywiseFlights<-as.data.frame(count(input("/user/hduser/CaseStudy/Airlines.txt",format=airline.format),DayofMonth)) #works like table function in base R

class(daywiseFlights)
colnames(daywiseFlights)<-c("DayofMonth","No_Flights")
head(daywiseFlights)


#Visualizing the output using ggplot2 functions
library(ggplot2)
qplot(x = DayofMonth, 
      y = No_Flights, 
      data = daywiseFlights)



# Q. Write a mapreduce program to compute the total number of canceled flights 
# operated by each airliner for the year 2008 and the reason for Cancelation. 
# Plot the final output using ggplot2 package functions. HINT: Airline can be 
# taken from uniquecarrier variable. Reason for Cancelation can be taken from 
# CancellationCode variable.


canceledFlights<-as.data.frame(
  count(input("/user/hduser/CaseStudy/Airlines.txt",
              format=airline.format),
        Year:UniqueCarrier:Cancelled:CancellationCode))

class(canceledFlights)
colnames(canceledFlights)<-c("Year","UniqueCarrier","Canceled","Cancellation_Reason","No_Flights")
head(canceledFlights)

#Visualizing the output using ggplot2 functions
library(ggplot2)
ggplot(canceledFlights[canceledFlights$Canceled=="1",], aes(x=factor(UniqueCarrier),y=No_Flights,fill=Cancellation_Reason)) + geom_bar(stat ="identity")




# Q. Write a mapreduce program to identify the distribution of overall flight delay
# across various days of the month. Plot the final output using ggplot2 package 
# functions. HINT: Overall Delay is computed as sum of ArrDelay + DepDelay + 
# CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay. 
# DayofMonth variable can be used for getting day level data.



#Using rmr2 functions
#Defining the map function
map <- function(k,v) {
  key<-v$DayofMonth
  val<-rowSums(v[c("ArrDelay","DepDelay","CarrierDelay","WeatherDelay","NASDelay","SecurityDelay","LateAircraftDelay")],na.rm=T)
  keyval(key,val)
}

#Defining the reduce function
reduce <- function(k,vv) {
  keyval(k, sum(vv))
}


#Running the mapreduce function
out<-mapreduce(input = "/user/hduser/CaseStudy/Airlines.txt",
               input.format = airline.format,
               map = map,
               reduce = reduce)


#Getting the output from HDFS
overallDelay.df<-data.frame(from.dfs(out))
colnames(overallDelay.df)<-c("DayofMonth","OverallDelay")
overallDelay.df


#Using plyrmr functions
overallDelay<-as.data.frame(
  input("/user/hduser/CaseStudy/Airlines.txt",
        format=airline.format) %|%
    group(DayofMonth) %|%
    transmute(Overall_Delay = sum(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,na.rm=T)))

class(overallDelay)
colnames(overallDelay)<-c("DayofMonth","Overall_Delay")
head(overallDelay)


#Visualizing the output using ggplot2 functions
library(ggplot2)
ggplot(overallDelay, aes(x=factor(DayofMonth),y=Overall_Delay)) + geom_bar(stat ="identity")



# Q. Write a mapreduce program to compute total airtime and average distance metrics at Year,
# Month, DayofMonth and DayofWeek levels HINT: Approach this as 4 separate small problems for
# Year, Month, DayofMonth and DayofWeek respectively.


# A sample data check:
Airline_sample <- read.csv("/home/hduser/RHadoop/Dataset/Airlines.txt", header = FALSE)
colnames(Airline_sample) <- c("Year", "Month", "DayofMonth", "DayofWeek",
                              "DepTime", "CRSDepTime", "ArrTime", "CRSArrTime", 
                              "UniqueCarrier", "FlightNum", "TailNum", 
                              "ActualElapsedTime", "CRSElapsedTime", "AirTime", 
                              "ArrDelay", "DepDelay", "Origin", "Dest", 
                              "Distance", "TaxiIn", "TaxiOut", "Cancelled", 
                              "CancellationCode", "Diverted", "CarrierDelay",
                              "WeatherDelay", "NASDelay", "SecurityDelay",
                              "LateAircraftDelay")

head(Airline_sample)




# Using rmr2
map <- function(k,v){
  key = v$Month
  val = v$AirTime
  keyval(key,val)
}

reduce <- function(k,vv){
  key = k
  val = sum(vv, na.rm = TRUE)
  keyval(key,val)
}

out <- mapreduce(input = "/user/hduser/CaseStudy/Airlines.txt",
                 input.format = airline.format,
                 map =map,
                 reduce = reduce)

from.dfs(out)


# Using plyrmr
Airtime =  as.data.frame(input("/user/hduser/CaseStudy/Airlines.txt",
      format = airline.format)%>%group(Month,DayofMonth,DayofWeek)%>%transmute(TotalAirtime = sum(AirTime, na.rm = TRUE), AvgDist = mean(Distance, na.rm = TRUE)))
head(Airtime)



# Q. Write a mapreduce program to compute the total number of flights operated by each airliner for
# the year 2008. Also find out the distribution of cancelled and non-cancelled flights amongst the total
# flights for the mentioned year. And further find out the distribution of total cancelled flights at
# cancellation reason level. HINT: Airline can be taken from uniquecarrier variable

canceledFlights<-as.data.frame(
  count(input("/user/hduser/CaseStudy/Airlines.txt",
              format=airline.format),
        Year:UniqueCarrier:Cancelled:CancellationCode))



# Q. Write a mapreduce program to identify which is the busiest route of all for the year 2008. Route
# can be achieved by using a combination of “Origin” and “Dest” variables. If one row consisted of
# Origin as BWI and Dest as TPA, then this route will be a combination of BWI-TPA. Another important
# thing to consider here is, a route will be considered as a similar one irrespective of the order i.e.
# “BWI-TPA” and “TPA-BWI” should be considered as one route" HINT: Compute frequency
# distribution of routes for the year 2008


busiest_route <- as.data.frame(
  count(input("/user/hduser/CaseStudy/Airlines.txt",
              format=airline.format),
        Year:Origin:Dest))

colnames(busiest_route) <- c("Year", "Orign", "Dest", "No of Planes")
head(busiest_route)

## So the busiest root is the HOU-DAL



# Q. In 2008, which airliner recorded the highest number of diverted flights? For this airliner, compute
# the monthly averages of distance traveled by both normal and diverted flights for the year 2008?
# You should not include the cancelled flights into this analysis. HINT: Use diverted and cancelled
# filters.

head(Airline_sample)
unique(Airline_sample$UniqueCarrier)

diverted_flight_analysis <- 
  as.data.frame(input("/user/hduser/CaseStudy/Airlines.txt",
                                 format = airline.format)%>%group(UniqueCarrier)%>%transmute(Diverted = sum(Diverted, na.rm = TRUE), Cancelled = sum(Cancelled, na.rm = TRUE)))
head(diverted_flight_analysis)






