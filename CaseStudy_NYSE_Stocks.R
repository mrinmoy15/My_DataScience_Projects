# Setting up the hadoop environment variables:
Sys.setenv("HADOOP_CMD"="/usr/local/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.6.5.jar")
Sys.setenv("HADOOP_CONF"="/usr/local/hadoop/conf")

##Importing input data directly to HDFS using rhdfs package
library(rhdfs)
hdfs.init() #Initializes rhdfs package
hdfs.mkdir("/user/hduser/Chapter14/CaseStudy")
hdfs.ls("/user/hduser/Chapter14")
hdfs.put("/home/hduser/RHadoop/Dataset/NYSE_stocks.txt", "/user/hduser/Chapter14/CaseStudy")

#Loading the rmr2 and plyrmr packages
library(rmr2)
library(plyrmr)

col.classes <- 
  c(exchange = 'character', symbol = "character", date = "character", 
    stock_price_open = "numeric", stock_price_high = "numeric",
    stock_price_low = "numeric", stock_price_close = "numeric", 
    stock_volume = "numeric", stock_price_adj_close = "numeric")

data.format =
  make.input.format(
    "csv",
    sep = ',',
    col.names = names(col.classes),
    colClasses = col.classes
  )


#Looking at the input data object in HDFS
check<-from.dfs("/user/hduser/Chapter14/CaseStudy/NYSE_stocks.txt", format = data.format)
mode(check) #list object
mode(check$key) #NULL object
mode(check$val) #list object
head(check$val) #Checking values under val part of input object


# Q1. Write a mapreduce program to compute average weekly (52 weeks) stock 
# prices for stock label "AIG" for the year 2001 and plot the output using 
# visualization. HINT: Stock Price on any given day can be taken the value of 
# stock_price_adj_close variable and year; week number can be extracted using 
# format function on the date variable. e.g. format(as.Date("2001-12-31"), "%W")

# Checking the dimension of the data
input("/user/hduser/Chapter14/CaseStudy/NYSE_stocks.txt", format=data.format) %|% dim

new_dataset <- as.data.frame(input("/user/hduser/Chapter14/CaseStudy/NYSE_stocks.txt", format=data.format) %>% 
  bind.cols(Year = format(as.Date(date), "%Y"), Week_no = format(as.Date(date), "%W")) %>% where(symbol == "AIG" & Year == "2001"))
  

avg_stock_price <- 
  as.data.frame(input(new_dataset) %>% group(Week_no) %>% transmute(Avg_stock_price = mean(stock_price_adj_close)))
 
#Visualizing the output using ggplot2 functions
library(ggplot2)
ggplot(avg_stock_price, aes(x= as.numeric(Week_no),y=Avg_stock_price)) + geom_line()




# Q2. Write a mapreduce program to identify top 10 stock labels that have the 
# highest volumes (sum) of stocks traded in the year 2001. For these labels, 
# compute the total stock volumes traded in the year 2000 and compute the 
# YOY percentage changes. 
# HINT: Use volume variable and YOY percentage changes need to be computed in 
# Base R or excel and not on RHadoop

mod_data <- as.data.frame(input("/user/hduser/Chapter14/CaseStudy/NYSE_stocks.txt", format=data.format) %>% 
                               bind.cols(Year = format(as.Date(date), "%Y"), Week_no = format(as.Date(date), "%W")))

stocks_2001 <- as.data.frame(input(mod_data) %>% where(Year == "2001") %>% 
  group(symbol) %>% transmute(stock_vol_2001 = sum(stock_volume)))
stocks_2001 <- stocks_2001[order(-stocks_2001$stock_vol_2001),]

stocks_2000 <- as.data.frame(input(mod_data) %>% where(Year == "2000") %>% 
                                   group(symbol) %>% transmute(stock_vol_2000 = sum(stock_volume)))


top_stocks_2001 <- merge(stocks_2001, stocks_2000, by = "symbol", all.x = TRUE)
head(top_stocks_2001)

top_stocks_2001 <- top_stocks_2001[order(-top_stocks_2001$stock_vol_2001), ]
head(top_stocks_2001)

top_stocks_2001 %>% mutate(YOY = (stock_vol_2001-stock_vol_2000)/stock_vol_2000)






# Q3. Write a mapreduce program to identify which stock symbol had the highest 
# fluctuation in price value on any given day? For this question, you should 
# use only 5 stock labels that are "AIG", "JNPR", "GE", "IBM", and “DIS". 
# HINT: Fluctuation in price on any given day is computed by subtracting High and 
# Low prices. Use only the absolute values for comparison

mod_data1 <- as.data.frame(input(mod_data) %>% where(symbol %in% c("AIG", "JNPR", "GE", "IBM", "DIS")))

head(mod_data1)

fluct <- as.data.frame(mod_data1 %>% mutate(fluct = abs(stock_price_high-stock_price_low)))
head(fluct)

max_fluct <- as.data.frame(fluct %>% group_by(symbol,date) %>% summarise(max_var = max(fluct)))
max_fluct <- max_fluct[order(-max_fluct$max_var), ]

head(max_fluct)




# Q4. Write a mapreduce program to compute aggregated average of daily mean value 
# of stocks. As a first step, you will try to calculate the daily mean for each line 
# using the open and close prices. Then you need to group together all of the 
# daily means for each stock symbol across multiple days and then compute the 
# aggregated average value for all stock symbols across all days.

# HINT: For map process, try to compute daily mean value and for reduce process, 
# try to find mean values of the daily means at a stock level
# daily_mean_price = (v$stock_price_open + v$stock_price_close)/2

map <- function(k,v){
  daily_avg_price <- (v$stock_price_open+ v$stock_price_close)/2
  keyval(v$symbol, daily_avg_price)
}


reduce <- function(k,vv){
  keyval(k,mean(vv))
}

out <- mapreduce(input = "/user/hduser/Chapter14/CaseStudy/NYSE_stocks.txt",
                 input.format = data.format,
                 map = map,
                 reduce = reduce
                 )

#Getting the output from HDFS
agg.avg.price <- data.frame(from.dfs(out))
colnames(agg.avg.price) <- c("symbol", "aggregated_daily_mean_price")
head(agg.avg.price)

agg.avg.price <-agg.avg.price[order(-agg.avg.price$aggregated_daily_mean_price),]
head(agg.avg.price)
