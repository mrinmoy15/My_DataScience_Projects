# loading necessary packages
library(lubridate)
library(dplyr)

# Reading the data
Flight_info <- read.csv("E:/Studies/Data_Analytics_Courses/Jigsaw_Courses/R_dataScience/T6/T6.1/Files/Fd.csv", stringsAsFactors = FALSE)
str(Flight_info)
summary(Flight_info)


Flight_info$FlightDate <- dmy(Flight_info$FlightDate)

# 1.The number of delayed flights for all weekdays
Flight_info %>% filter(delay=="delayed"&weekdays(date)!="Saturday"&weekdays(date)!="Sunday")%>%nrow()

# 2.the average distance, total distance and count for all delayed flights on Friday
Flight_info %>% filter(weekdays(date)=="Friday" & delay=="delayed") %>% 
  summarize( avg_distance = mean(distance),total_distance = sum(distance),count = length(distance))

# 3.the number of flights were on time on Week days and Weekends
Flight_info %>% 
  filter(delay=="ontime",weekdays(date)!="Saturday"&weekdays(date)!="Sunday")%>%nrow()

Flight_info%>%
  filter(delay=="ontime",weekdays(date)=="Saturday"|weekdays(date)=="Sunday")%>%nrow()

# 4.the number of flights for each destination across all weekdays
Flight_info %>% 
  filter(weekdays(date)!="Sunday"&weekdays(date)!="Saturday") %>% 
    group_by(dest)%>%summarize(n())

# 5. the number of times weather was bad across all weekdays
Flight_info %>% filter(weekdays(date)!="Sunday"&weekdays(date)!="Saturday"&weather==1)%>%nrow()
