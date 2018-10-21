library("dplyr")
library(tidyr)

# Rainfall 2, 6 and 12 min minute pluvio data, handling NA's as 0
# 2 min
rain2min =  read.csv2("Data/Rain2min.csv", header = T, dec = ".")
rain2min$Date <- as.POSIXct(strptime(rain2min$Date, 
                                     "%d/%m/%Y %H:%M", 
                                     tz="EST"))
names(rain2min) = c("Date", "Rain2min.mm")
sum(is.na(rain2min$Rain2min.mm))

time_step = 2
READ = F
if (READ){
  # 6 min
  rain6min =  read.csv2("Data/Rain6min.csv", header = T, dec = ".")
  rain6min$Date <- as.POSIXct(strptime(rain6min$Date, 
                                       "%d/%m/%Y %H:%M", 
                                       tz="EST"))
  names(rain6min) = c("Date", "Rain6min.mm")
  sum(is.na(rain6min$Rain6min.mm))
  
  # 12 min
  rain12min =  read.csv2("Data/Rain12min.csv", header = T, dec = ".")
  rain12min$Date <- as.POSIXct(strptime(rain12min$Date, 
                                        "%d/%m/%Y %H:%M", 
                                        tz="EST"))
  names(rain12min) = c("Date", "Rain12min.mm")
  sum(is.na(rain12min$Rain12min.mm))
  
  #h <- merge(h, rain12min, by = "Date", all = T)
  #h$Rain12min.mm <- ifelse(is.na(h$Rain12min.mm), 0, h$Rain12min.mm)
  # Choose Time step
  time_step = 2
  if (time_step == 2){
    h_temp = rain2min[, c("Date", "Rain2min.mm")]
  } else if (time_step == 6) {
    h_temp = rain6min[, c("Date", "Rain6min.mm")]
  } else {
    h_temp = rain12min[, c("Date", "Rain12min.mm")]
  }
}

h_temp = rain2min[, c("Date", "Rain2min.mm")]

names(h_temp) = c("Date", "Rain.mm")