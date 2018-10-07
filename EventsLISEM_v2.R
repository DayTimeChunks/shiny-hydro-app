# LISEM Rainfall selection

library("dplyr")
library("ggplot2")
library("scales")
library("ggrepel")
library(tidyr)

setwd("/Users/DayTightChunks/Documents/PhD/shiny-hydro-app")

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

day = 225
bbreak = F
if (day == 183){
  # FIRST EVENT 
  start_lisem = '2016-03-30 21:18:00'
  beach_break = '2016-03-31 00:00:00'
  end_lisem = '2016-03-31 00:40:00'
} else if (day == 188) { # April 5
  #start_lisem = '2016-04-05 03:00:00' # # v1
  start_lisem = '2016-04-05 16:10:00'
  end_lisem = '2016-04-05 21:00:00'
# } else if (day == 200) { # April 17 # v1
} else if (day == 199) { # April 16
  # April 16 & 17, long day with continous intensity
  start_lisem = '2016-04-16 07:12:00'
  # beach_break = '2016-04-17 00:00:00' # v1
  # end_lisem = '2016-04-17 03:02:00' # v1
  end_lisem = '2016-04-16 13:02:00'
} else if (day == 214) {
  # April 30 & May 1st
  start_lisem = '2016-04-30 18:00:00'
  beach_break = '2016-05-01 00:00:00'
  end_lisem = '2016-05-01 04:24:00'
} else if (day == 225) { # First Likely runoff event on the 12th!
  # May 11 & 12
  # start_lisem = '2016-05-11 18:00:00'
  #beach_break = '2016-05-12 00:00:00'
  #end_lisem = '2016-05-12 10:00:00'
  start_lisem = '2016-05-12 06:00:00'
  end_lisem = '2016-05-12 10:00:00'
}

if (bbreak) {
  head_event = h_temp %>% filter(Date >= as.POSIXct(start_lisem,  tz="EST") & Date < as.POSIXct(beach_break,  tz="EST"))
  tail_event = h_temp %>% filter(Date >= as.POSIXct(beach_break,  tz="EST") & Date <= as.POSIXct(end_lisem,  tz="EST"))
  
  # LISEM rain (head event), subtract from BEACH TSS
  print(cumsum(head_event$Rain.mm)[length(head_event$Rain.mm)])
  # LISEM rain (tail event), subtract from BEACH TSS
  print(cumsum(tail_event$Rain.mm)[length(tail_event$Rain.mm)])
  
} else {
  event = h_temp %>% filter(Date >= as.POSIXct(start_lisem,  tz="EST") & Date < as.POSIXct(end_lisem,  tz="EST"))
  
  # LISEM rain (event), subtract from BEACH TSS
  cumsum(event$Rain.mm)[length(event$Rain.mm)]
}

x1 <- h_temp %>% filter(Date >= as.POSIXct(start_lisem,  tz="EST") & Date <= as.POSIXct(end_lisem,  tz="EST"))
x1$tmp = time_step
x1$tmp[1]=0
x1$time = cumsum(x1$tmp)
x1$rain = x1$Rain.mm * 60
x1 = x1[, c("time", "rain")] # Minutes, mm/hr

name = paste("Event_", as.character(day), ".txt", sep = "")
write.table(x1, name, sep = "\t", row.names = FALSE, col.names = TRUE)


# SECOND EVENT 
# x2 <- h %>% filter(Date >= as.POSIXct('2016-04-16 00:00:00',  tz="EST") & Date < as.POSIXct('2016-04-18 00:00:00',  tz="EST"))

# THIRD EVENT 
# x3 <- h %>% filter(Date >= as.POSIXct('2016-05-29 00:00:00',  tz="EST") & Date < as.POSIXct('2016-05-31 00:00:00',  tz="EST"))




###########################################################################
# Discharge

# Full Date
dischargeAlteck = read.csv2("Data/hydroAlteck2016_smooth_R.csv")
head(dischargeAlteck)

dischargeAlteck$Date = as.POSIXct(strptime(dischargeAlteck$DateCheck, 
                                           "%d/%m/%Y %H:%M"
                                           , tz="EST")
)
# Date as Day
dischargeAlteck$DayMoYr = as.POSIXct(strptime(dischargeAlteck$DateCheck, 
                                              "%d/%m/%Y"
                                              , tz="EST")
)
# sum(is.na(dischargeAlteck$Date))
## Convert m3.h -> m3
dischargeAlteck$Vol2min <- dischargeAlteck$Q.HW1*2/60