
library("dplyr")
library("ggplot2")
library("scales")
library("ggrepel")
library(tidyr)


setwd("/Users/DayTightChunks/Documents/PhD/shiny-hydro-app")
getwd()
h <- read.csv2("data/groupAlteck2016_R.csv")
h$Date = as.POSIXct(strptime(h$Date, "%Y-%m-%d %H:%M", tz="EST"))
d <- h %>% filter(Date >= as.POSIXct('2016-03-30 00:00:00',  tz="EST") & Date < as.POSIXct('2016-04-01 00:00:00',  tz="EST"))


View(d)

w <- d$WeekNo 

if(0 %in% w){
  df1 <- hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-03-29 06:04:00'))
  } else {
  df1 <- hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-04-21 09:11:00'))
}
  
View(df1)

checkboxGroupInput(inputId  = "weeks", label = "weeks:", levels(factor(h$WeeksNo)), selected=levels(factor(h$WeeksNo)), inline = TRUE )

if(0 %in% w & !(1 %in% weeks) & !(2 %in% weeks) & !(3 %in% weeks)){
  df1 <- hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-03-29 06:04:00'))}

if(0 %in% weeks & 1 %in% weeks & 2 %in% weeks & 3 %in% weeks){
  df1 <- hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-04-21 09:11:00'))}

df1 <- reactive({
  
  #mindate <- input$year[1] # this needs to input the equivalent date for the min Week
  #maxdate <- input$year[2] # access the date respective to week input
  
  
  if (input$weeks == 0){
    
    draw.data(input$weeks) #%>%
    #filter(Season >= mindate) %>%
    #filter(Season <= maxdate) %>%
    #group_by(Season, tier) %>%
    #summarise(meanOutcomey=mean(totgoal),meanOutcomex=mean(hgoal))
    
  }
  
  else
    
    if (input$weeks == 2){
      
      draw.data(input$weeks) #%>%
      #filter(Season >= minyear) %>%
      #filter(Season <= maxyear) %>%
      #group_by(Season, tier) %>%
      #summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG, meanOutcomex=mean(hgoal))
      
    }
  
  
})


# removed from global
draw.data <- function(weeks){ # weeks are the week number selected in ui
  
  if(as.factor(0) %in% weeks & !(as.factor(1) %in% weeks) & !(as.factor(2) %in% weeks) & !(as.factor(3) %in% weeks)){
    df1 <- hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-03-29 06:04:00'))}
  
  if(as.factor(0) %in% weeks & as.factor(1) %in% weeks & as.factor(2) %in% weeks & as.factor(3) %in% weeks){
    df1 <- hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-04-21 09:11:00'))}
  
  return(df1)
}



# removed
dx <- function(hydro){ 
  
  if (TRUE){
    hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-03-29 06:04:00'))
  
  }
}

xx = hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-03-29 06:04:00'))

