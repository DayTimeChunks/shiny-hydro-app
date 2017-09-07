
setwd("/Users/DayTightChunks/Documents/PhD/InteractiveApps/Alteck-app/data")

library("dplyr")
library("ggplot2")
library("scales")
library("ggrepel")
library("dplyr")

AOdf = read.csv2("AOdataframe_R.csv")

AOdf$WeekSubWeek <- as.character(AOdf$WeekSubWeek)
AOdf$WeekSubWeek <- factor(AOdf$WeekSubWeek, levels = unique(AOdf$WeekSubWeek))
AOdf$Weeks <- factor(AOdf$Weeks, levels = unique(AOdf$Weeks))
AOdf$ti <- as.POSIXct(strptime(AOdf$ti, "%Y-%m-%d %H:%M", tz="CET"))

View(AOdf)

hydro <- read.csv2("groupAlteck2016_R.csv")
hydro$Date = as.POSIXct(strptime(hydro$Date, "%Y-%m-%d %H:%M", tz="CET"))
View(hydro)


""" Planning reactive rectangles..."""

te <- data.frame(xmin = as.POSIXct('2016-03-25 12:04:00'),
                 xmax = as.POSIXct('2016-03-29 06:04:00'),
                 ymin = 0.0, 
                 ymax = 5.0)

hydroFlux = data.frame(my_x_series = hydro$Date, my_y_series= hydro$Qfor)
hydroSamples <- subset(hydro, Type == "Sample")
hydroSamples <- data.frame(my_x_samples = hydroSamples$Date, my_y_samples= hydroSamples$Qfor)


# To use your reactive function "hydro2", you call it as an object, but with () at the end:
ggplot() +
  geom_rect(data=te, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='grey80', alpha=0.8) +
  geom_line(data=hydroFlux,  aes(x=my_x_series, y=my_y_series), color="blue") +
  geom_point(data=hydroSamples, aes(x=my_x_samples, y=my_y_samples), color="red")+
  #geom_point(data=hydro[hydro$Type=="Sample",], aes(colour=Type, group=Type)) +
  #geom_point(data=hydro2()[hydro2()$Weeks==input$weeks,], aes(colour=Type, group=Type)) +
  scale_colour_manual(values=c("blue", "red")) +
  scale_y_continuous(trans=log_trans(), breaks=c(1,10,100,1000)) +
  #theme_bw() +
  scale_x_datetime(breaks = date_breaks("week"), labels = date_format("%d/%m")) +
  xlab("Date") +
  ylab(expression((m^3*.h^-1))) +
  theme(legend.position=c(.01, .8)) +
  theme(legend.justification = c(0,0))+
  #theme(plot.margin = unit(c(1,1,-1.2,1), "lines")) +
  theme(legend.title=element_blank()) 
  

  
  #annotate("text", x = as.POSIXct('2016-03-25 12:04:00'), y = 5, label = "W0", size = 3.5, colour = "red")
  



# First overall plot
output$plot <- renderPlot({
  
  # To use your reactive function "hydro2", you call it as an object, but with () at the end:
  ggplot(hydro2(), aes(Date, Qfor)) +
    geom_line(data=hydro2()[hydro2()$Type=="Discharge", ],  aes(color=Type, group = Type)) +
    geom_point(data=hydro2()[hydro2()$Type=="Sample",], aes(colour=Type, group=Type)) +
    #geom_point(data=hydro2()[hydro2()$Weeks==input$weeks,], aes(colour=Type, group=Type)) +
    scale_colour_manual(values=c("blue", "red")) +
    scale_y_continuous(trans=log_trans(), breaks=c(1,10,100,1000)) +
    #theme_bw() +
    scale_x_datetime(breaks = date_breaks("week"), labels = date_format("%d/%m")) +
    xlab("Date") +
    ylab(expression((m^3*.h^-1))) +
    theme(legend.position=c(.01, .8)) +
    theme(legend.justification = c(0,0))+
    #theme(plot.margin = unit(c(1,1,-1.2,1), "lines")) +
    theme(legend.title=element_blank()) 
  #+ geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='grey80', alpha=0.8) 
  
  
  #annotate("text", x = as.POSIXct('2016-03-25 12:04:00'), y = 5, label = "W0", size = 3.5, colour = "red")
  
}) 
