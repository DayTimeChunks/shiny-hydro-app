# server.R

library(ggplot2)
library(dplyr)
library(gridExtra)
#library(tidyr)
source("global.R")

# Objects from "global.R": 
# - hydro

shinyServer(function(input, output) {
  
  # Subset the data to show by date range
  hydro2 <- reactive({
    subset(hydro, Date >= input$dates[1] & Date <= input$dates[2])
  })
  
  # New
  # Get Date and Discharge values to plot as data.frame (relevant to plot with rectangles)
  hydroFlux <- reactive({ 
    data.frame(my_x_flux = hydro2()$Date, my_y_flux= hydro2()$Q.HW1)
  })
  
  # New
  # Get Date and Discharge for SAMPLES only!
  hydro3 <- reactive({
    subset(hydro2(), Type == "Sample")
  })
  hydroSamples <- reactive({ 
    data.frame(my_x_samples = hydro3()$Date, my_y_samples= hydro3()$Q.HW1)
  })
  
  # New
  # Add one dynamic rectangle
  rectangles <- reactive({
    
    if (input$week == "W0"){
      
      data.frame(xmin = as.POSIXct('2016-03-25 12:04:00'),
                 xmax = as.POSIXct('2016-03-29 06:04:00'),
                 ymin = 0.0, 
                 ymax = 5.0
      )
    } else if (input$week == "W1"){
      
      data.frame(xmin = as.POSIXct('2016-03-30 06:20:00'),
                 xmax = as.POSIXct('2016-04-05 15:07:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W2"){
      
      data.frame(xmin = as.POSIXct('2016-04-05 16:07:00'),
                 xmax = as.POSIXct('2016-04-15 00:38:50'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W3"){
      
      data.frame(xmin = as.POSIXct('2016-04-15 21:54:00'),
                 xmax = as.POSIXct('2016-04-21 09:11:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W4"){
      
      data.frame(xmin = as.POSIXct('2016-04-21 19:50:00'),
                 xmax = as.POSIXct('2016-04-26 06:38:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W5"){
      
      data.frame(xmin = as.POSIXct('2016-04-26 19:39:00'),
                 xmax = as.POSIXct('2016-05-03 12:02:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W6"){
      
      data.frame(xmin = as.POSIXct('2016-05-03 17:11:00'),
                 xmax = as.POSIXct('2016-05-13 19:05:00'),
                 ymin = 0.0, 
                 ymax = 1000.0
      )
    } else if (input$week == "W7"){
      
      data.frame(xmin = as.POSIXct('2016-05-13 19:05:00'),
                 xmax = as.POSIXct('2016-05-16 15:11:00'),
                 ymin = 0.0, 
                 ymax = 200.0
      )
    } else if (input$week == "W8"){
      
      data.frame(xmin = as.POSIXct('2016-05-18 03:56:00'),
                 xmax = as.POSIXct('2016-05-23 18:02:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W9"){
      
      data.frame(xmin = as.POSIXct('2016-05-25 04:38:00'),
                 xmax = as.POSIXct('2016-05-30 17:28:00'),
                 ymin = 0.0, 
                 ymax = 500.0
      )
    } else if (input$week == "W10"){
      
      data.frame(xmin = as.POSIXct('2016-05-31 13:25:00'),
                 xmax = as.POSIXct('2016-06-04 15:31:00'),
                 ymin = 0.0, 
                 ymax = 1000.0
      )
    } else if (input$week == "W11"){
      
      data.frame(xmin = as.POSIXct('2016-06-07 19:33:00'),
                 xmax = as.POSIXct('2016-06-14 13:06:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W12"){
      
      data.frame(xmin = as.POSIXct('2016-06-14 14:06:00'),
                 xmax = as.POSIXct('2016-06-17 11:05:00'),
                 ymin = 0.0, 
                 ymax = 1000.0
      )
    } else if (input$week == "W13"){
      
      data.frame(xmin = as.POSIXct('2016-06-22 02:04:00'),
                 xmax = as.POSIXct('2016-06-28 08:55:00'),
                 ymin = 0.0, 
                 ymax = 1000.0
      )
    } else if (input$week == "W14"){
      
      data.frame(xmin = as.POSIXct('2016-06-28 09:55:00'),
                 xmax = as.POSIXct('2016-07-04 14:41:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    } else if (input$week == "W15"){
      
      data.frame(xmin = as.POSIXct('2016-07-04 15:41:00'),
                 xmax = as.POSIXct('2016-07-12 02:42:00'),
                 ymin = 0.0, 
                 ymax = 100.0
      )
    }
  })
  
  # This reactive  function needs to filter based on weeks
  df1 <- reactive({
    #mindate <- input$year[1] # this needs to input the equivalent date for the min Week
    #maxdate <- input$year[2] # access the date respective to week input
    
    if (input$week == "W0"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-03-25 12:04:00') & Date <= as.POSIXct('2016-03-29 06:04:00'))
      
    } else if (input$week == "W1"){
      
      #hydro %>% filter(Date >= as.POSIXct('2016-03-30 06:20:00') & Date <= as.POSIXct('2016-04-01 14:55:00'))
      hydro %>% filter(Date >= as.POSIXct('2016-03-30 06:20:00') & Date <= as.POSIXct('2016-04-05 15:07:00'))
        
    } else if (input$week == "W2"){
        
      #hydro %>% filter(Date >= as.POSIXct('2016-04-05 15:07:00') & Date <= as.POSIXct('2016-04-09 00:38:50'))
      hydro %>% filter(Date >= as.POSIXct('2016-04-05 16:07:00') & Date <= as.POSIXct('2016-04-15 00:38:50'))
      
    } else if (input$week == "W3"){
        
      hydro %>% filter(Date >= as.POSIXct('2016-04-15 21:54:00') & Date <= as.POSIXct('2016-04-21 09:11:00'))
       
    } else if (input$week == "W4"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-04-21 19:50:00') & Date <= as.POSIXct('2016-04-26 06:38:00'))
      
    } else if (input$week == "W5"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-04-26 19:39:00') & Date <= as.POSIXct('2016-05-03 12:02:00'))
    
    } else if (input$week == "W6"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-05-03 17:11:00') & Date <= as.POSIXct('2016-05-13 17:05:00'))
      
    } else if (input$week == "W7"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-05-13 19:05:00') & Date <= as.POSIXct('2016-05-16 15:11:00'))
      
    } else if (input$week == "W8"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-05-18 03:56:00') & Date <= as.POSIXct('2016-05-23 18:02:00'))
      
    } else if (input$week == "W9"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-05-25 04:38:00') & Date <= as.POSIXct('2016-05-30 17:28:00'))
      
    } else if (input$week == "W10"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-05-31 13:25:00') & Date <= as.POSIXct('2016-06-04 15:31:00'))
      
    } else if (input$week == "W11"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-06-07 19:33:00') & Date <= as.POSIXct('2016-06-14 13:06:00'))
      
    } else if (input$week == "W12"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-06-14 14:06:00') & Date <= as.POSIXct('2016-06-17 11:05:00'))
      
    } else if (input$week == "W13"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-06-22 02:04:00') & Date <= as.POSIXct('2016-06-28 08:55:00'))
      
    } else if (input$week == "W14"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-06-28 09:55:00') & Date <= as.POSIXct('2016-07-04 14:41:00'))
      
    } else if (input$week == "W15"){
      
      hydro %>% filter(Date >= as.POSIXct('2016-07-04 15:41:00') & Date <= as.POSIXct('2016-07-12 02:42:00'))
      
    }
    
  })
  
  

  
  # Year-long discharge plot
  output$plot <- renderPlot({
    ggplot() +
      geom_rect(data=rectangles(), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='grey80', alpha=0.8) +
      geom_line(data=hydroFlux(),  aes(x=my_x_flux, y=my_y_flux), color="blue") +
      geom_point(data=hydroSamples(),  aes(x=my_x_samples, y=my_y_samples), color="red") +
      scale_y_continuous(trans=log_trans(), breaks=c(1,10,100,1000)) +
      #theme_bw() +
      scale_x_datetime(breaks = date_breaks("week"), labels = date_format("%d/%m")) +
      xlab("Date") +
      ylab(expression((m^3*.h^-1))) +
      theme(legend.position=c(.01, .8)) +
      theme(legend.justification = c(0,0))+
      #theme(plot.margin = unit(c(1,1,-1.2,1), "lines")) +
      theme(legend.title=element_blank()) 
      
    
  })
  
  
  # Event plot
  output$studyPlot <- renderPlot({
    ggplot(df1(), aes(Date, Q.HW1)) +
      geom_line(data=df1()[df1()$Type=="Discharge", ],  aes(color=Type, group = Type)) +
      geom_point(data=df1()[df1()$Type=="Sample",], aes(colour=SubWeeks, group=SubWeeks), size=3) +
      #geom_point(data=hydro2()[hydro2()$Weeks==input$weeks,], aes(colour=Type, group=Type)) +
      scale_colour_manual(values=c("blue", "red", "forestgreen", "orange", "purple", "green", "black")) +
      scale_y_continuous(trans=log_trans(), breaks=c(1,5, 10, 50, 100, 500, 1000)) +
      #theme_bw() +
      scale_x_datetime(breaks = date_breaks("days"), labels = date_format("%d/%m")) +
      xlab("Date") +
      ylab(expression((m^3*.h^-1))) +
      #theme(legend.position=c(1.1, .8)) +
      theme(legend.justification = c(0,0))+
      #theme(plot.margin = unit(c(1,1,-1.2,1), "lines")) +
      theme(legend.title=element_blank())
    
    
    
  })
  
  # Start of x & y axis for Scatter plot (LEFT)
  X1 <- reactive({
    
    if (input$Outcome_X1 == "delta 13C"){
      AOdf$diss.d13C
    } 
    else if (input$Outcome_X1 == "Conc") {
      AOdf$Conc.mug.L
    }
    else if (input$Outcome_X1 == "Date") {
      AOdf$ti
    }
    else if (input$Outcome_X1 == "Sample Flux Change (m3/h)") {
      AOdf$changeflux
    }
    else if (input$Outcome_X1 == "Max/Min Flux Change (m3/h)") {
      AOdf$chExtreme
    }
    else if (input$Outcome_X1 == "Volume (m3/sample period)") {
      AOdf$Volume.m3
    }
    else if (input$Outcome_X1 == "Average Discharge (m3/h)") {
      AOdf$AveDischarge.m3.h
    }
    else if (input$Outcome_X1 == "Suspended Solids (mg/L)") {
      AOdf$MES.mg.L
    }
    else if (input$Outcome_X1 == "Organic Matter (mg/L)") {
      AOdf$MO.mg.L
    }
    else if (input$Outcome_X1 == "Exported Suspended Solids (kg)") {
      AOdf$ExpMES.Kg
    }
    else if (input$Outcome_X1 == "NH4^+ (mM)") {
      AOdf$NH4.mM
    }
    else if (input$Outcome_X1 == "Cl^- (mM)") {
      AOdf$Cl.mM
    }
    else if (input$Outcome_X1 == "NO3^2- (mM)") {
      AOdf$NO3...mM
    }
    else if (input$Outcome_X1 == "PO4^+ (mM)") {
      AOdf$PO4..mM
    }
    else if (input$Outcome_X1 == "NPOC (ppm)") {
      AOdf$NPOC.ppm
    }
    else if (input$Outcome_X1 == "Inorganic Carbon (ppm) - filtered") {
      AOdf$TIC.ppm.filt
    }
    else if (input$Outcome_X1 == "Inorganic Carbon (ppm) - unfiltered") {
      AOdf$TIC.ppm.unfilt
    }
    else if (input$Outcome_X1 == "Organic Carbon (ppm) - unfiltered") {
      AOdf$TOC.ppm.unfilt
    }
    else if (input$Outcome_X1 == "Isotopic Shift Diss. (Delta delta 13C)") {
      AOdf$DD13C.diss
    }
    else if (input$Outcome_X1 == "Isotopic Shift Filt. (Delta delta 13C)") {
      AOdf$DD13C.filt
    }
    else if (input$Outcome_X1 == "Fraction Remaining Diss. (f)") {
      AOdf$f.diss
    }
    else if (input$Outcome_X1 == "Biodegradation Diss. (%)") {
      AOdf$B.diss
    }
    else if (input$Outcome_X1 == "Biodegradation Filt. (%)") {
      AOdf$B.filt
    }
    
      
  })
  
  Y1 <- reactive({
    
    if (input$Outcome_Y1 == "Average Discharge (m3/h)"){
      AOdf$AveDischarge.m3.h
    }
    else if (input$Outcome_Y1 == "Conc") {
        AOdf$Conc.mug.L
    }
    else if (input$Outcome_Y1 == "delta 13C") {
      AOdf$diss.d13C
    }
    else if (input$Outcome_Y1 == "Date") {
      AOdf$ti
    }
    else if (input$Outcome_Y1 == "Sample Flux Change (m3/h)") {
      AOdf$changeflux
    }
    else if (input$Outcome_Y1 == "Max/Min Flux Change (m3/h)") {
      AOdf$chExtreme
    }
    else if (input$Outcome_Y1 == "Volume (m3/sample period)") {
      AOdf$Volume.m3
    }
    else if (input$Outcome_Y1 == "Suspended Solids (mg/L)") {
      AOdf$MES.mg.L
    }
    else if (input$Outcome_Y1 == "Organic Matter (mg/L)") {
      AOdf$MO.mg.L
    }
    else if (input$Outcome_Y1 == "Exported Suspended Solids (kg)") {
      AOdf$ExpMES.Kg
    }
    else if (input$Outcome_Y1 == "NH4^+ (mM)") {
      AOdf$NH4.mM
    }
    else if (input$Outcome_Y1 == "Cl^- (mM)") {
      AOdf$Cl.mM
    }
    else if (input$Outcome_Y1 == "NO3^2- (mM)") {
      AOdf$NO3...mM
    }
    else if (input$Outcome_Y1 == "PO4^+ (mM)") {
      AOdf$PO4..mM
    }
    else if (input$Outcome_Y1 == "NPOC (ppm)") {
      AOdf$NPOC.ppm
    }
    else if (input$Outcome_Y1 == "Inorganic Carbon (ppm) - filtered") {
      AOdf$TIC.ppm.filt
    }
    else if (input$Outcome_Y1 == "Inorganic Carbon (ppm) - unfiltered") {
      AOdf$TIC.ppm.unfilt
    }
    else if (input$Outcome_Y1 == "Organic Carbon (ppm) - unfiltered") {
      AOdf$TOC.ppm.unfilt
    }
    else if (input$Outcome_Y1 == "Isotopic Shift Diss. (Delta delta 13C)") {
      AOdf$DD13C.diss
    }
    else if (input$Outcome_Y1 == "Isotopic Shift Filt. (Delta delta 13C)") {
      AOdf$DD13C.filt
    }
    else if (input$Outcome_Y1 == "Fraction Remaining Diss. (f)") {
      AOdf$f.diss
    }
    else if (input$Outcome_Y1 == "Biodegradation Diss. (%)") {
      AOdf$B.diss
    }
    else if (input$Outcome_Y1 == "Biodegradation Filt. (%)") {
      AOdf$B.filt
    }
    
  })
  
  # Scatter plots 
  output$scatterPlot_1 <- renderPlot({
    s1 <- ggplot(AOdf, aes(x=X1(), y=Y1(), color = Weeks, group = Weeks)) +
      geom_point(size = 2) +
      #theme(axis.text.y = element_blank()) +
      #theme(legend.title=element_blank()) +
      #stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
      #theme(legend.position="bottom") +
      #scale_y_continuous(trans=log_trans(), breaks=c(1, 3, 5, 8, 10, 30, 50, 80, 100, 300)) +
      ylab(input$Outcome_Y1) +
      xlab(input$Outcome_X1) +
      geom_text_repel(aes(label=WeekSubWeek, color = factor(Weeks)),
                      size = 3.5,
                      arrow = arrow(length = unit(0.005, 'npc'), type = "closed"),
                      force = 0.5, 
                      point.padding = unit(0.5, 'lines'), 
                      max.iter = 2e3,
                      nudge_x = .05)
      
    
    s1
  })
  
  
  # Start of x & y axis for Scatter plot (RIGHT)
  X2 <- reactive({
    
    if (input$Outcome_X2 == "delta 13C"){
      AOdf$diss.d13C
    } 
    else if (input$Outcome_X2 == "Conc") {
      AOdf$Conc.mug.L
    }
    else if (input$Outcome_X2 == "Date") {
      AOdf$ti
    }
    else if (input$Outcome_X2 == "Sample Flux Change (m3/h)") {
      AOdf$changeflux
    }
    else if (input$Outcome_X2 == "Max/Min Flux Change (m3/h)") {
      AOdf$chExtreme
    }
    else if (input$Outcome_X2 == "Volume (m3/sample period)") {
      AOdf$Volume.m3
    }
    else if (input$Outcome_X2 == "Average Discharge (m3/h)") {
      AOdf$AveDischarge.m3.h
    }
    else if (input$Outcome_X2 == "Suspended Solids (mg/L)") {
      AOdf$MES.mg.L
    }
    else if (input$Outcome_X2 == "Organic Matter (mg/L)") {
      AOdf$MO.mg.L
    }
    else if (input$Outcome_X2 == "Exported Suspended Solids (kg)") {
      AOdf$ExpMES.Kg
    }
    else if (input$Outcome_X2 == "NH4^+ (mM)") {
      AOdf$NH4.mM
    }
    else if (input$Outcome_X2 == "Cl^- (mM)") {
      AOdf$Cl.mM
    }
    else if (input$Outcome_X2 == "NO3^2- (mM)") {
      AOdf$NO3...mM
    }
    else if (input$Outcome_X2 == "PO4^+ (mM)") {
      AOdf$PO4..mM
    }
    else if (input$Outcome_X2 == "NPOC (ppm)") {
      AOdf$NPOC.ppm
    }
    else if (input$Outcome_X2 == "Inorganic Carbon (ppm) - filtered") {
      AOdf$TIC.ppm.filt
    }
    else if (input$Outcome_X2 == "Inorganic Carbon (ppm) - unfiltered") {
      AOdf$TIC.ppm.unfilt
    }
    else if (input$Outcome_X2 == "Organic Carbon (ppm) - unfiltered") {
      AOdf$TOC.ppm.unfilt
    }
    else if (input$Outcome_X2 == "Isotopic Shift Diss. (Delta delta 13C)") {
      AOdf$DD13C.diss
    }
    else if (input$Outcome_X2 == "Fraction Remaining Diss. (f)") {
      AOdf$f.diss
    }
    else if (input$Outcome_X2 == "Biodegradation Diss. (%)") {
      AOdf$B.diss
    }
    
    
    
  })
  
  Y2 <- reactive({
    
    if (input$Outcome_Y2 == "Average Discharge (m3/h)"){
      AOdf$AveDischarge.m3.h
    }
    else if (input$Outcome_Y2 == "Conc") {
      AOdf$Conc.mug.L
    }
    else if (input$Outcome_Y2 == "delta 13C") {
      AOdf$diss.d13C
    }
    else if (input$Outcome_Y2 == "Date") {
      AOdf$ti
    }
    else if (input$Outcome_Y2 == "Sample Flux Change (m3/h)") {
      AOdf$changeflux
    }
    else if (input$Outcome_Y2 == "Max/Min Flux Change (m3/h)") {
      AOdf$chExtreme
    }
    else if (input$Outcome_Y2 == "Volume (m3/sample period)") {
      AOdf$Volume.m3
    }
    else if (input$Outcome_Y2 == "Suspended Solids (mg/L)") {
      AOdf$MES.mg.L
    }
    else if (input$Outcome_Y2 == "Organic Matter (mg/L)") {
      AOdf$MO.mg.L
    }
    else if (input$Outcome_Y2 == "Exported Suspended Solids (kg)") {
      AOdf$ExpMES.Kg
    }
    else if (input$Outcome_Y2 == "NH4^+ (mM)") {
      AOdf$NH4.mM
    }
    else if (input$Outcome_Y2 == "Cl^- (mM)") {
      AOdf$Cl.mM
    }
    else if (input$Outcome_Y2 == "NO3^2- (mM)") {
      AOdf$NO3...mM
    }
    else if (input$Outcome_Y2 == "PO4^+ (mM)") {
      AOdf$PO4..mM
    }
    else if (input$Outcome_Y2 == "NPOC (ppm)") {
      AOdf$NPOC.ppm
    }
    else if (input$Outcome_Y2 == "Inorganic Carbon (ppm) - filtered") {
      AOdf$TIC.ppm.filt
    }
    else if (input$Outcome_Y2 == "Inorganic Carbon (ppm) - unfiltered") {
      AOdf$TIC.ppm.unfilt
    }
    else if (input$Outcome_Y2 == "Organic Carbon (ppm) - unfiltered") {
      AOdf$TOC.ppm.unfilt
    }
    else if (input$Outcome_Y2 == "Isotopic Shift Diss. (Delta delta 13C)") {
      AOdf$DD13C.diss
    }
    else if (input$Outcome_Y2 == "Fraction Remaining Diss. (f)") {
      AOdf$f.diss
    }
    else if (input$Outcome_Y2 == "Biodegradation Diss. (%)") {
      AOdf$B.diss
    }
    
  })
  
  
  output$scatterPlot_2 <- renderPlot({
    ggplot(AOdf, aes(x=X2(), y=Y2(), color = Weeks, group = Weeks)) +
      geom_point(size = 2) +
      #theme(axis.text.y = element_blank()) +
      #theme(legend.title=element_blank()) +
      #stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
      #theme(legend.position="bottom") +
      #scale_y_continuous(trans=log_trans(), breaks=c(1, 3, 5, 8, 10, 30, 50, 80, 100, 300)) +
      ylab(input$Outcome_Y2) +
      xlab(input$Outcome_X2) +
      geom_text_repel(aes(label=WeekSubWeek, color = factor(Weeks)),
                      size = 3.5,
                      arrow = arrow(length = unit(0.005, 'npc'), type = "closed"),
                      force = 0.5, 
                      point.padding = unit(0.5, 'lines'), 
                      max.iter = 2e3,
                      nudge_x = .05)
    
  })
  
})

