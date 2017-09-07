library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
#library(tidyr)
library(shinydashboard)

# In case we want toonly render data upon action button, see:
# https://shiny.rstudio.com/gallery/widgets.html

#shinyUI(fluidPage(
#shinyUI(
dashboardPage(
  dashboardHeader(title = "Interactive Data"),
  dashboardSidebar(disable = T), 
  dashboardBody(
    titlePanel("CSIA - Compound Specific Isotope Analysis of Pesticides at the Catchment Scale"),
    h3("Innovative Tools for Tracking Micropollutants in Water Resources"),
    tabsetPanel( # Master tabsetPanel
      tabPanel("Hydrology",
               tabsetPanel( # myID = Hydrology
                 tabPanel("Event Selection",
                          h3("Select event:"),
                          p("Test"),
                          # Create a row for additional information
                          fluidRow(
                            # Take up 2/3 of the width with this element  
                            column(2, "Start: ", verbatimTextOutput("ev_time1")),
                            column(2, "End: ", verbatimTextOutput("ev_time2")),
                            column(2, "Duration (hrs.): ", verbatimTextOutput("ev_duration")),
                            # And the remaining 1/3 with this one
                            column(2, " ", actionButton("calc_duration", "Calculate") ),
                            column(2, " ", actionButton("record_duration", "Keep")),
                            column(2, " ", actionButton("clear", "Clear Points")),
                            br(),
                            br()
                          ),    
                          wellPanel( # myId =  Date Selection
                            fluidRow(column(10, offset = 1, sliderInput(inputId="dates_ev", label="Dates", 
                                                                        min = as.POSIXct('2016-03-25 00:00:00'), max = as.POSIXct('2016-07-12 10:20:00'),
                                                                        value = c(as.POSIXct('2016-03-25 00:00:00'), as.POSIXct('2016-07-12 10:20:00'))
                                                                        )))), # close wellPanel, myId =  Date Selection
                                   wellPanel(
                                     fluidRow(column(12, plotOutput("rain_plot_ev", click = "plot_click_P")))),
                                   wellPanel(
                                     fluidRow(column(12, plotOutput("plot_ev", click = "plot_click_Q"))))
                 ),
                 tabPanel("Sample Selection", h3("Full season"),
                          wellPanel( # myId =  Date Selection
                            fluidRow(column(10, offset = 1, sliderInput(inputId="dates", label="Dates", 
                                                                        min = as.POSIXct('2016-03-25 00:00:00'), 
                                                                        max = as.POSIXct('2016-07-12 10:20:00'),
                                                                        value = c(as.POSIXct('2016-03-25 00:00:00'), as.POSIXct('2016-07-12 10:20:00'))
                            )))), # close wellPanel, myId =  Date Selection
                          wellPanel(
                            fluidRow(column(12, plotOutput("rain_plot")))),
                          wellPanel(
                            fluidRow(column(12, plotOutput("plot")))),
                          
                          h3("Sampled Event"),
                          wellPanel(fluidRow(
                            column(10, plotOutput("studyPlot")),
                            column(2, selectInput(inputId="week", label = "Week",
                                                  list("Week 0" = "W0", "Week 1" = "W1", "Week 2" = "W2", "Week 3" = "W3", "Week 4" = "W4",
                                                       "Week 5" = "W5", "Week 6" = "W6", "Week 7" = "W7", "Week 8" = "W8", "Week 9" = "W9",
                                                       "Week 10" = "W10", "Week 11" = "W11", "Week 12" = "W12", "Week 13" = "W13", "Week 14" = "W14",
                                                       "Week 15" = "W15"), 
                                                  selected="W6"))
                          )
                          )
                 ), 
                 tabPanel("PCA", 
                          h3("Add clustering..."))
               )), # myID = close Hydrology
      # Top tab panel 2
      tabPanel("Hydrochemistry", 
               h3("Scatter Plots"),
               wellPanel(fluidRow(
                 column(6, selectInput("Outcome_Y1", label = "Y-Axis (Left):",
                                    list("delta 13C" = "delta 13C",
                                         "Concentration" = "Conc",
                                         "Sample Flux Change (m3/h)" = "Sample Flux Change (m3/h)",
                                         "Max/Min Flux Change (m3/h)" = "Max/Min Flux Change (m3/h)",
                                         "Average Discharge (m3/h)" = "Average Discharge (m3/h)",
                                         "Volume Discharged (m3/sample period)" = "Volume (m3/sample period)",
                                         "Suspended Solids (mg/L)" = "Suspended Solids (mg/L)",
                                         "Organic Matter (mg/L)" = "Organic Matter (mg/L)",
                                         "Exported Solids (Kg/sample period)" = "Exported Suspended Solids (kg)",
                                         "NH4^+ (mM)" = "NH4^+ (mM)",
                                         "Cl^- (mM)" = "Cl^- (mM)",
                                         "NO3^2- (mM)" = "NO3^2- (mM)",
                                         "PO4^+ (mM)" = "PO4^+ (mM)",
                                         "NPOC (ppm)" = "NPOC (ppm)",
                                         "Inorganic Carbon (ppm) - filtered" = "Inorganic Carbon (ppm) - filtered",
                                         "Inorganic Carbon (ppm) - unfiltered" = "Inorganic Carbon (ppm) - unfiltered",
                                         "Organic Carbon (ppm) - unfiltered" = "Organic Carbon (ppm) - unfiltered",
                                         "Isotopic Shift (Delta delta 13C)" = "Isotopic Shift (Delta delta 13C)",
                                         "Fraction Remaining (f)" = "Fraction Remaining (f)",
                                         "Biodegradation (%)" = "Biodegradation (%)"),
                                    selected= "Average Discharge (m3/h)"),
                        selectInput("Outcome_X1", label = "X-Axis (Left):",
                                    list("Week" = "WeekSubWeek",
                                         "Date" = "Date",
                                         "delta 13C" = "delta 13C",
                                         "Concentration" = "Conc",
                                         "Sample Flux Change (m3/h)" = "Sample Flux Change (m3/h)",
                                         "Max/Min Flux Change (m3/h)" = "Max/Min Flux Change (m3/h)",
                                         "Average Discharge (m3/h)" = "Average Discharge (m3/h)",
                                         "Volume Discharged (m3/sample period)" = "Volume (m3/sample period)",
                                         "Suspended Solids (mg/L)" = "Suspended Solids (mg/L)",
                                         "Organic Matter (mg/L)" = "Organic Matter (mg/L)",
                                         "Exported Solids (Kg/sample period)" = "Exported Suspended Solids (kg)",
                                         "NH4^+ (mM)" = "NH4^+ (mM)",
                                         "Cl^- (mM)" = "Cl^- (mM)",
                                         "NO3^2- (mM)" = "NO3^2- (mM)",
                                         "PO4^+ (mM)" = "PO4^+ (mM)",
                                         "NPOC (ppm)" = "NPOC (ppm)",
                                         "Inorganic Carbon (ppm) - filtered" = "Inorganic Carbon (ppm) - filtered",
                                         "Inorganic Carbon (ppm) - unfiltered" = "Inorganic Carbon (ppm) - unfiltered",
                                         "Organic Carbon (ppm) - unfiltered" = "Organic Carbon (ppm) - unfiltered",
                                         "Isotopic Shift (Delta delta 13C)" = "Isotopic Shift (Delta delta 13C)",
                                         "Fraction Remaining (f)" = "Fraction Remaining (f)",
                                         "Biodegradation (%)" = "Biodegradation (%)"), 
                                    selected= "Conc")
                 ),
                 column(6, selectInput("Outcome_Y2", label = "Y-Axis (Right):",
                                    list("delta 13C" = "delta 13C",
                                         "Concentration" = "Conc",
                                         "Sample Flux Change (m3/h)" = "Sample Flux Change (m3/h)",
                                         "Max/Min Flux Change (m3/h)" = "Max/Min Flux Change (m3/h)",
                                         "Average Discharge (m3/h)" = "Average Discharge (m3/h)",
                                         "Volume Discharged (m3/sample period)" = "Volume (m3/sample period)",
                                         "Suspended Solids (mg/L)" = "Suspended Solids (mg/L)",
                                         "Organic Matter (mg/L)" = "Organic Matter (mg/L)",
                                         "Exported Solids (Kg/sample period)" = "Exported Suspended Solids (kg)",
                                         "NH4^+ (mM)" = "NH4^+ (mM)",
                                         "Cl^- (mM)" = "Cl^- (mM)",
                                         "NO3^2- (mM)" = "NO3^2- (mM)",
                                         "PO4^+ (mM)" = "PO4^+ (mM)",
                                         "NPOC (ppm)" = "NPOC (ppm)",
                                         "Inorganic Carbon (ppm) - filtered" = "Inorganic Carbon (ppm) - filtered",
                                         "Inorganic Carbon (ppm) - unfiltered" = "Inorganic Carbon (ppm) - unfiltered",
                                         "Organic Carbon (ppm) - unfiltered" = "Organic Carbon (ppm) - unfiltered",
                                         "Isotopic Shift (Delta delta 13C)" = "Isotopic Shift (Delta delta 13C)",
                                         "Fraction Remaining (f)" = "Fraction Remaining (f)",
                                         "Biodegradation (%)" = "Biodegradation (%)"),
                                    selected="Average Discharge (m3/h)"),
                        selectInput("Outcome_X2", label = "X-Axis (Right):",
                                    list("Week" = "WeekSubWeek",
                                         "Date" = "Date",
                                         "delta 13C" = "delta 13C",
                                         "Concentration" = "Conc",
                                         "Sample Flux Change (m3/h)" = "Sample Flux Change (m3/h)",
                                         "Max/Min Flux Change (m3/h)" = "Max/Min Flux Change (m3/h)",
                                         "Average Discharge (m3/h)" = "Average Discharge (m3/h)",
                                         "Volume Discharged (m3/sample period)" = "Volume (m3/sample period)",
                                         "Suspended Solids (mg/L)" = "Suspended Solids (mg/L)",
                                         "Organic Matter (mg/L)" = "Organic Matter (mg/L)",
                                         "Exported Solids (Kg/sample period)" = "Exported Suspended Solids (kg)",
                                         "NH4^+ (mM)" = "NH4^+ (mM)",
                                         "Cl^- (mM)" = "Cl^- (mM)",
                                         "NO3^2- (mM)" = "NO3^2- (mM)",
                                         "PO4^+ (mM)" = "PO4^+ (mM)",
                                         "NPOC (ppm)" = "NPOC (ppm)",
                                         "Inorganic Carbon (ppm) - filtered" = "Inorganic Carbon (ppm) - filtered",
                                         "Inorganic Carbon (ppm) - unfiltered" = "Inorganic Carbon (ppm) - unfiltered",
                                         "Organic Carbon (ppm) - unfiltered" = "Organic Carbon (ppm) - unfiltered",
                                         "Isotopic Shift (Delta delta 13C)" = "Isotopic Shift (Delta delta 13C)",
                                         "Fraction Remaining (f)" = "Fraction Remaining (f)",
                                         "Biodegradation (%)" = "Biodegradation (%)"),
                                    selected="delta 13C")
                 ),
                 br(),
                 fluidRow(
                   column(6, plotOutput("scatterPlot_1")),
                   column(6, plotOutput("scatterPlot_2")))
               )
               )), # myID = close Hydrochemistry
      # tab panel 3
      tabPanel("Soils", 
               h3("Add composite and detailed soils..."))
      ) # close # Master tabsetPanel
    ) # close "dashboardBody"
  ) # close "dashboardPage"
