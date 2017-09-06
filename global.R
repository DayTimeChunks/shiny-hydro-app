
# To do:
# 1. include a second discharge figure that filters the data set by selected WEEK

# b) create the filter function for the checked weeks
    # b1. store the checked weeks in a list, figure out the min and max, pass them to Date-equivalents to draw x-axis ends
    # b2.
# c) create a new reactive function for the updated filter data



library("dplyr")
library("ggplot2")
library("scales")
library("ggrepel")

#library("cowplot")
#library("gridExtra")
#library("Cairo")
#library("GGally")

hydro <- read.csv2("data/groupAlteck2016_R.csv")
hydro$Date = as.POSIXct(strptime(hydro$Date, "%Y-%m-%d %H:%M", tz="CET"))
hydro$Weeks = as.factor(as.character(hydro$Weeks))

hydro = hydro[, c("Date","sampleQ", "Type", "Q.HW1", "Weeks", "SubWeeks", "WeekNo")]



AOdf = read.csv2("data/AOdataframe_R.csv")
AOdf = AOdf[, c("WeekSubWeek", "ti", "tf",
                "diss.d13C","SD.d13C", "filt.d13C", "filt.SD.d13C",        
                "Conc.mug.L", "Conc.SD", "Conc.Solids.mug.gMES",  
                "iflux", "fflux", "changeflux", "chExtreme", "tdiff" , 
                "AveDischarge.m3.h", "Volume.m3", "Sampled.Hrs", "Sampled",
                "MES.mg.L", "MES.sd", "MO.mg.L", "ExpMES.Kg", 
                "NH4.mM", "Cl.mM", "NO3...mM", "PO4..mM", "NPOC.ppm", "TIC.ppm.filt", "TIC.ppm.unfilt", "TOC.ppm.unfilt",
                "DD13C.diss", "DD13C.filt", "f.diss", "f.filt",  "B.diss", "B.filt")]

# Adding a Weeks column for labelling
AOdf$WeekSubWeek <- as.character(AOdf$WeekSubWeek)
Split <- strsplit(AOdf$WeekSubWeek, "-", fixed = TRUE)
AOdf$Weeks <- sapply(Split, "[", 1)

AOdf$WeekSubWeek <- factor(AOdf$WeekSubWeek, levels = unique(AOdf$WeekSubWeek))
AOdf$Weeks <- factor(AOdf$Weeks, levels = unique(AOdf$Weeks))
AOdf$ti <- as.POSIXct(strptime(AOdf$ti, "%Y-%m-%d %H:%M", tz="EST"))

#View(hydro)


