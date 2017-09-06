setwd("/Users/DayTightChunks/Documents/PhD/InteractiveApps/Alteck-app")
hydro <- read.csv2("data/groupAlteck2016_R.csv")
str(hydro)

AOdf = read.csv2("data/AOdataframe_R.csv")
str(AOdf)


AOdf$WeekSubWeek <- as.character(AOdf$WeekSubWeek)
Split <- strsplit(AOdf$WeekSubWeek, "-", fixed = TRUE)
AOdf$Weeks <- sapply(Split, "[", 1)

AOdf$WeekSubWeek <- factor(AOdf$WeekSubWeek, levels = unique(AOdf$WeekSubWeek))
#AOdf$Weeks <- factor(AOdf$Weeks, levels = unique(AOdf$Weeks))
AOdf$ti <- as.POSIXct(strptime(AOdf$ti, "%Y-%m-%d %H:%M", tz="EST"))



