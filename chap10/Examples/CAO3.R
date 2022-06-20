## Daily ozone concentrations in Los Angeles

CAO3_daily<-read.csv("O3_2013_SiteCA060371103.csv",header=TRUE)
CAO3_daily_9033<-CAO3[515:874,]

library(stats)
ts.plot(CAO3_9033$Daily.Max.8.hour.Ozone.Concentration,
        ylab="Max Daily 8 hour Ozone Level (ppm)", 
        xlab="Day in 2013")

