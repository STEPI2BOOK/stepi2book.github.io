#######################################################
### Example 10.1. Ground level ozone concentrations ###
#######################################################

### Daily ozone concentrations in Los Angeles 
### In this example we see time series plots first 
### for daily data and then for hourly data for a single
### monitoring site in Los Angeles California
library(stats)

LA_O3_daily<-read.csv("O3_2013_LA_sites.csv",header=TRUE)
LA_O3_daily_Site060379033<-LA_O3_daily[515:874,]
ts.plot(LA_O3_daily_Site060379033$Daily.Max.8.hour.Ozone.Concentration,
        ylab="Max Daily 8hr Ozone Level (ppm)", 
        xlab="Day in 2013 at Los Angeles Site 060379033")
abline(h = 0.075, v = 0, col = "gray60")

### Hourly concentrations of ozone at the same site.
LA_O3_hourly_Site840060370113<-read.csv("O3_2013_Hourly_LA_Site840060370113.csv",header=TRUE)
ts.plot(LA_O3_hourly_Site840060370113$value,
        ylab="Hourly Ozone Concentration (ppm)", 
        xlab="Hour in 2013's ozone season")
abline(h = 0.075, v = 0, col = "gray60")


