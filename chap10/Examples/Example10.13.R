################################################
### Example 10.13 - Forecasting ozone levels ###
################################################

### Holt - Winters forecasts of future ozone concentration levels
### Data source: EPA airdata site (http://www.epa.gov/airdata/)
### Data for the first seven days of July 2013 data and forecasts for day 8

### Libraries
library(stats)
library("TTR")
library("forecast")

### Creating hourly ozone data file for a single week
### One night hour per day missing for instrument calibration - imputed for simplicity 
### as the average
LA_O3_hourly_Site840060370113 <- read.csv("O3_2013_Hourly_LA_Site840060370113.csv",header=TRUE)
imputeNA <- mean(LA_O3_hourly_Site840060370113$value,na.rm=TRUE) ##
LA_O3_hourly_Site840060370113$value[is.na(LA_O3_hourly_Site840060370113$value)]<-imputeNA
LA_O3_short <- cbind(LA_O3_hourly_Site840060370113$datetime,LA_O3_hourly_Site840060370113$value)
LA_O3_Jul <- LA_O3_short[1465:1632,])

### Turn this into a time series object 
level.ts <- ts(LA_O3_Jul[,2],frequency=24,start=c(0))
LA_ozoneforecasts <- HoltWinters(level.ts) 

### HoltWinters model fitting
plot(LA_ozoneforecasts, xlab="Hours - First Week -July 2013", ylab="O3 (ppm)",col.predicted=1,col="black",lty=2)

### HoltWinters 24 ahead forast on Day 8
LA_ozoneforecasts_24hr<-forecast.HoltWinters(LA_ozoneforecasts, h=24)
plot.forecast(LA_ozoneforecasts_24hr)





