##############################################
### Decomposing the ozone series from Example 10.1
### Data source: EPA airdata site (http://www.epa.gov/airdata/)

library(stats)
LA_O3_hourly_Site840060370113<-read.csv("O3_2013_Hourly_LA_Site840060370113.csv",header=TRUE)

########Deasonalizing ozone daa
library("TTR")
imputeNA<-mean(LA_O3_hourly_Site840060370113$value,na.rm=TRUE) ##
LA_O3_hourly_Site840060370113$value[is.na(LA_O3_hourly_Site840060370113$value)]<-imputeNA
LA_O3_hourly_Site840060370113.ts<-
  ts(LA_O3_hourly_Site840060370113$value[2185:3672],frequency=24, start=c(1,1))
LA_O3_hourly_Site840060370113components <-
  decompose(LA_O3_hourly_Site840060370113.ts)
plot(LA_O3_hourly_Site840060370113components)


     

