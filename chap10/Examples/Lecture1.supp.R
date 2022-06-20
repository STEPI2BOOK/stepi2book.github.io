##Lecture 1 Supplementary material
##King's death data analyis.

##Read in data
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

##Create a time series object in R
kingstimeseries <- ts(kings)
 
 ##Start by plotting it - this is an essential first step
 plot.ts(kingstimeseries,xlab = "king", ylab="Age of death", main="Age at death of successive English monarchs")

##More complex data series - New York birth numbers by month of year
birthstimeseriescomponents <- decompose(birthstimeseries)

plot.ts(birthstimeseries, xlab="Year", ylab="Number of births by month", main="Number of births by Month in New York City")




##Another: monthly souvenir sales at a shop in Queensland, Australia
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirtimeseries,xlab="Month", ylab="Souvenirs sold", main="Souvenir sales by month in Queensland, Australia")

## Suggests a log transform 
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries, xlab="Month", ylab="Log souvenir sales", main="Log souvenir sales by month in Queensland, Australia")

##################Analysis 
##################Install the TTR package
library("TTR")

########Trends in King's death data
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3,xlab="King",ylab="Kings death age (yr)",main="Smoothed English monarch's death age - moving average of span 3")

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA3,xlab="King",ylab="Kings death age (yr)",main="Smoothed English monarch's death age - moving average of span 8")

########Deasonalizing New York birth data

birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted, xlab="month", ylab="Residual # births per month", main="Result of deseasonalizing the New York birth count per month series")

###########Modeling time series
###########Stationarity is usually assumed for AR and other such ############models. The celebrated Box-Jenkins approach achieves
###########a stationary series by differencing to remove regular 
###########components that cause non-stationarity, eg trends. First differences work for the
##########kng's death data
kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1,xlab="king", ylab="First differences of age at death", main="First difference plot for ages ate death of British monarchs")

###########Finding an ARIMA model: start with acf plots and these suggest either an 
##########AR(3) or MA(1) model i.e ARMA(3,0) ARMA(0,1) for first differences
acf(kingtimeseriesdiff1, lag.max=20, main="Acf plot for English Monarch's death age") # Plot a correlogram
pacf(kingtimeseriesdiff1, lag.max=20, main="Pacf plot for English Monarch's death age") # Plot a correlogram

#############Forecasting
######Precipitation via the Holt - Winters approach
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)  
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries, xlab="Year", ylab="Rainfall")

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE) 
rainseriesforecasts
plot(rainseriesforecasts, xlab="Year", ylab="Rainfall")

########Forecasting using ARIMA. Volcanic ash example
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries, xlab="Year", ylab="Volcanic dust level")

#######Plot "correlogram" and partial correlograms, then get the values
acf(volcanodustseries, lag.max=20) # plot a correlogram acf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations

pacf(volcanodustseries, lag.max=20) # plot a correlogram pacf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations


########Fitting the ARIMA(2,0,0) model
volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))

########Forecasting with the ARIMA(2,0,0) model
volcanodustseriesforecasts <- forecast.Arima(volcanodustseriesarima, h=31)





