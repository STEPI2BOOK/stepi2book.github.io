###############################################
### Example 10.14. Forecasting volcanic ash ###
###############################################

### ARIMA forecasts are made in this example 
### Data source: Hyn�d�man, R.J.  Time Series Data Library, http://data.is/TSDLdemo
### Data cover the period from 1500AD to 2000AD

### Forecasting using ARIMA. Volcanic ash example
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries, xlab="Year", ylab="Volcanic dust level")

### Plot "correlogram" and partial correlograms, then get the values
acf(volcanodustseries, lag.max=20) # plot a correlogram(acf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations

pacf(volcanodustseries, lag.max=20) # plot a correlogram(pacf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations

### Finding an ARIMA model
auto.arima(volcanodustseries,ic="aic")


### Fitting the ARIMA(2,0,0) model
volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))l

### Forecasting with the ARIMA(2,0,0) model
volcanodustseriesforecasts <- forecast.Arima(volcanodustseriesarima, h=31)

