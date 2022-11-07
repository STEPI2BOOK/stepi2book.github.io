# Setting directory
setwd("C:/Users/Camila/Desktop/PhD Research/Temperature Paper/Computing")

# Libraries
library(geoR)

# Reading data
data = read.table("MURIdata.txt", header = T)

# Lambert Projection
source("lambert.r")

# Coordinates Lambert Projection
MURILambCoords = Flamb2(abs(data[,c("lat","lom")]))

# Adding them to the data file
data$lambx = MURILambCoords$xy[,1]/100 # note transformation in the coordinates!
data$lamby = MURILambCoords$xy[,2]/100

# Observations in Celsius
data$obsCelsius = with(data, obs-273.15)

# Creating month variable
data$month = with(data, as.integer(substr(as.character(initdate/100), start = 5, stop = 6)))

# Fixing elevation variable
data$elevTrunc = with(data, ifelse(elev < 0, 0, elev))

################################################################################
# SPATIAL LM 

# Unique dates
uniqueDates = sort(unique(data$initdate))
totalDays = length(uniqueDates)

# Times
times = sort(unique(data$time))

# Latitude and Longitude Effects
latEffect  = array(NA, dim = totalDays)
longEffect = array(NA, dim = totalDays)
# Standard Errors
latStdError = array(NA, dim = totalDays)
longStdError = array(NA, dim = totalDays)
# CIs for Latitude and Longitude Effects
upperLat = array(NA, dim = totalDays)
lowerLat = array(NA, dim = totalDays)
upperLong = array(NA, dim = totalDays)
lowerLong = array(NA, dim = totalDays)
# Residuals
SpatialResiduals = list()

# Spatial Linear Model (2nd order polynomial)
for (i in 1:totalDays){
  dataForSelectedDay = subset(data, initdate == uniqueDates[i])
  lmfit = lm(obsCelsius ~ lambx*lamby + I(lambx^2) + I(lamby^2), data = dataForSelectedDay)
  # Latitude and Longitude Effects
  latEffect[i]  = as.numeric(lmfit$coeff[c("lamby")])
  longEffect[i] = as.numeric(lmfit$coeff[c("lambx")])
  # Standard Errors
  latStdError[i]  = coef(summary(lmfit))[,c("Std. Error")][c("lamby")]
  longStdError[i] = coef(summary(lmfit))[,c("Std. Error")][c("lambx")]
  # CIs for Latitude and Longitude Effects
  upperLat[i]  =  latEffect[i]  + 2*latStdError[i]
  lowerLat[i]  =  latEffect[i]  - 2*latStdError[i]
  upperLong[i] =  longEffect[i] + 2*longStdError[i]
  lowerLong[i] =  longEffect[i] - 2*longStdError[i]
  # Residuals
  SpatialResiduals[[i]] = lmfit$res
}

################################################################################
# PLOT : Latitude and Longitude Effects

# Latitude effect over time
pdf("LatLongEffects.pdf")
par(mfrow = c(1,2))
plot(times, latEffect, pch = 16, xlab = "Time", ylab = "Latitude Effect", type = "n", lwd = 2, ylim = c(-.025,.025), cex.lab = 1.6)
polygon(c(times,rev(times)),c(lowerLat,rev(upperLat)), border = gray(0.6), col = gray(0.6))
lines(times, latEffect, pch = 16, lwd = 2)
lo = loess(latEffect~times)
xl = seq(min(times), max(times), (max(times) - min(times))/1000)
lines(xl, predict(lo, xl), col = 2, lwd = 2)

plot(times, longEffect, pch = 16, xlab = "Time", ylab = "Longitude Effect", col = "gray", type = "n", lwd = 2,ylim = c(-.025,.025), cex.lab = 1.6)
polygon(c(times,rev(times)),c(lowerLong,rev(upperLong)), border = gray(0.6), col = gray(0.6))
lines(times, longEffect, pch = 16, lwd = 2)
lo2 = loess(longEffect~times)
lines(xl, predict(lo2, xl), col = 2, lwd = 2)
dev.off()

# Using ggplot

library(ggplot2)
library(gridExtra)

SpatialEffects = data.frame(times,latEffect,longEffect,lowerLat,upperLat,lowerLong,upperLong)

latgg <- ggplot(SpatialEffects, aes(x = times, y = latEffect)) + geom_line() +
   geom_ribbon(aes(ymin=lowerLat, ymax=upperLat), alpha=0.2, fill = "red") + xlab("Time (Days)") + ylab("Latitude Effect") + 
   ylim(c(-.025,.025)) +  theme(text = element_text(size=14))

longg <- ggplot(SpatialEffects, aes(x = times, y = longEffect)) + geom_line() +
   geom_ribbon(aes(ymin=lowerLong, ymax=upperLong), alpha=0.2, fill = "red") + xlab("Time (Days)") + ylab("Longitude Effect") + 
   ylim(c(-.025,.025))+ theme(text = element_text(size=14))
   
   grid.arrange(latgg, longg, nrow = 1)

################################################################################
# PLOT: Variograms

# Select Dates
selectedDates = c(2000020800, 2000061800)
#selectedDates = uniqueDates

for (i in 1:length(selectedDates)){
  dataForSelectedDay = subset(data, initdate == selectedDates[i])
  # Creating geodata object
  geodata = as.geodata(dataForSelectedDay, coords.col = c("lambx", "lamby"), data.col = c("obsCelsius"))
  # Variogram
  variogb = variog(geodata, option = c("bin"), trend = "2nd")
  variogc = variog(geodata, option = c("cloud"), trend = "2nd")
  # Plot Cloud Variogram
  #pdf(paste("VariogCloudDate", selectedDates[i], ".pdf", sep = ""))
  #plot(variogc, xlab = "Distance (km)", ylab = "Semivariances", pch = 16, col = "gray", ylim = c(0,410))
  #dev.off()
  # Plot Binned Variogram
  #pdf(paste("VariogBinnedDate", selectedDates[i], ".pdf", sep = ""))
  #jpeg(filename = paste("VariogBinnedDate", selectedDates[i], ".jpg", sep = ""))
  #plot(variogb, xlab = "Distance (km)", ylab = "Semivariances", pch = 16, col = "gray", ylim = c(0,20))
  #dev.off()
  # Envelopes
  variogEnv = variog.mc.env(geodata, obj.variog = variogb)
  pdf(paste("VariogBinnedwithEnvelopeDate", selectedDates[i], ".pdf", sep = ""))
  plot(variogb, xlab = "Distance (km)", ylab = "Semivariances", pch = 16, ylim = c(0,max(variogEnv$v.upper)), type = "n")
  with(variogEnv, polygon(c(u,rev(u)),c(v.lower,rev(v.upper)), border = gray(0.6), col = gray(0.6)))
  with(variogb, points(u, v, pch = 16, type = "b"))
  dev.off()
  # Plot Directional Variogram
  #variogdir = variog4(geodata)
  #pdf(paste("VariogDirectionalDate", selectedDates[i], ".pdf", sep = ""))
  #plot(variogdir$'0'$u,  variogdir$'0'$v, type = "b", pch = 16, xlab = "Distances (km)",
  #  ylab = "Semivariances", ylim = c(0,275), xlim = c(0, 1510), main = "")
  #lines(variogdir$'45'$u,  variogdir$'45'$v, type = "b", pch = 16, lty = 2, col = gray(0.5))
  #lines(variogdir$'90'$u,  variogdir$'90'$v, type = "b", pch = 16, lty = 3, col = gray(0.7))
  #lines(variogdir$'135'$u, variogdir$'135'$v, type = "b", pch = 16, lty = 4, col = gray(0.8))
  #legend("topleft", c(expression(paste("0",degree)), expression(paste("45",degree)),
  #  expression(paste("90",degree)), expression(paste("135",degree))),
  #  pch = c(16), lty = 1:4, col = c(1,gray(0.5), gray(0.7),gray(0.8)))
  #dev.off()
  
}

dataDay1 = data.frame(u = variogb$u, v = variogb$v, v.lower = variogEnv$v.lower, v.upper = variogEnv$v.upper)
dataDay2 = data.frame(u = variogb$u, v = variogb$v, v.lower = variogEnv$v.lower, v.upper = variogEnv$v.upper)


day1 <- ggplot(dataDay1, aes(x = u, y = v)) + geom_line() +
   geom_ribbon(aes(ymin=v.lower, ymax=v.upper), alpha=0.2, fill = "red") + xlab("Distance (km)") + ylab("Semivariances") +
   ggtitle("February 08")  + theme(text = element_text(size=14))

day2 <- ggplot(dataDay2, aes(x = u, y = v)) + geom_line() +
   geom_ribbon(aes(ymin=v.lower, ymax=v.upper), alpha=0.2, fill = "red") + xlab("Distance (km)") + ylab("Semivariances")  +
   ggtitle("June 18") + theme(text = element_text(size=14))
   
   
   grid.arrange(day1, day2, nrow = 1)

################################################################################
# PLOT: ACF PACF 

# Select Dates
selectedDates = c(2000020800, 2000061800)

for (i in 1:length(selectedDates)){
  acf(SpatialResiduals[[which(uniqueDates == selectedDates[i])]], main = "")
  pacf(SpatialResiduals[[which(uniqueDates == selectedDates[i])]], main = "")
  }
  
################################################################################
# PLOT: Data structure (Unbalancedness)

times = sort(unique(data$time))
totalTime = max(unique(data$time))
nstations = dim(unique(data[,c("lat","lom")]))[1]
stationLabels  = sort(unique(data$labels))

matriz = table(data$time,data$labels) 
matrizComplete = matrix(0, nrow = totalTime, ncol = nstations)

# Indexes of matrizComplete are not equivalent to the labels. 
# For example, the 833 row is equivalent to the 833th label, which is not 833, but 1090
for (j in 1:nstations){
  for (i in 1:totalTime){
    if(i %in% times) matrizComplete[i,j] = matriz[which(times == i),j] 
  } 
}

  # Basic Plot
  Days <- seq(1,totalTime); Stations <- seq(1,nstations)
  #pdf("unbalanced.pdf")
  image(Days, Stations, matrizComplete, col = grey(c(1,0)))
  box()
  #dev.off()
  
  # GGplot
  library(reshape2)
  library(ggplot2)
  m = melt(matrizComplete)
  m$value = with(m, ifelse(value == 1, "Available", "Not Available"))
  
  unbalanced = ggplot(m, aes(Var1, Var2, fill = value)) + 
    geom_raster() + scale_fill_manual(values=c("black", "white")) + 
    guides(fill=guide_legend(title=c("Data"))) +
    theme(legend.key = element_rect(colour = "black", size = 1.2)) +
    xlab("Days") + ylab("Stations") + theme(text = element_text(size = 16))
  ggsave(unbalanced, file="C:/Users/Camila/Desktop/Thesis/Figures/unbalancedMURI.png", height = 6, width = 8)

################################################################################
# PLOT: Map of the stations
 
library(ggmap)
# Bounding Box for Pacific Northwerstern area
location = c(-132, 37, -110, 54)
# Getting Map
map = get_map(location = location, source = "stamen")
# Plot Map
mapStationsMURI = ggmap(map, extent = 'panel') +
  geom_point(data = data, aes(x=lom, y=lat), size = 1.5, col = 2, pch = 17) +
  xlab("Longitude") + ylab("Latitude") + theme(text = element_text(size = 16))

mapStationsMURI
  
ggsave(mapStationsMURI, file = "C:/Users/Camila/Desktop/Thesis/Figures/mapStationsMURI.png", height = 6, width = 8)

################################################################################
# PLOTS: Maps month by month

library(akima)
library(reshape2)
library(ggmap)    

# Bounding Box for Pacific Northwerstern area
location = c(-132, 30, -100, 60)
# Getting Map
map = get_map(location = location, source = "google")  

# This function puts data into long format
lf = function(data){
  datalf <- melt(data$z, na.rm = TRUE)
  names(datalf) <- c("x", "y", "z")
  datalf$Lon <- data$x[datalf$x]
  datalf$Lat <- data$y[datalf$y]
  return(datalf)
  }

months = c("January", "February", "March", "April", "May", "June")
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# Separate plots by month #
for (i in 1:6){
  dataMonth = subset(data, month == i)
  # Linear Spline Interpolation
  dataMonthlf = lf(with(dataMonth, interp(x = lom, y = lat, z = obsCelsius, linear = T, duplicate = "mean")))
  mapMonth = ggmap(map, extent = 'panel') +
    geom_tile(data = dataMonthlf, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.4) +     
    #stat_contour(data = dataMonthlf, aes(x = Lon, y = Lat, z = z)) +
    #scale_fill_continuous(name = expression(atop("Temperature", (degree~C))), limits = c(-25, 40), low = "white", high = "blue") +
    scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(7), limits = c(-11, 31)) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(months[i])
    summary(dataMonthlf$z)
  mapMonth
 # ggsave(mapMonth, file = paste("mapContour0", i, ".pdf", sep = ""))
}
                      
# Another option (NOT USED) #
mapMonth2 = ggmap(map, extent = 'panel') + 
    stat_contour(data = dataMonthlf, geom="polygon", aes(x = Lon, y = Lat, z = z, fill = ..level..)) +
    scale_fill_continuous(name = expression(atop("Temperature", (degree~C))), limits = c(-25, 40), low = "white", high = "blue") +
    xlab("Longitude") + ylab("Latitude") 
    
# Facetting #              
dataMonthlfTotal = NULL
for (i in 1:6){
  dataMonth = subset(data, month == i)
  lftemp = lf(with(dataMonth, interp(x = lom, y = lat, z = obsCelsius, linear = T, duplicate = "mean")))
  lftemp$month = months[i]
  dataMonthlfTotal = rbind(dataMonthlfTotal, lftemp)
}
dataMonthlfTotal$month = factor(dataMonthlfTotal$month, levels = months)

mapContours = ggmap(map, extent = 'panel') + 
    geom_tile(data = dataMonthlfTotal, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.5) +     
    stat_contour(data = dataMonthlfTotal, aes(x = Lon, y = Lat, z = z, fill = ..level..),geom="polygon") +
    scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(7)) +
    xlab("Longitude") + ylab("Latitude") + facet_wrap(~month)
mapContours

ggsave(mapContours, file = "mapContours.pdf")

################################################################################
# PLOT: Selecting stations far apart

# Selected Labels
label.star = c(736, 165) #c(136, 165) #c(736, 165) # c(569,164) 

# Subsetting data
tempdata.star = subset(data, labels %in% label.star) 
tempdata.star$Station = with(tempdata.star, ifelse(labels == label.star[1], "British Columbia", "California"))
# Time Series Raw Scale
tsPlotRawCaliBC = ggplot(tempdata.star, aes(x = time, y = obsCelsius, group = Station, colour = Station)) + 
  geom_line() + geom_point() +  xlab("Days") + ylab("Temperature (Celsius)") + theme(legend.position=c(.2, .8))
ggsave(tsPlotRawCaliBC, file = "tsPlotRawCaliBC.jpeg")
# Map
mapLocCaliBC = ggmap(map) + geom_point(data = data, aes(x=lom, y=lat), size = 2.5, pch = 17, col = gray(0.6)) +
  geom_point(data = tempdata.star, aes(x=lom, y=lat, colour = Station), size = 2.5, pch = 17) +
  xlab("Longitude") + ylab("Latitude")
mapLocCaliBC
ggsave(mapLocCaliBC, file = "mapLocCaliBC.jpeg")
# Obtaining residuals
lmfit = lm(obsCelsius ~ factor(month) + lambx*lamby + I(lambx^2) + I(lamby^2) + elevTrunc, data = data)
resData = cbind(data, res = lmfit$res)
resData.star = subset(resData, labels %in% label.star) 
resData.star$Station = with(resData.star, ifelse(labels == label.star[1], "British Columbia", "California"))
# Time Series Residuals
tsPlotResCaliBC = ggplot(resData.star, aes(x = time, y = res, group = Station, colour = Station)) + 
  geom_line() + geom_point() +  xlab("Days") + ylab("Residuals (Celsius)") + theme(legend.position=c(.2, .1))
ggsave(tsPlotResCaliBC, file = "tsPlotResCaliBC.jpeg")
################################################################################
# PLOT: Time series

###############################################################################
# PLOT: Map of residuals

# Separate plots by month #
for (i in 1:6){
  dataMonth = subset(resData, month == i)
  # Linear Spline Interpolation
  dataMonthlf = lf(with(dataMonth, interp(x = lom, y = lat, z = res, linear = T, duplicate = "mean")))
  mapMonth = ggmap(map, extent = 'panel') +
    geom_tile(data = dataMonthlf, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.4) +     
    scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(10), limits = c(-30, 20)) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(months[i])
    summary(dataMonthlf$z)
 #mapMonth
 ggsave(mapMonth, file = paste("mapResContour0", i, ".pdf", sep = ""))
}


# Separate plots by selected days #
selectedDays = c("February 8", "June 27")
selectedDates = c("2000020800", "2000062700")
for (i in 1:length(selectedDates)){
  dataDay = subset(resData,  initdate == selectedDates[i])
  # Linear Spline Interpolation
  dataDaylf = lf(with(dataDay, interp(x = lom, y = lat, z = res, linear = T)))
  mapDay = ggmap(map, extent = 'panel') +
    geom_tile(data = dataDaylf, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.4) +     
    scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(10), limits = c(-14,14)) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(selectedDays[i])
    summary(dataDaylf$z)
 #mapMonth
 ggsave(mapDay, file = paste("mapResContourDay", selectedDates[i], ".pdf", sep = ""))
}
