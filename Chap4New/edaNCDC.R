setwd("C:/Users/Camila/Desktop/PhD research/Temperature Paper/Computing")

# Reading NCDCdata
data = read.table("datanew.txt", header = T)

# Lambert
source("lambert.r")
# Updated staircase EM algorithm function
source("LZ-EM.est.1.0.1.r")
# Updated predictions
source("LZ-predict.1.0.1.r")   

# Libraries
library(geoR)
library(gstat)
library(EnviroStat)
library(fields)
library(ggplot2)
library(ggmap)
library(forecast)
library(sm)
library(sp)
library(maptools)
library(GEOmap)

# Coordinates in Lambert Projection (/100 km, thousandths of km)
lambcoords = Flamb2(abs(data[,c("LATITUDE","LONGITUDE")]))
data$lambx = lambcoords$xy[,1]
data$lamby = lambcoords$xy[,2]

# Creating time variable
data$time = rep(1:182, times = 97) # time, sequentially numbered


################################################################################
# SPATIAL LM 

# Unique dates
uniqueDates = sort(unique(data$DATE))
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
  dataForSelectedDay = subset(data, DATE == uniqueDates[i])
  lmfit = lm(TMAX10 ~ lambx*lamby + I(lambx^2) + I(lamby^2), data = dataForSelectedDay)
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

# Using ggplot

library(ggplot2)
library(gridExtra)

SpatialEffects = data.frame(times,latEffect,longEffect,lowerLat,upperLat,lowerLong,upperLong)

latgg <- ggplot(SpatialEffects, aes(x = times, y = latEffect)) + geom_line() +
  geom_ribbon(aes(ymin=lowerLat, ymax=upperLat), alpha=0.2, fill = "gray10") + xlab("Time (Days)") + ylab(expression(paste("Latitude Effect ", (degree~C/km)))) + 
  ylim(c(-.03,.03)) +  theme(text = element_text(size=16)) #+ theme_bw()

longg <- ggplot(SpatialEffects, aes(x = times, y = longEffect)) + geom_line() +
  geom_ribbon(aes(ymin=lowerLong, ymax=upperLong), alpha=0.2, fill = "gray10") + xlab("Time (Days)") + ylab(expression(paste("Longitude Effect ", (degree~C/km)))) + 
  ylim(c(-.03,.03))+ theme(text = element_text(size=16)) #+ theme_bw()

ggsave(grid.arrange(latgg, longg, nrow = 1), file = "LatLongEffects.png", width = 8, height = 4.5)

#########################################################################################

# PLOT: Variograms

# Select Dates
selectedDates = c(20000104, 20000621)
#selectedDates = uniqueDates
dataDay1 = NULL
dataDay2 = NULL
objs = c("dataDay1", "dataDay2")

for (i in 1:length(selectedDates)){
  dataForSelectedDay = subset(data, DATE == selectedDates[i])
  # Creating geodata object
  geodata = as.geodata(dataForSelectedDay, coords.col = c("lambx", "lamby"), data.col = c("TMAX10"))
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
  assign(paste(objs[i],sep=""), data.frame(u = variogb$u, v = variogb$v, v.lower = variogEnv$v.lower, v.upper = variogEnv$v.upper))
  #jpeg(paste("VariogBinnedwithEnvelopeDate", selectedDates[i], ".jpeg", sep = ""))
  #plot(variogb, xlab = "Distance (km)", ylab = "Semivariances", pch = 16, ylim = c(0,max(variogEnv$v.upper)), type = "n")
  #with(variogEnv, polygon(c(u,rev(u)),c(v.lower,rev(v.upper)), border = gray(0.6), col = gray(0.6)))
  #with(variogb, points(u, v, pch = 16, type = "b"))
  #dev.off()
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



day1 <- ggplot(dataDay1, aes(x = u, y = v)) + geom_line() +
  geom_ribbon(aes(ymin=v.lower, ymax=v.upper), alpha=0.2, fill = "gray10") + xlab("Distance (km)") + ylab(expression(paste("Semivariances ", (degree~C^2)))) +
  ylim(c(0,40)) + ggtitle("January 04")  + theme(text = element_text(size=16)) 
  #+ theme_bw()

day2 <- ggplot(dataDay2, aes(x = u, y = v)) + geom_line() +
  geom_ribbon(aes(ymin=v.lower, ymax=v.upper), alpha=0.2, fill = "gray10") + xlab("Distance (km)") + ylab("")  +
  ylim(c(0,40)) +ggtitle("June 21") + theme(text = element_text(size=16)) 
   #+ theme_bw()

ggsave(grid.arrange(day1, day2, nrow = 1),file = "VariogBinned.png", width = 8, height = 4.5)
#################################################################################################################


################################################################################
# PLOTS: Maps month by month

library(akima)
library(reshape2)
library(ggmap)    

# Bounding Box for Pacific Northwerstern area
#location = c(-132, 30, -100, 60)
location = c(-132, 37, -110, 54)
# Getting Map
map = get_map(location = location, source = "stamen")  

mapNCDC = ggmap(map, extent = 'panel') +
   geom_point(data = data, aes(x=LONGITUDE, y=LATITUDE), size = 1.8, col = "blue", pch = 17)  +     
   xlab("Longitude") + ylab("Latitude") + theme(text = element_text(size = 16))

mapNCDC

ggsave(mapNCDC, file = "C:/Users/Camila/Desktop/Thesis/Figures/mapStationsNCDC.png")

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
  dataMonthlf = lf(with(dataMonth, interp(x = LONGITUDE, y = LATITUDE, z = TMAX10, linear = T, duplicate = "mean")))
  mapMonth = 
     ggmap(map, extent = 'panel') +
    geom_tile(data = dataMonthlf, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.4) +     
    #stat_contour(data = dataMonthlf, aes(x = Lon, y = Lat, z = z)) +
    #scale_fill_continuous(name = expression(atop("Temperature", (degree~C))), limits = c(-25, 40), low = "white", high = "blue") +
    scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(7), limits = c(-11, 31)) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(months[i]) + theme(text = element_text(size = 16))
  summary(dataMonthlf$z)
  mapMonth
  # ggsave(mapMonth, file = paste("mapContour0", i, ".pdf", sep = ""))
}

##############################################################################################################

dataMonthlf = NULL
# Separate plots by month #
for (i in 1:6){
   dataMonth = subset(data, month == i)
   # Linear Spline Interpolation
   dataMonthlf = rbind(dataMonthlf,lf(with(dataMonth, 
                                           interp(x = LONGITUDE, y = LATITUDE, z = TMAX10, linear = T, 
                                                  duplicate = "mean"))))
}
   
dataMonthlf$month = factor(c(rep("January",1275), rep("February",1275), rep("March",1275), rep("April",1275), rep("May",1275), rep("June",1275)), levels = c("January", "February", "March", "April", "May","June"))

#dataMonthlf$month = factor(c(rep(1,1275), rep(2,1275), rep(3,1275), rep(4,1275), rep(5,1275), rep(6,1275)),
#                           levels = c("January", "February", "March", "April", "May","June"))

mapMonth = ggmap(map, extent = 'panel') +
   geom_tile(data = dataMonthlf, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.4) +     
   scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(7), limits = c(-11, 31)) +
   xlab("Longitude") + ylab("Latitude") + facet_wrap(~month) + theme(text = element_text(size = 14))

mapMonth
ggsave(mapMonth, file = "C:/Users/Camila/Desktop/Thesis/Figures/MapContourNCDC.png")
################################################################################
# PLOTS: Maps month by month

library(akima)
library(reshape2)
library(ggmap)    

# Bounding Box for Pacific Northwerstern area
#location = c(-132, 30, -100, 60)
location = c(-132, 37, -110, 54)
# Getting Map
map = get_map(location = location, source = "stamen")  

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
  dataMonthlf = lf(with(dataMonth, interp(x = LONGITUDE, y = LATITUDE, z = TMAX10, linear = T, duplicate = "mean")))
  #mapMonth = 
     ggmap(map, extent = 'panel') +
    geom_tile(data = dataMonthlf, aes(x = Lon, y = Lat, z = z, fill = z), alpha = 0.4) +     
    #stat_contour(data = dataMonthlf, aes(x = Lon, y = Lat, z = z)) +
    #scale_fill_continuous(name = expression(atop("Temperature", (degree~C))), limits = c(-25, 40), low = "white", high = "blue") +
    scale_fill_gradientn(name = expression(atop("Temperature", (degree~C))), colours=jet.colors(7), limits = c(-2, 35)) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(months[i])
     print(i)
  #summary(dataMonthlf$z)
  #mapMonth
   #ggsave(mapMonth, file = paste("mapContour0", i, "NCDC.jpeg", sep = ""))
}

########################################################################################

RC = c(0.013,0.145,0.208,0.185,0.148)
RCse = 0.039
RCupper = RC + 2*RCse
RClower = RC - 2*RCse
RCmonth = 2:6

RCdata = data.frame(RCmonth,RC,RCupper,RClower)
  
ggplot(RCdata, aes(x = RCmonth, y = RC)) + geom_line() + geom_point() +
  geom_ribbon(aes(ymin=RClower, ymax=RCupper), alpha=0.2, fill = "red") + xlab("Months") + ylab(expression(paste("RC effect ", (degree~C/(month%*%km^2))))) +
  ylim(c(-0.1,0.3)) + ggtitle("")  + theme(text = element_text(size=14)) + theme_bw()

tmp <- as.data.frame(effect("lambx:lamby:as.factor(month)", lmfit, list(month=c(2:6))))
ggplot(data=tmp, aes(x=month, y=fit, colour=as.factor(wt))) +
  geom_line() +
  labs(colour="wt")


