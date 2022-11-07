setwd("C:/Users/Camila/Desktop/PhD Research/Temperature Paper/Computing")

# Lambert
source("lambert.r")
# Updated staircase EM algorithm function
source("LZ-EM.est.1.0.1.r")
# Updated predictions
source("LZ-predict.1.0.1.r")     
# PRISM 
source("read.PRISM.field.r")

# Temperature Modelling
source("TemperatureModellingUnifiedFunctions.r")

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
library(KRLS)
library(gridExtra)
library(reshape2)

#######################################################################################

# Reading NCDCdata
data = read.table("datanew.txt", header = T)
lambcoords = Flamb2(abs(data[,c("LATITUDE","LONGITUDE")]))
data$lambx = lambcoords$xy[,1]/100   # note transformation in the coordinates!
data$lamby = lambcoords$xy[,2]/100


load("run.RData")
# Selecting stations for training and validation purposes
id = sampleStations(stations = unique(data[,c("LATITUDE","LONGITUDE")]), u = 33, seed = 8)
data$stLabel = ifelse(data$stID %in% id$gauged, "Training","Validation")

# Gauged stations data (for Training)
gaugedData = meanModelling(data, idSel = id$gauged)

qqnorm(gaugedData$Res, pch = 16, col = "gray", main = "")
qqline(gaugedData$Res, lwd = 2, col = "red")

# Ungauged stations data (for Validation)
predData = meanModelling(data, idSel = id$ungauged)
predData$time = rep(1:182, times = 33)

# Applying EnviroStat Model
r       = EnviroStatModel(gaugedData, predData, lambcoords, lambda = 5, PRISM = 0)
r.PRISM = EnviroStatModel(gaugedData, predData, lambcoords, lambda = 5, PRISM = 1)

# Obtaining predictions
predBSP   = predSimu(predData, r$hyper.est, PRISM = 0)
predPRISM = predSimu(predData, r.PRISM$hyper.est, PRISM = 1)
predOK    = predgeoR(predData)

# Comparing prop
1-mean(predBSP$prop)
1-mean(predPRISM$prop)
1-mean(predOK$prop)

# Summary

round(mean(predBSP$MSE),3)
round(sd(predBSP$MSE),3)
round(mean(predBSP$MSEday),3)
round(mean(predPRISM$MSE),3)
round(sd(predPRISM$MSE),3)
round(mean(predOK$MSE),3)
round(sd(predOK$MSE),3)

# Comparing MSEs
MSEdataBSP   = data.frame(cbind(unique(predData[,c("LATITUDE","LONGITUDE")]), MSE = predBSP$MSE))
MSEdataPRISM = data.frame(cbind(unique(predData[,c("LATITUDE","LONGITUDE")]), MSE = predPRISM$MSE))
MSEdataOK    = data.frame(cbind(unique(predData[,c("LATITUDE","LONGITUDE")]), MSE = predOK$MSE))

MSEdata = merge(merge(MSEdataBSP,MSEdataPRISM, by = c("LATITUDE","LONGITUDE")), MSEdataOK, by = c("LATITUDE","LONGITUDE"))
names(MSEdata) = c("LATITUDE","LONGITUDE", "BSP", "BSP - PRISM", "OK")
MSEdata = melt(MSEdata, id = c("LATITUDE","LONGITUDE"))
names(MSEdata) = c("LATITUDE","LONGITUDE", "Method", "MSE")

MSEdataDay = data.frame(Day = rep(1:182,3), 
                        MSE = c(predBSP$MSEday, predPRISM$MSEday, predOK$MSEday), 
                        Method = c(rep(c("BSP"),182), rep(c("BSP - PRISM"),182), rep(c("OK"),182)))

##########################################################
# PLOT: Map of MSE

location = c(-132, 30, -100, 60)
map = get_map(location = location, source = "google")

location = c(-132, 37, -110, 54)
map = get_map(location = location, source = "stamen")  


MSEmap = ggmap(map, extent = 'panel') +
   geom_point(data = gaugedData, aes(x=LONGITUDE, y=LATITUDE), size = 1.7, pch = 17, col = 2) +
   geom_point(data = MSEdata, aes(x=LONGITUDE, y=LATITUDE, colour = MSE, name = "MSPE"), size = 1.7, pch = 16) +
   scale_colour_gradient("MSPE",limits = c(0, 29)) + ggtitle("") + theme(text = element_text(size = 14)) +
   xlab("Longitude") + ylab("Latitude") + facet_wrap(~Method)
MSEmap

ggsave(MSEmap, file = "C:/Users/Camila/Desktop/Thesis/Figures/MSEmap.pdf", width=10)

##########################################################


bpMSE = ggplot(MSEdata, aes(x=Method, y=MSE)) + geom_boxplot() + xlab("") + ggtitle("MSPE across time") +
   theme(text = element_text(size = 16)) + scale_y_continuous(limit = c(0,50)) + ylab("MSPE")

bpMSEday = ggplot(MSEdataDay, aes(x=Method, y=MSE)) + geom_boxplot() + xlab("") + ggtitle("MSPE across space") +
   theme(text = element_text(size = 16))  + scale_y_continuous(limit = c(0,50)) + ylab("")

grid.arrange(bpMSE, bpMSEday, nrow = 1)

ggsave(grid.arrange(bpMSE, bpMSEday, nrow = 1), file = "C:/Users/Camila/Desktop/Temperature Paper/Figures/bpMSE.pdf",
       width = 8, height = 5)


###########################################################

location = c(-132, 37, -110, 54)
map = get_map(location = location, source = "stamen")  

stationMap = ggmap(map, extent = 'panel') +
   geom_point(data = data, aes(x=LONGITUDE, y=LATITUDE, shape = stLabel, colour = stLabel), size = 2.5)+
   scale_shape_manual(values = c(17,16), name = "Stations") +
   scale_colour_manual(values = c('red','blue'), name = "Stations") +
   #geom_point(data = data, aes(x=LONGITUDE, y=LATITUDE), colour = "red", size = 2.5, pch = 17) +
   xlab("Longitude") + ylab("Latitude") + theme(text = element_text(size = 16)) 

ggsave(stationMap, file = "C:/Users/Camila/Desktop/Thesis/Figures/mapStationsNCDCTrainValid.png")
