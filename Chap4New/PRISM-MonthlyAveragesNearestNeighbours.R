rm(list=ls(all=TRUE))

################################################################################
#                    30-year Normal 4-km resolution grid                       #
################################################################################

# Set directory
setwd("C:/Users/Camila/Desktop/PRISM/PRISM_tmean_30yr_normal_4kmM2_all_asc/")

# Libraries
library(fields)
library(raster)
library(sp)
library(foreach)
library(doParallel)
library(reshape2)
library(ggplot2)

# read.PRISM.field by Doug Nychka
source("C:/Users/Camila/Desktop/PRISM/read.PRISM.field.R")

# Read NCDC dataset
dataall = read.table("C:/Users/Camila/Desktop/GHCND Temperature Data/NCDCdata2.txt", 
                     header = T) # includes 97 stations

dataall$month = as.integer(substr(as.character(dataall$DATE), start = 5, stop = 6))

# Stations 
locs = unique(dataall[,4:5])    
locs.df = data.frame(x = locs$LONGITUDE, y = locs$LATITUDE)


##############################
                        
PRISM.average <- function(month, locs){
  
  # Read PRISM data
  fn1 = paste("PRISM_tmean_30yr_normal_4kmM2_", 
            ifelse(month<10, paste("0", month, sep = ""), month), "_asc.asc", sep = "")
  temp1 =  read.PRISM.field(fn1)

  # Smoothing
  temp1 =  average.image(temp1, Q=12)
  
  # PRISM locs
  lx = length(temp1$x)
  ly = length(temp1$y)
  prism.locs = cbind(temp1$x, rep(temp1$y, each = lx))
                     
  # PRISM data in vector format
  prism.data = c(temp1$z[(1:lx),rev(1:ly)])

  # PRISM grid
  prism.grid = as.data.frame(prism.locs)
  names(prism.grid) <- c("long","lat")

  # Converting to SpatialPoints object
  coordinates(prism.grid) <- ~ long + lat
  proj4string(prism.grid) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  # Create SpatialPoints
  sp = SpatialPoints(locs, bbox = prism.grid@bbox)
  
  # Create raster
  r <- raster(ncol=lx, nrow=ly, xmn = min(temp1$x),
              xmx = max(temp1$x), ymn = min(temp1$y), ymx = max(temp1$y))
  r[] <- prism.data
  
  # Identify cell numbers
  cell.numbers = extract(r, sp, cellnumbers = T)[,1]
  
  cl <-  makePSOCKcluster(2)
  registerDoParallel(cl, cores = 4)
  
  avg = foreach(i = 1:nrow(locs), .combine = c, .packages = "raster") %dopar% {
    # Identify Adjacent cells
    a = unique(c(adjacent(r, cell = cell.numbers[i])))
    
    # Compute average of adjacent cells
    mean(extract(r, y = xyFromCell(r, a)), na.rm = T)
  }
  stopCluster(cl)

  return(avg = avg)

}

##############################

# For all months

locs = locs.df
monthly.avg = matrix(NA, nrow = nrow(locs), ncol = 12)
for(i in 1:12){
  monthly.avg[,i] = PRISM.average(i, locs)
                        
}

# PRISM averages

PRISMavg = as.data.frame(monthly.avg[,1:6])
names(PRISMavg) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
PRISMavg$STATION = unique(dataall$STATION)

# Melting 
PRISMavg.melt = melt(PRISMavg, id = "STATION", variable.name = "month", value.name = "PRISMavg")
PRISMavg.melt$month = as.numeric(PRISMavg.melt$month)

# Creating a new data file with all stations and PRISM monthly averages
datanew = merge(dataall, PRISMavg.melt, by=c("STATION", "month"))
datanew$TMAX10 = with(datanew, TMAX/10)
datanew$anom = with(datanew, TMAX10-PRISMavg)
datanew$stID =  with(datanew, as.numeric(STATION))

write.table(datanew, file = "datanew.txt", row.names = F)


# MAPPING

for (i in 1:97){
 
  d = subset(datanew, stID == i)
  
  ggsave(paste("AnomaliesStation-", i, ".pdf", sep = ""))
  ggplot(d, aes(x = TMAX10, y = anom, group = month, colour = as.factor(month))) + 
    geom_point() + labs(x = "Temperatures (Celsius)", y = "Anomalies (Celsius)", 
                        colour = "Station", title = paste("Station",i))  
  
  
}



