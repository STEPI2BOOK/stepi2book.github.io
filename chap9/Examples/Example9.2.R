#################################################################
### Example 9.2 - Examining the log concentrations of lead in ###
###               the Meuse River flood plain                 ###
#################################################################

###  Create Google Earth readable file with ggmap
###  Reference: kahle-wickham2013ggplot2.pdf

### Load Libraries
library(sp)
library(rgdal)

### Load data
data(meuse) 
coordinates(meuse)<-~x+y # convert to SPDF 

### Assign Netherlands reference system 
proj4string(meuse) <- CRS('+init=epsg:28992') 

### Convert to longlat reference system for Google Earth
meuse_ll <- spTransform(meuse,CRS("+proj=longlat +datum=WGS84"))

### Write .kml file for Google Earth
writeOGR(meuse_ll, "meuse.kml", "meuse", driver="KML")   

### Open "meuse.kml" in Google Earth to see plot.