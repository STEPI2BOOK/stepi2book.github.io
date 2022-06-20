########################################################
########  Create Google Earth readable file with ggmap
########  Reference: kahle-wickham2013ggplot2.pdf

library(sp)
library(rgdal)


data(meuse) ### Load data

##coordinates(meuse_sp)<-~x+y # convert to SPDF 

### Assign Netherlands reference system 
proj4string(meuse_sp) <- CRS('+init=epsg:28992') 

### Convert to longlat reference system for Google Earth
meuse_ll <- spTransform(meuse_sp,
                        CRS("+proj=longlat +datum=WGS84"))

### Write .kml file for Google Earth

writeOGR(meuse_ll, "meuse.kml", "meuse", driver="KML")   

### Open "meuse.kml" in Google Earth to see plot.