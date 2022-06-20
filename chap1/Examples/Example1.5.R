##############################################
### Example 1.5 - Visualising Spatial Data ###
##############################################

### Loading relevant libraries
library(sp)
library(rgdal)

### Load Meuse river 
data(meuse)

### We first assign a reference system used in the Netherlands
proj4string(meuse_sp) <- CRS('+init=epsg:28992') 
 
### Then we convert it to another for Google mapping that 
### requires the latitude - longitude scale
meuse_ll <- spTransform(meuse_sp,CRS("+proj=longlat +datum=WGS84"))

###Finally we write the result that can be read by Google maps
writeOGR(meuse_ll, "meuse.kml", "meuse", driver="KML")