################################################################
###  Mapping locations of California daily temperature sites ###
################################################################
library(sp)
library(ggmap)

### Loading the metadata - including Lat and Long and Preparing the file.
ca_data <- read.csv("metadataCA.txt", sep="")
ca_data_sp <- ca_data  ## copy ca_data into ca_data_sp so that ca_data can be used later by other packages
coordinates(ca_data_sp) <- ~Long+Lat  ## convert data to "sp" format

### Specifying the projection code
proj4string(ca_data_sp) <- CRS("+proj=longlat +ellps=WGS84")  ## assign a reference system to ca_data_sp

### Specifying the bounding box - 2 x 2 matrix of corners of the geographic area. 
### Then specifying the range of locations within the box.  The location must be 
### in left-bottom-right-top bounding box format
latLongBox = bbox(ca_data_sp)
location = c(latLongBox[1, 1]-0.2, latLongBox[2, 1]-0
             latLongBox[1, 2]+0.2, latLongBox[2, 2]+0.2)

### Now create the map with location dots marked on it in
CAmap <- get_map(location = location, source = "google")
CAmap <- ggmap(CAmap)
CAmap <- CAmap + geom_point(data=ca_data, aes(x=Long, y=Lat), size=4)
CAmap

#############################################################################
###  Repeating the above analysis for New York ozone monitoring locations ###
#############################################################################
ny_data <- read.csv("NY.metadata.txt", sep="")
ny_data_sp <- ny_data  ## copy ca_data into ca_data_sp so that ca_data can be used later by other packages
coordinates(ny_data_sp) <- ~Longitude+Latitude  ## convert data to "sp" format

### Specifying the projection code
proj4string(ny_data_sp) <- CRS("+proj=longlat +ellps=WGS84")  ## assign a reference system to ca_data_sp

### Specifying the bounding box - 2 x 2 matrix of corners of the geographic area. 
### Then specifying the range of locations within the box.  The location must be 
### in left-bottom-right-top bounding box format
latLongBox = bbox(ny_data_sp)
location = c(latLongBox[1, 1]-0.2, latLongBox[2, 1]-0,
             latLongBox[1, 2]+0.2, latLongBox[2, 2]+0.2)

### Now create the map with location dots marked on it in
NYmap <- get_map(location = location, source = "google", color="bw", maptype="roadmap")
NYmap <- ggmap(NYmap)
NYmap <- NYmap + geom_point(data=ny_data, aes(x=Longitude, y=Latitude), size=4)
NYmap

