##############################################################
###  Mapping locations of mercury deposition network sites ###
##############################################################
library(sp)
library(ggmap)

# Loading the metadata - including Lat and Long and Preparing the file.
mdn_metadata <- read.csv("MDNSitesMetaData.csv", sep=",")

# copy mdn_metadata into mdn_metadata_sp 
# so the former can be used later by other packages
mdn_metadata_sp<-mdn_metadata 

## convert data to "sp" format
coordinates(mdn_metadata_sp) <- ~Long+Lat  

# Specifying the projection code
proj4string(mdn_metadata_sp) <- CRS("+proj=longlat +ellps=WGS84")  ## assign a reference system to ca_data_sp

# Specifying the bounding box - 2 x 2 matrix of corners 
# of the geographic area.
# Then specifying the range of locations within the box.  
# The location must be in left-bottom-right-top bounding box format
latLongBox = bbox(mdn_metadata_sp)
location = c(latLongBox[1, 1]-0.2, latLongBox[2, 1]-0,latLongBox[1, 2]+0.2, latLongBox[2, 2]+0.2)

### Now create the map with location dots marked on it in
MDNmap <- get_map(location = location, source = "ocm")
MDNmap <- ggmap(MDNmap)
MDNmap <- MDNmap + geom_point(data=mdn_metadata, aes(x=Long, y=Lat), size=4)
MDNmap


#############################################################################
###  Repeating the above analysis for New York ozone monitoring locations ###
#############################################################################
library(sp)
library(ggmap)

ny_data <- read.csv("NY.metadata.txt", sep="")

# copy ca_data into ca_data_sp so that ca_data can be used later by other packages
ny_data_sp <- ny_data

# convert data to "sp" format
coordinates(ny_data_sp) <- ~Longitude+Latitude 

# Specifying the projection code
# assign a reference system to ca_data_sp
proj4string(ny_data_sp) <- CRS("+proj=longlat +ellps=WGS84")  


# Specifying the bounding box - 2 x 2 matrix of corners 
# of the geographic area. Then specifying the range of 
# locations within the box.  The location must be in 
# left-bottom-right-top bounding box format
latLongBox = bbox(ny_data_sp)
location = c(latLongBox[1, 1]-0.2, latLongBox[2, 1]-0,
             latLongBox[1, 2]+0.2, latLongBox[2, 2]+0.2)

# Now create the map with location dots marked on it in
NYmap <- get_map(location = location, source = "google", color="bw", maptype="roadmap")
NYmap <- ggmap(NYmap)
NYmap <- NYmap + geom_point(data=ny_data, aes(x=Longitude, y=Latitude), size=4)
NYmap

