#Metro Vancouver air quality site information

######## Loading the libraries
library(sp)
library(ggmap)

# Loading the site metadata - including Lat and Long and Preparing the file.
site_metadata <- read.csv("Stations_2014update_v10072014-1.csv", sep=",")
metvan_metadata<-site_metadata[513:547,]
metvan_metadata_sp<-metvan_metadata ## copy metvan_metadata into metvan_metadata_sp so the former 

# can be used later by other packages
# convert data to "sp" format
coordinates(metvan_metadata_sp) <- ~Long+Lat 

# Specifying the projection code
# assigning a reference system to metvan_metadata_sp
proj4string(metvan_metadata_sp) <- CRS("+proj=longlat +ellps=WGS84")  

# Specifying the bounding box - 2 x 2 matrix of corners of the geographic area. 
# Then specifying the range of locations within the box.  
# The location must be in left-bottom-right-top bounding box format
latLongBox = bbox(metvan_metadata_sp)
location = c(latLongBox[1, 1]-0.09, latLongBox[2, 1]-0,
             latLongBox[1, 2]+0.09, latLongBox[2, 2]+0.09)

# Now create the map with location dots marked on it in
metvanmap <- get_map(location = location, source = "google")
metvanmap <- ggmap(metvanmap)
metvanmap <- metvanmap + geom_point(data=metvan_metadata, aes(x=Long, y=Lat), size=4)
metvanmap

postscript('metvanmap.eps', colormodel="cmyb")





