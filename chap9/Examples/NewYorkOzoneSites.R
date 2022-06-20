#################################################################
######## Plotting New York ozone monitoring locations.
######## Reference: kahle-wickham2013ggplot2.pdf
library(sp)
library(ggmap)

ny_data <- read.csv("NY.metadata.txt", sep="")

### copy ca_data into ca_data_sp so 
### ca_data can be used later by other packages
ny_data_sp <- ny_data   

### convert data to "sp" format
coordinates(ny_data_sp) <- ~Longitude+Latitude  
                       
### Specifying the projection code
proj4string(ny_data_sp) <- CRS("+proj=longlat +ellps=WGS84")  
                        ### assign a reference system to ca_data_sp

### Specifying the bounding box - 2 x 2 matrix of c
### corners of the geographic area. Then specifying the 
### range of locations within the box.  The location 
### must be in left-bottom-right-top bounding box format
latLongBox = bbox(ny_data_sp)
location = c(latLongBox[1, 1]-0.2, latLongBox[2, 1]-0,
             latLongBox[1, 2]+0.2, latLongBox[2, 2]+0.2)

######## Now create the map with location dots marked on it in
NYOzoneMap <- get_map(location = location, source = "google", color="bw", 
                 maptype="roadmap")
NYOzoneMap <- ggmap(NYmap)
NYOzoneMap <- NYmap + geom_point(data=ny_data, aes(x=Longitude, y=Latitude), size=4)
NYOzoneMap
