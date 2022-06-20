######################################################################################
### Example 9.3. Mapping the locations of ozone monitoring sites in New York State ###
######################################################################################

### Loading libraries
library(sp)
library(ggmap) 

## Load the metadata giving the site coordinates 
ny_data <- read.csv("NY.metadata.txt", sep="") 
## Now copy ny_data into ny_data_sp and convert data to "sp" format 
ny_data_sp <- ny_data
coordinates(ny_data_sp) <- ~Longitude+Latitude 

## assign a reference system to ny_data_sp 
proj4string(ny_data_sp) <- CRS("+proj=longlat +ellps=WGS84") 

### We next specify a bounding box - a 2 x 2 matrix of 
### corners of the geographic area. Then specify the ### range of locations within the box. 
### Note: location must ### bounding box format be in left -bottom -right -top 
latLongBox = bbox(ny_data_sp)
location = c(latLongBox[1, 1]-0.2, latLongBox[2, 1]-0, 
latLongBox[1, 2]+0.2, latLongBox[2, 2]+0.2) 

######## Now create the map with location dots 
NYmap <- get_map(location = location, source = "google",color="bw", maptype="roadmap") 
NYmap <- ggmap(NYmap) 
NYmap <- NYmap + geom_point(data=ny_data, aes(x=Longitude , y=Latitude), size=4) 
