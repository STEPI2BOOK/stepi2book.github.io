#########################################################
###################  Plots using plotGoogleMaps 
################### Reference:kilibarda2013plotGoogleMaps.pdf
# Data preparation
# Point data
# Data preparation
# Point data
library(plotGoogleMaps)
data(meuse)
coordinates(meuse)<-~x+y # convert to SPDF 
proj4string(meuse) <- CRS('+init=epsg:28992') 
# adding Coordinate Referent Sys.
# Create web map of Point data 


m<-plotGoogleMaps(meuse,filename='meuselead_map.htm')

# zinc labels
ic=iconlabels(meuse$lead, height=12) 
m<-plotGoogleMaps(meuse,zcol='lead',filename='meuselead_z2.htm',
          iconMarker=ic, mapTypeId='ROADMAP',layerName = 'MEUSE POINTS')


m<-bubbleGoogleMaps(meuse,zcol='lead',max.radius = 80,filename='meuselead2.htm') # remove outlines
m<-bubbleGoogleMaps(meuse,zcol='lead',max.radius = 80,
                    filename='meuselead2.htm',strokeOpacity=0)


# Line data
data(meuse.grid)
coordinates(meuse.grid)<-c('x','y') 
meuse.grid<-as(meuse.grid,'SpatialPixelsDataFrame') 
im<-as.image.SpatialGridDataFrame(meuse.grid['dist']) 
cl<-ContourLines2SLDF(contourLines(im))
proj4string(cl) <- CRS('+init=epsg:28992')

mapMeuseCl<- plotGoogleMaps(cl,zcol='level',strokeWeight=1:9 ,
                            filename='meusedist5.htm',mapTypeId='ROADMAP')



###################################################
####################################################

#########################################################
###################  Plots using ggmaps
################### Need to use spTransform to get to lat long
################## Reference: kahle-wickham2013ggplot2.pdf
library(sp)
library(rgdal)

data(meuse)

##coordinates(meuse_sp)<-~x+y # convert to SPDF 
proj4string(meuse_sp) <- CRS('+init=epsg:28992') 
meuse_ll <- spTransform(meuse_sp,
                            CRS("+proj=longlat +datum=WGS84"))
writeOGR(meuse_ll, "meuse.kml", "meuse", driver="KML")                        

##xy<-coordinates(meuse.ll)
##SPDF <- SpatialPointsDataFrame(coords=xy, data=meuse_ll)
### And then convert it (back) to a data.frame
###meuse_dataframe <- as.data.frame(SPDF)
xy<-coordinates(meuse_ll)
meuse$x<-xy[,1]
meuse$y<-xy[,2]


library(ggmap)

##junk2<-get_map(location=c(5.75, 50.93),source="google")

##qmap(location = c(5.71, 50.95,5.78, 50.99), zoom = 13)
##gglocator(2)

theme_set(theme_bw(16))
meusemap<-qmap(location=c(5.71, 50.95,5.78, 50.99),
               zoom=13,color="bw",legend="topleft")

meusemap+geom_point(aes(x=x,y=y,size=lead), data=meuse)



meusemap+geom_point(aes(x=x,y=y,size=lead), data=meuse) + 
  stat_density2d(aes(x=x,y=y,size=lead), data = meuse,
                 geom = "density2d", position = "identity",
                 na.rm = FALSE, color="black",contour = TRUE, n = 100)


