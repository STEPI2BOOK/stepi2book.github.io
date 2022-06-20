library(plotGoogleMaps)
data(meuse)
coordinates(meuse)<-~x+y  # convert to SPDF 
proj4string(meuse) <- CRS('+init=epsg:28992') 

### Adding Coordinate Reference System.
### For Cadmium
### Create web map of Point data 
m<-plotGoogleMaps(meuse,filename='myMap1.htm')

### For zinc: labels
ic=iconlabels(meuse$zinc, height=12) 
m<-plotGoogleMaps(meuse,zcol='zinc',filename='myMap_z2.htm',iconMarker=ic, mapTypeId='ROADMAP',layerName = 'MEUSE POINTS')

m<-bubbleGoogleMaps(meuse,zcol='zinc',
                    max.radius = 80,
                    filename='myMap3.htm') # remove outlines
                    
m<-bubbleGoogleMaps(meuse,zcol='zinc',
                    max.radius = 80,
                    filename='myMap3.htm',
                    strokeOpacity=0)

# colPalette defines colors for plot 
m<-segmentGoogleMaps(meuse, zcol=c('zinc','dist.m'),
                     mapTypeId='ROADMAP', 
                     filename='myMap4.htm', 
                     colPalette=c('#E41A1C','#377EB8'), 
                     strokeColor='black')

# Results of least square
ell<- data.frame(E=c(7456263,7456489,7456305,7457415,7457688),
                 N=c(4954146 ,4952978, 4952695, 4953038, 4952943), 
                 Name=c('30T', '31T', '3N', '40T', '41T'),
                 A=c(2.960863 ,4.559694, 7.100088, 2.041084 ,3.375919), 
                 B=c(2.351917, 2.109060, 2.293085, 1.072506, 2.382449), 
                 teta=c(28.35242, 41.04491, 38.47216, 344.73686, 27.53695))
coordinates(ell) <- ~E+N
proj4string(ell) <- CRS("+proj=tmerc +lat_0=0 +lon_0=21 +k=0.9999
                        +x_0=7500000 +y_0=0 +ellps=bessel
                        +towgs84=574.027,170.175,401.545,4.88786,-0.66524,-
                        13.24673,0.99999311067 +units=m")
# fillOpacity 100 %
m<-ellipseGoogleMaps( ell,filename="Ellipse.htm",zcol=2:4,
                      mapTypeId='ROADMAP',fillOpacity=1,strokeOpacity=0)

# Line data
data(meuse.grid)
coordinates(meuse.grid)<-c('x','y') 
meuse.grid<-as(meuse.grid,'SpatialPixelsDataFrame') 
im<-as.image.SpatialGridDataFrame(meuse.grid['dist']) 
cl<-ContourLines2SLDF(contourLines(im))
proj4string(cl) <- CRS('+init=epsg:28992')
mapMeuseCl<- plotGoogleMaps(cl,
                            zcol='level',
                            strokeWeight=1:9,
                            filename='myMap5.htm',
                            mapTypeId='ROADMAP')

