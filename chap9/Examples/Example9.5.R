#####################################################
### Example 9.5. Spatial modelling of temperature ###
#####################################################
library(geoR)
library(sp)

### First we load the data
CAmetadata<- read.table("metadataCA.txt",header=TRUE)
CATemp<-read.csv("MaxCaliforniaTemp.csv",header=TRUE)
CATemp_20120401<-CATemp[CATemp$Date==20120401,2:19]
CATemp_20120401<-t(CATemp_20120401)

### Change names of lat, long
names(CAmetadata)[names(CAmetadata)=="Long"]<-"x"
names(CAmetadata)[names(CAmetadata)=="Lat"]<-"y"

### Augment data file with coordinates
CATemp_20120401 <-cbind(CAmetadata,CATemp_20120401)
names(CATemp_20120401)[names(CATemp_20120401)=="92"]<-"Temp"

library(geoR)
### CATemp.geo<-as.geodata(CATemp,coords.col = 3:4,covar.names=c("Location","Elev","OLat","Olong"))
### CATempOri.geo<-as.geodata(CATemp_2012040,coords.col = 3:4,covar.names=c("Location","Elev","OLat","Olong"))
CATempOri.geo<-as.geodata(CATemp_20120401,coords.col = 3:4,data.col=7)

### Compute empirical variograms for the residual process with geoR. 
###  The power variogram seemed best
### although this points to nonstationarity in the process
CA.vario4<-variog4(CATempOri.geo, uvec = seq(0,8,l=8))
plot(CA.vario4)

