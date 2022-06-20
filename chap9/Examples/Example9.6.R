#####################################################################
### Example 9.6. Spatial prediction of temperatures in California ###
#####################################################################
library(geoR)
library(sp)
library(utils)
library(gstat)

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
coordinates(CATemp_20120401) <- ~x +y

### Now construct the variogram
CAgstat.vario <- variogram(Temp~1,CATemp_20120401, print.SSE=TRUE)
plot(CAgstat.vario)

### Get the fit
CATemp.variofit <- fit.variogram(CAgstat.vario, vgm(5000,"Exp",2.9,0)) 

### Examine the fit
summary(CATemp.variofit)

### Create the grid on which to make the spatial predictions
CAPred.loci<-expand.grid(seq(-125,-115,.1),seq(32,42,.1))
names(CAPred.loci)[names(CAPred.loci)=="Var1"]<-"x"
names(CAPred.loci)[names(CAPred.loci)=="Var2"]<-"y"

coordinates(CAPred.loci) = ~ x + y
gridded(CAPred.loci) = TRUE

mod <- vgm(5000, "Exp", 2.9, .04)
# ordinary kriging:
x <- krige(Temp ~1, CATemp_20120401, CAPred.loci, model = mod)
spplot(x["var1.pred"], main = "ordinary kriging predictions")
spplot(x["var1.var"],  main = "ordinary kriging variance")
