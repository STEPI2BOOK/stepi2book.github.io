#####################################################################################
### Example 9.13 - Plotting directional variograms for temperatures in California ###
#####################################################################################

### Load data 
> CAmetadata<- read.table("metadataCA.txt",header=TRUE) 
> CATemp<-read.csv("MaxCaliforniaTemp.csv",header=TRUE) > dimnames(CATemp)[[2]][1]="Date" 
> CATemp20120401<-subset(CATemp, Date==20120401) 

### Augment data file with coordinates, note date is omitted as is the 
### value for Bakersfield for which there is no meta-data 
> CAmetadata=cbind(CAmetadata , t(CATemp20120401[c(2:14,16:19)])) 

### Create geoR data geofile 
> library(geoR) 
CATemp.geo<-as.geodata(CAmetadata ,coords.col = 3:4,data.col =7) 

### Compute and plot the directional variogram 
> CA.vario4<-variog4(CATemp.geo, uvec = seq(0,8,l=8)) > plot(CA.vario4) 
