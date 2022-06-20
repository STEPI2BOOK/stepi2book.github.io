###########################################
###   This code illustrates use of the geoR package
###   for fitting empirical variograms in Example 8.7


library(geoR)
library(sp)

### The first analysis is based on a soil concentration 
### sample set that is supplied with geoR (s100). It plots
### empirical semivariograms as described in the book.
cloud <- variog(s100, option = "cloud", max.dist=1)
plot(cloud)

smooth <- variog(s100, option = "smooth", max.dist=1)
plot(smooth)


### Compute empirical variograms with geoR using the "bin" 
### option. The first analysis takes us back to the 
### meuse lead data seen in Example 8.5
meuse.geo <- as.geodata(meuse)
plot(variog(meuse.geo,option = "bin", 
                             max.dist=1000))

### The last plot is for the New York Ozone data seen 
### in the chapter 14 tutorial of Le and Zidek (2006)

### First we load the data
locations<-read.table("NY.metadata.txt", header=TRUE)
o3<- read.table("NY.data.ch14.txt")

### Add variable names
names(o3)<-c("month","day","hour","weekday","week",
                    "S1","S2","S3","S4","S5","S6","S7","S8","S9")

### Take sqrt transformation of ozone concentrations
o3sqrt<-cbind(o3[,1:5],sqrt(o3[,6:14]))

### Compute average temperatures on Aug 1
o3sqrt.Aug1<-apply(o3sqrt[2929:2952,],2,mean)

### Change lat long to Lambert coordinates
library(EnviroStat)
coords<-Flamb2(locations)

### Augment data file with coordinates
o3sqrt.Aug1.aug <-cbind(coords$xy,o3sqrt.Aug1)

### Convert to geodata
o3.geo<-as.geodata(o3sqrt.Aug1.aug)

### Compute empirical variograms with geoR using bin 
###  and smooth options
plot(variog(o3.geo, options="cloud", max.dist=400))







