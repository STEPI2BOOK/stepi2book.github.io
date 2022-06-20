##################################################################################
### Thanks go to Yang Liu for permission to include his code used to model the ###
### California's temperature field for Question 9 in a project assigned for    ###
### a course co-instucted by Gavin Shaddick and James V Zidek in 2013.         ###
##################################################################################
library(EnviroStat)
library(sp)
library(rgdal)

dat <- read.csv("MaxCaliforniaTemp.csv", header=T)
crs.org <-  read.table("metadataCA.txt", header=T)
#dyn.load("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/SG-method/SG.dll")
#dyn.load("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Design/design.1.0.dll")
#source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/SG-method/SG.1.0.1.r")
#source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Para.est/LZ-EM.est.1.0.1.r")
#source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Design/LZ-design.1.0.1.r")
#source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Pred.dist/LZ-predict.1.0.1.r")
###Question 2


month <- 1:13
day <- 1:28
dgrid <- expand.grid(d=day, m=month)
dat <- dat[-365, ]
ZZ <-  model.matrix(~as.factor(dgrid$m))

em.fit = staircase.EM(dat[,2:19],p=1,covariate= ZZ,maxit=200,tol=.000001, verbose=T)
cov.est = em.fit$Psi[[1]]
dim1= dim(cov.est)[1]
dim2= dim(em.fit$Omega)[1]
corr.est = cov.est / sqrt( matrix(diag(cov.est),dim1,dim1)*t(matrix(diag(cov.est),dim1,dim1)) )

crs.lam <- Flamb2(abs(crs.org[, 3:4])) #the projected coordinates of the original 18 stations
ldist <- Fdist(crs.lam$xy)
disp = 2-2*corr.est
scoord  = crs.lam$xy/10 

X11()
par(mfrow=c(1, 2))
sg.est = Falternate3(disp, scoord, max.iter=100,alter.lim=100, model=1) 

apply(scoord, 2, range)
coords.grid = Fmgrid(range(scoord[,1]), range(scoord[,2]))
par(mfrow=c(1,2))
temp = setplot(scoord, ax=T)  
deform  = Ftransdraw(disp=disp, Gcrds=scoord, MDScrds=sg.est$ncoords, gridstr=coords.grid)

Tspline = sinterp(scoord, sg.est$ncoords, lam = 50 )


##########Need lay to find out the points inside California
#Map base
state <- readOGR(dsn="states010", layer="statesp010g")
cal <- state[state$NAME=="California", ]


#Prepare the original points
crs.org.sp <- crs.org
coordinates(crs.org.sp) <- ~Long + Lat
#The new data grid
ngrid <-  15
lat10 = seq(32, 42,length=ngrid)
long10 = seq(113, 125,length=ngrid)
ogrid =  cbind(rep(lat10,ngrid),c(outer(rep(1,ngrid),long10)))

ogrid.sp <- data.frame(Long=-ogrid[,2], Lat=ogrid[,1])
coordinates(ogrid.sp) <- ~Long + Lat
#Subset the points in Oregon
oo <- overlay(ogrid.sp, cal)
ogrid.sp.inCA <- ogrid.sp[!is.na(oo), ]
llgrid <- ogrid[!is.na(oo), ]

(u= sum(!is.na(oo))) # number of new locations inside California
p=1

z = crs.lam
newcrds.lamb = Flamb2(llgrid,latrf1=z$latrf1, latrf2=z$latrf2, latref=z$latref, lngref=z$lngref)$xy/10

#Combine the new locations and stations together begining with new locations
allcrds = rbind(newcrds.lamb,scoord)
corr.est = corrfit(allcrds, Tspline = Tspline, sg.fit  = sg.est, model = 1)
# Non-homogeneous and interpolating using the same thin-plate spline
Tspline.var = sinterp(allcrds[(u+1):(u+18),],matrix(diag(cov.est),ncol=1),lam=50)

varfit = seval(allcrds,Tspline.var)$y
temp = matrix(varfit,length(varfit),length(varfit))
covfit = corr.est$cor * sqrt(temp * t(temp))

hyper.est = staircase.hyper.est(emfit= em.fit,covfit=covfit,u =u, p=1)

nsel = 5
yy = ldet.eval((hyper.est$Lambda.0+ t(hyper.est$Lambda.0))/2,nsel,all =F)

$coord.sel
[1]  3 20 44 46 66

X11()

pdf("E:/STAT547L/fp/report/Q9.pdf", width=5, height=5)
par(mar=c(1, 1, 1, 1))
plot(cal)
plot(ogrid.sp.inCA, add=T, pch=17, col="skyblue")
plot(crs.org.sp, add=T, pch=15, cex=1)
plot(ogrid.sp.inCA[yy$coord.sel], add=T, pch=19, col="red", cex=1.5)
legend("topright", legend=c("Original Site", "Candidate Site", "Recommended Site"), 
col=c(1, "skyblue", "red"), text.col=c(1, "skyblue", "red"), bty="n", pt.cex=c(1, 1, 1.5),
pch=c(15, 17,  19))
dev.off()

covvec <- diag(covfit)
X11()
plot(cal)
#text(ogrid.sp.inCA, text=, col="skyblue")
plot(crs.org.sp, pch=as.character(round(covvec[c(u+1:u+18)]/100)), add=T)
plot(ogrid.sp.inCA[yy$coord.sel], add=T, pch=19, col="red", cex=1.5)
legend("topright", legend=c("Original Site", "Canddiate Site", "Recommended Site"), 
col=c(1, "skyblue", "red"), text.col=c(1, "skyblue", "red"), bty="n", pt.cex=c(1, 1, 1.5),
pch=c(15, 17,  19))

plot(1:19, pch=1:19)

#############################Plot them on Google Map
library(plotGoogleMaps)
library(RColorBrewer)

gpdat4 <- data.frame(cbind(crs.org[,3:4], unlist(dat[dat$X==20120401, 2:19])))
colnames(gpdat4)[3] <- "obs"
coordinates(gpdat4) = c("Long", "Lat")
proj4string(gpdat4) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

gpGrid4 <- data.frame(cbind(llgrid[,1], -llgrid[,2], c(mu.u4)))
colnames(gpGrid4) <- c("Lat", "Long", "Pred")
coordinates(gpGrid4) = c("Long", "Lat")
proj4string(gpGrid4) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
gridded(gpGrid4) <- TRUE

im4<-as.image.SpatialGridDataFrame(gpGrid4['Pred'])
cl4<-ContourLines2SLDF(contourLines(im4))
proj4string(cl4) <-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

tcolpal4 <- brewer.pal(9,"Reds")[-c(1:3)]
tcolpal4 <- tcolpal4[c(2:6, 1)]

setwd('E:/STAT547L/MidTerm/report/')
gmStat4 <- plotGoogleMaps(gpdat4, add=TRUE, zcol="obs", , mapTypeId='ROADMAP', layerName="Observation")
#Map of the stations
orCl4<- plotGoogleMaps(cl4, previousMap=gmStat4, zcol='level', filename='Q3-4.htm', colPalette=tcolpal4,
			strokeWeight = 2, mapTypeId='ROADMAP', layerName='LeZidek Prediction') 
#Map of contour, Apr

gpdat7 <- data.frame(cbind(crs.org[,3:4], unlist(dat[dat$X==20120701, 2:19])))
colnames(gpdat7)[3] <- "obs"
coordinates(gpdat7) = c("Long", "Lat")
proj4string(gpdat7) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

gpGrid7 <- data.frame(cbind(llgrid[,1], -llgrid[,2], c(mu.u7)))
colnames(gpGrid7) <- c("Lat", "Long", "Pred")
coordinates(gpGrid7) = c("Long", "Lat")
proj4string(gpGrid7) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
gridded(gpGrid7) <- TRUE

im7<-as.image.SpatialGridDataFrame(gpGrid7['Pred'])
cl7<-ContourLines2SLDF(contourLines(im7))
proj4string(cl7) <-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

tcolpal7 <- brewer.pal(9,"Reds")[-c(1:3)]
#tcolpal <- tcolpal[c(2:6, 1)]

gmStat7 <- plotGoogleMaps(gpdat7, add=TRUE, zcol="obs", , mapTypeId='ROADMAP', layerName="Observation")
#Map of the stations
orCl7<- plotGoogleMaps(cl7, previousMap=gmStat7, zcol='level', filename='Q3-7.htm', colPalette=tcolpal7,
			strokeWeight = 2, mapTypeId='ROADMAP', layerName='LeZidek Prediction') 
#Map of contour, Apr


pdf('E:/STAT547L/MidTerm/report/Q3-bgrid.pdf', width=8, height=3)
par(mfrow=c(1, 3), mar=c(1, 1, 3,1))
Tgrid  = bgrid(start=c(0,0), xmat=scoord , coef=m4Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord, labels = c(1:dim(scoord)[1]))
draw(Tgrid, fs=T)
title("Q3-g: Apr 1st", cex=.7)
Tgrid  = bgrid(start=c(0,0), xmat=scoord , coef=m7Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord, labels = c(1:dim(scoord)[1]))
draw(Tgrid, fs=T)
title("Q3-g: July 1st", cex=.7)

Tgrid  = bgrid(start=c(1,1), xmat=scoord , coef=Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord,labels = c(1:(dim(scoord)[1])))
draw(Tgrid, fs=T)
title("Q3-h: Le-Zidek", cex=.7)
dev.off()


########################
#R Code first
#Question Order 3, 4, 8, 9, 10, 5
#Next, Winbugs models for Question 5
#Next, Winbugs script for Question 3
##############################################################################
##############################################################################
#############################Q9###############################################
##Question 9
dat <- read.csv("E:/STAT547L/MidTerm/Data/MaxCaliforniaTemp.csv", header=T)
crs.org <-  read.table("E:/STAT547L/MidTerm/Data/metadataCA.txt", header=T)
dyn.load("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/SG-method/SG.dll")
dyn.load("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Design/design.1.0.dll")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/SG-method/SG.1.0.1.r")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Para.est/LZ-EM.est.1.0.1.r")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Design/LZ-design.1.0.1.r")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Pred.dist/LZ-predict.1.0.1.r")
###Question 2
library(sp)
library(rgdal)

month <- 1:13
day <- 1:28
dgrid <- expand.grid(d=day, m=month)
dat <- dat[-365, ]
ZZ <-  model.matrix(~as.factor(dgrid$m))

em.fit = staircase.EM(dat[,2:19],p=1,covariate= ZZ,maxit=200,tol=.000001, verbose=T)
cov.est = em.fit$Psi[[1]]
dim1= dim(cov.est)[1]
dim2= dim(em.fit$Omega)[1]
corr.est = cov.est / sqrt( matrix(diag(cov.est),dim1,dim1)*t(matrix(diag(cov.est),dim1,dim1)) )

crs.lam <- Flamb2(abs(crs.org[, 3:4])) #the projected coordinates of the original 18 stations
ldist <- Fdist(crs.lam$xy)
disp = 2-2*corr.est
scoord  = crs.lam$xy/10 

X11()
par(mfrow=c(1, 2))
sg.est = Falternate3(disp, scoord, max.iter=100,alter.lim=100, model=1) 

apply(scoord, 2, range)
coords.grid = Fmgrid(range(scoord[,1]), range(scoord[,2]))
par(mfrow=c(1,2))
temp = setplot(scoord, ax=T)  
deform  = Ftransdraw(disp=disp, Gcrds=scoord, MDScrds=sg.est$ncoords, gridstr=coords.grid)

Tspline = sinterp(scoord, sg.est$ncoords, lam = 50 )


##########Need lay to find out the points inside California
#Map base
state <- readOGR(dsn="E:/STAT547L/MidTerm/state010", layer="statep010")
cal <- state[state$STATE=="California", ]

#Prepare the original points
crs.org.sp <- crs.org
coordinates(crs.org.sp) <- ~Long + Lat
#The new data grid
ngrid <-  15
lat10 = seq(32, 42,length=ngrid)
long10 = seq(113, 125,length=ngrid)
ogrid =  cbind(rep(lat10,ngrid),c(outer(rep(1,ngrid),long10)))

ogrid.sp <- data.frame(Long=-ogrid[,2], Lat=ogrid[,1])
coordinates(ogrid.sp) <- ~Long + Lat
#Subset the points in Oregon
oo <- overlay(ogrid.sp, cal)
ogrid.sp.inCA <- ogrid.sp[!is.na(oo), ]
llgrid <- ogrid[!is.na(oo), ]

(u= sum(!is.na(oo))) # number of new locations inside California
p=1

z = crs.lam
newcrds.lamb = Flamb2(llgrid,latrf1=z$latrf1, latrf2=z$latrf2, latref=z$latref, lngref=z$lngref)$xy/10

#Combine the new locations and stations together begining with new locations
allcrds = rbind(newcrds.lamb,scoord)
corr.est = corrfit(allcrds, Tspline = Tspline, sg.fit  = sg.est, model = 1)
# Non-homogeneous and interpolating using the same thin-plate spline
Tspline.var = sinterp(allcrds[(u+1):(u+18),],matrix(diag(cov.est),ncol=1),lam=50)

varfit = seval(allcrds,Tspline.var)$y
temp = matrix(varfit,length(varfit),length(varfit))
covfit = corr.est$cor * sqrt(temp * t(temp))

hyper.est = staircase.hyper.est(emfit= em.fit,covfit=covfit,u =u, p=1)

nsel = 5
yy = ldet.eval((hyper.est$Lambda.0+ t(hyper.est$Lambda.0))/2,nsel,all =F)

$coord.sel
[1]  3 20 44 46 66

X11()

pdf("E:/STAT547L/fp/report/Q9.pdf", width=5, height=5)
par(mar=c(1, 1, 1, 1))
plot(cal)
plot(ogrid.sp.inCA, add=T, pch=17, col="skyblue")
plot(crs.org.sp, add=T, pch=15, cex=1)
plot(ogrid.sp.inCA[yy$coord.sel], add=T, pch=19, col="red", cex=1.5)
legend("topright", legend=c("Original Site", "Candidate Site", "Recommended Site"), 
col=c(1, "skyblue", "red"), text.col=c(1, "skyblue", "red"), bty="n", pt.cex=c(1, 1, 1.5),
pch=c(15, 17,  19))
dev.off()

covvec <- diag(covfit)
X11()
plot(cal)
#text(ogrid.sp.inCA, text=, col="skyblue")
plot(crs.org.sp, pch=as.character(round(covvec[c(u+1:u+18)]/100)), add=T)
plot(ogrid.sp.inCA[yy$coord.sel], add=T, pch=19, col="red", cex=1.5)
legend("topright", legend=c("Original Site", "Canddiate Site", "Recommended Site"), 
col=c(1, "skyblue", "red"), text.col=c(1, "skyblue", "red"), bty="n", pt.cex=c(1, 1, 1.5),
pch=c(15, 17,  19))

plot(1:19, pch=1:19)

#############################Plot them on Google Map
library(plotGoogleMaps)
library(RColorBrewer)

gpdat4 <- data.frame(cbind(crs.org[,3:4], unlist(dat[dat$X==20120401, 2:19])))
colnames(gpdat4)[3] <- "obs"
coordinates(gpdat4) = c("Long", "Lat")
proj4string(gpdat4) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

gpGrid4 <- data.frame(cbind(llgrid[,1], -llgrid[,2], c(mu.u4)))
colnames(gpGrid4) <- c("Lat", "Long", "Pred")
coordinates(gpGrid4) = c("Long", "Lat")
proj4string(gpGrid4) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
gridded(gpGrid4) <- TRUE

im4<-as.image.SpatialGridDataFrame(gpGrid4['Pred'])
cl4<-ContourLines2SLDF(contourLines(im4))
proj4string(cl4) <-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

tcolpal4 <- brewer.pal(9,"Reds")[-c(1:3)]
tcolpal4 <- tcolpal4[c(2:6, 1)]

setwd('E:/STAT547L/MidTerm/report/')
gmStat4 <- plotGoogleMaps(gpdat4, add=TRUE, zcol="obs", , mapTypeId='ROADMAP', layerName="Observation")
#Map of the stations
orCl4<- plotGoogleMaps(cl4, previousMap=gmStat4, zcol='level', filename='Q3-4.htm', colPalette=tcolpal4,
			strokeWeight = 2, mapTypeId='ROADMAP', layerName='LeZidek Prediction') 
#Map of contour, Apr

gpdat7 <- data.frame(cbind(crs.org[,3:4], unlist(dat[dat$X==20120701, 2:19])))
colnames(gpdat7)[3] <- "obs"
coordinates(gpdat7) = c("Long", "Lat")
proj4string(gpdat7) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

gpGrid7 <- data.frame(cbind(llgrid[,1], -llgrid[,2], c(mu.u7)))
colnames(gpGrid7) <- c("Lat", "Long", "Pred")
coordinates(gpGrid7) = c("Long", "Lat")
proj4string(gpGrid7) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
gridded(gpGrid7) <- TRUE

im7<-as.image.SpatialGridDataFrame(gpGrid7['Pred'])
cl7<-ContourLines2SLDF(contourLines(im7))
proj4string(cl7) <-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

tcolpal7 <- brewer.pal(9,"Reds")[-c(1:3)]
#tcolpal <- tcolpal[c(2:6, 1)]

gmStat7 <- plotGoogleMaps(gpdat7, add=TRUE, zcol="obs", , mapTypeId='ROADMAP', layerName="Observation")
#Map of the stations
orCl7<- plotGoogleMaps(cl7, previousMap=gmStat7, zcol='level', filename='Q3-7.htm', colPalette=tcolpal7,
			strokeWeight = 2, mapTypeId='ROADMAP', layerName='LeZidek Prediction') 
#Map of contour, Apr


pdf('E:/STAT547L/MidTerm/report/Q3-bgrid.pdf', width=8, height=3)
par(mfrow=c(1, 3), mar=c(1, 1, 3,1))
Tgrid  = bgrid(start=c(0,0), xmat=scoord , coef=m4Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord, labels = c(1:dim(scoord)[1]))
draw(Tgrid, fs=T)
title("Q3-g: Apr 1st", cex=.7)
Tgrid  = bgrid(start=c(0,0), xmat=scoord , coef=m7Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord, labels = c(1:dim(scoord)[1]))
draw(Tgrid, fs=T)
title("Q3-g: July 1st", cex=.7)

Tgrid  = bgrid(start=c(1,1), xmat=scoord , coef=Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord,labels = c(1:(dim(scoord)[1])))
draw(Tgrid, fs=T)
title("Q3-h: Le-Zidek", cex=.7)
dev.off()

