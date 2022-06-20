##################################################################################
### Thanks go to Yang Liu for permission to include his code used to model the ###
### California's temperature field for Question 3 in a project assigned for    ###
### a course co-instucted by Gavin Shaddick and James V Zidek in 2013.         ###
##################################################################################

dat <- read.csv("E:/STAT547L/MidTerm/Data/MaxCaliforniaTemp.csv", header=T)
adat <- unlist(c(dat[,-1]))
hist(adat, breaks=20)

# Slightly right skewed
hist(adat^1/3, breaks=20)
hist(log(adat+100), breaks=20)

# Not helpful
# Maybe no transformation is needed
hist(log(dat[,2]), breaks=20)
hist(log(dat[,4]), breaks=20)
hist(dat[,4], breaks=20)

month <- 1:13
day <- 1:28
dgrid <- expand.grid(d=day, m=month)
dat <- dat[-365, ]
dat$m<- dgrid$m
dat$d <- dgrid$d
mon.mean <- matrix(NA, 13, 18)

for (i in 1:13) {
	tdt <- dat[dat$m==i, 2:19]
	for (j in 1:18){
		mon.mean[i, j] <- mean(tdt[, j])
	}
}
grand.mean <-apply(mon.mean, 1, mean)
mon.res <- mon.mean - grand.mean
colnames(mon.res) <- colnames(dat[,2:19])

# Open PDF file
pdf("Q3-b.pdf", width=12, height=5)
par(mfrow=c(1, 3), mar=c(4, 3, 5, 1))
# Boxplot Monthly average Residuals
boxplot(mon.res, main=" Boxplot of Monthly Average Residuals \n at each station", xlab="Station")
abline(h=0, col="red", lwd=2)
# Plot monthly residuals SanFran
plot(mon.res[, 1], main="Monthly Residuals at San Francisco", xlab="Month", pch=15)
abline(h=0, col="red", lwd=2)
# PLot monthly residuals Death Valley
plot(mon.res[, 6], main="Monthly Residuals at Death Valley", xlab="Month", pch=15)
abline(h=0, col="red", lwd=2)
dev.off()

dat.res <- dat[, 2:19]
for (i in 1:nrow(dat)){
	for (j in 1:18){
		dat.res[i, j] <- dat.res[i, j] - mon.mean[dat$m[i], j]
	}
}

pdf("Q3-c.pdf", width=8, height=4)
par(mfrow=c(1, 2), mar=c(4, 4, 4, 1))
plot(dat.res[,1], type="l", xlab="Time", ylab="Residuals", main="San Francisco")
plot(dat.res[,6], type="l", xlab="Time", ylab="Residuals", main="Death Valley")
dev.off()
#########################
### End of a, b and c ###
#########################

crs.org <-  read.table("E:/STAT547L/MidTerm/Data/metadataCA.txt", header=T)
dyn.load("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/SG-method/SG.dll")
dyn.load("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Design/design.1.0.dll")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/SG-method/SG.1.0.1.r")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Para.est/LZ-EM.est.1.0.1.r")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Design/LZ-design.1.0.1.r")
source("E:/STAT547L/MidTerm/EnviRo.stat.1.0.1/Pred.dist/LZ-predict.1.0.1.r")

ttt <-crs.org[, 5:6]
colnames(ttt) <- colnames(crs.org)[3:4]
crs.oa <- rbind(crs.org[, 3:4], ttt)
crs.lam <- Flamb2(abs(crs.org[, 3:4])) #the projected coordinates of the original 18 stations
crs.oa.lam <- Flamb2(abs(crs.oa)) #with the reference
tdist = Fdist(crs.oa.lam$xy)
ev.ind <- cbind(1:18, 1:18+18)
ev.dist <- tdist[ev.ind]

### Regression Model for Apr 1st
o0401 <-unlist(dat[dat$X==20120401, 2:19])
d0401 <- cbind(o0401, crs.org$Elev, ev.dist, crs.lam$xy)
colnames(d0401) <- c("Temp", "Elev", "Dist", "x", "y")
d0401 <- data.frame(d0401)

lm41 <- lm(Temp~Elev + Dist + I(Elev* Dist) + x + y + I(Dist^2) + I(Elev^2), data=d0401)
flm41 <- stepAIC(lm41)
lm42 <- lm(Temp~Dist + I(Elev * Dist) +x+ y+ I(Dist^2) + I(Elev^2) + I(Dist^3) + I(Elev^3), data=d0401)
flm42 <- stepAIC(lm42)
lm43 <- lm(Temp~Dist + I(Elev * Dist) +x+ y+ I(Dist^4) + I(Elev^4), data=d0401)
flm43 <- stepAIC(lm43)
lm44 <- lm(Temp~Dist + I(Elev * Dist) +x+ y+ I(Dist^5) + I(Elev^5), data=d0401)
flm44 <- stepAIC(lm44)

AIC(flm41, flm42, flm43, flm44 )
#Final model for Apr 1st
flm4 <- flm43

pdf("E:/STAT547L/MidTerm/report/Q3-flm4.pdf", width=10, height=4)
par(mfrow=c(1, 3), mar=c(4, 4, 4, 1))
plot(flm4$residual~d0401$Temp, pch=15, cex=1.5)
plot(flm4$residuals, pch=15, cex=1.5)
qqnorm(flm4$residuals, pch=15, cex=1.5)
qqline(flm4$residuals, lwd=2, col="red")
dev.off()
xtable(summary(flm4), display=c("d", "E", "E", "f", "f"))


### Regression Model for July 1st
o0701 <-unlist(dat[dat$X==20120701, 2:19])
d0701 <- cbind(o0701, crs.org$Elev, ev.dist, crs.lam$xy)
colnames(d0701) <- c("Temp", "Elev", "Dist", "x", "y")
d0701 <- data.frame(d0701)

lm71 <- lm(Temp~Elev + Dist + I(Elev* Dist) + x + y + I(Dist^2) + I(Elev^2), data=d0701)
flm71 <- stepAIC(lm71)
lm72 <- lm(Temp~Dist + I(Elev * Dist) +x+ y+ I(Dist^2) + I(Elev^2) + I(Dist^3) + I(Elev^3), data=d0701)
flm72 <- stepAIC(lm72)
lm73 <- lm(Temp~Dist + I(Elev * Dist) +x+ y+ I(Dist^4) + I(Elev^4), data=d0701)
flm73 <- stepAIC(lm73)
lm74 <- lm(Temp~Dist + I(Elev * Dist) +x+ y+ I(Dist^5) + I(Elev^5), data=d0701)
flm74 <- stepAIC(lm74)
AIC(flm71, flm72, flm73, flm74 )

flm7 <- flm71
xtable(summary(flm7), display=c("d", "E", "E", "f", "f"))

pdf("E:/STAT547L/MidTerm/report/Q3-flm7.pdf", width=10, height=4)
par(mfrow=c(1, 3), mar=c(4, 4, 4, 1))
plot(flm7$residual~d0701$Temp, pch=15, cex=1.5)
plot(flm7$residuals, pch=15, cex=1.5)
qqnorm(flm7$residuals, pch=15, cex=1.5)
qqline(flm7$residuals, lwd=2, col="red")
dev.off()


### Question G whether stationary
### Question 2
library(sp)
library(rgdal)
library(spatial)
library(geoR)
library(gstat)

r4dat <- cbind(flm4$residuals, crs.lam$xy)
colnames(r4dat)[1] <- "Resd"
r4dat <- data.frame(r4dat)
coordinates(r4dat) <-~x+y
r4v <- variogram(Resd~1, r4dat, alpha=c(0,45,90,135) )
r4v.fit <- fit.variogram(r4v, vgm(1, "Gau", 600, 1))
pdf("E:/STAT547L/MidTerm/report/Q3-gdiag4.pdf", width=5, height=5)
plot(r4v, model=r4v.fit, as.table=TRUE, main="Apr 1st")
dev.off()

r7dat <- cbind(flm7$residuals, crs.lam$xy)
colnames(r7dat)[1] <- "Resd"
r7dat <- data.frame(r7dat)
coordinates(r7dat) <-~x+y
r7v <- variogram(Resd~1, r7dat, alpha=c(0,45,90,135) )
r7v.fit <- fit.variogram(r7v, vgm(1, "Gau", 600, 1))

pdf("E:/STAT547L/MidTerm/report/Q3-gdiag7.pdf", width=5, height=5)
plot(r7v, model=r7v.fit, as.table=TRUE, main="July 1st")
dev.off()

##Q3 Part G, h
library(forecast)
library(plotGoogleMaps)
library(RColorBrewer)
m4dat <- dat[dat$m==4, 2:19]
#Remove the spatial mean
for (i in 1:nrow(m4dat))
	m4dat[i, ] <- m4dat[i, ]- flm4$fitted
m4davg <- apply(m4dat, 1, mean) #Daily average
for (i in 1:18)
	m4dat[, i] <- m4dat[, i] - m4davg
plot(m4dat[,1], type="b")

m4resd <- m4dat
#For the residuals, remove temporal trend separately
for (i in 1:18)
{
	tarima <- auto.arima(m4dat[, i])
	m4resd[, i]<- tarima$residuals
}
m4cor <- cor(m4resd)
m4disp <- 2- 2*m4cor
#The SG method

scoord <- crs.lam$xy/10
X11()
m4sgest = Falternate3(m4disp, scoord ,max.iter=100,alter.lim=100, model=2)

apply(scoord, 2, range)
#coords.grid = Fmgrid(range(crd7.lamb[,1]), range(crd7.lamb[,2]))
coords.grid = Fmgrid(range(scoord[,1]),  range(scoord[,2]) )

par(mfrow=c(1,2), mar=c(4, 3, 4, 1))
junk = setplot(scoord, ax=T)
deform4  = Ftransdraw(disp=m4disp, Gcrds=scoord ,MDScrds=m4sgest$ncoords,
                       gridstr=coords.grid)
					   
m4Tspline = sinterp( scoord, m4sgest$ncoords, lam = 35 )

X11()
par(mfrow=c(1, 1), mar=c(1, 1, 1,1))
Tgrid  = bgrid(start=c(0,0), xmat=scoord , coef=m4Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord, labels = c(1:dim(scoord)[1]))
draw(Tgrid, fs=T)

#################Kriging

#Prepare the original data in the D space and the vgm
m4Ddata <- data.frame(cbind(deform4$Dcrds, o0401))
colnames(m4Ddata) <- c("x", "y", "obs")
coordinates(m4Ddata) <- ~x + y
m4variog <- vgm(psill=m4sgest$variogfit$a[2],"Gau", range=1/m4sgest$variogfit$t0, nugget=m4sgest$variogfit$a[1])
m4gstat <- gstat(id="obs", formula=obs~1, data=m4Ddata, model=m4variog)

#Prepare the grid in the original space
lat10 = seq(min(crs.org[, 3]),max(crs.org[, 3]),length=50)
long10 = seq(max(abs(crs.org[, 4])),min(abs(crs.org[, 4])),length=50)
llgrid =  cbind(rep(lat10,50),c(outer(rep(1,50),long10)))
z = crs.lam
llgrid.lam = Flamb2(llgrid,latrf1=z$latrf1, latrf2=z$latrf2, latref=z$latref, lngref=z$lngref)$xy/10
Dkg.grid <- data.frame(t(seval(llgrid.lam, m4Tspline)$y))
colnames(Dkg.grid) <- c("x", "y")
coordinates(Dkg.grid) <-  c("x", "y")

m4Dpred <- predict(m4gstat, newdata=Dkg.grid)
##Kriging here

m4kgres <- data.frame(cbind(llgrid[,1], -llgrid[,2], m4Dpred$obs.pred,  m4Dpred$obs.var))
colnames(m4kgres) <- c("Lat", "Long", "Pred", "Var")
coordinates(m4kgres) <- c("Long", "Lat")
gridded(m4kgres) <- TRUE

im4<-as.image.SpatialGridDataFrame(m4kgres['Pred'])
cl4<-ContourLines2SLDF(contourLines(im4, nlevels=10))
proj4string(cl4) <-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

gpdat4 <- data.frame(cbind(crs.org[,3:4], unlist(dat[dat$X==20120401, 2:19])))
colnames(gpdat4)[3] <- "obs"
coordinates(gpdat4) = c("Long", "Lat")
proj4string(gpdat4) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

setwd('E:/STAT547L/MidTerm/report/')

colpal4 <- c(rev(brewer.pal(9, "YlGn"))[1:4], brewer.pal(9, "Reds"))
colpal4 <- colpal4[c(3:13, 1, 2)]

gmStat4 <- plotGoogleMaps(gpdat4, add=TRUE, zcol="obs", , mapTypeId='ROADMAP', layerName="Observation")
#Map of the stations
orCl4<- plotGoogleMaps(cl4, previousMap=gmStat4, zcol='level', filename='Q3-g4.htm', colPalette=colpal4,
			strokeWeight = 2, mapTypeId='ROADMAP', layerName='Kriging Prediction') 
#Map of contour, Apr


###########################################################
X11()
pdf("E:/STAT547L/MidTerm/report/Q3-g4.pdf", width=4, height=5)
par(mar=c(1, 1, 1, 1))
image(m4kgres, col=terrain.colors(30))
contour(m4kgres, add=TRUE, drawlabels=TRUE, col='brown')
title('Question g: Kriging Prediction for Apr 1st')
dev.off()
#	t(seval(t(Gcrds), Tspline)$y)
#ttest <- t(seval(t(crs.lam$xy/10), m4Tspline)$y)
################################################################


m7dat <- dat[dat$m==7, 2:19]
#Remove the spatial mean
for (i in 1:nrow(m7dat))
	m7dat[i, ] <- m7dat[i, ]- flm7$fitted
m7davg <- apply(m7dat, 1, mean) #Daily average
for (i in 1:18)
	m7dat[, i] <- m7dat[, i] - m7davg
plot(m7dat[,1], type="b")

m7resd <- m7dat
#For the residuals, remove temporal trend separately
for (i in 1:18)
{
	tarima <- auto.arima(m7dat[, i])
	m7resd[, i]<- tarima$residuals
}
m7cor <- cor(m7resd)
m7disp <- 2- 2*m7cor
#The SG method

scoord <- crs.lam$xy/10
X11()
m7sgest = Falternate3(m7disp, scoord ,max.iter=100,alter.lim=100, model=2)

#m7sgest2 = Falternate3(m7disp, scoord ,max.iter=100,alter.lim=100, model=1)
#Still the Gaussian is better.

apply(scoord, 2, range)
#coords.grid = Fmgrid(range(crd7.lamb[,1]), range(crd7.lamb[,2]))
coords.grid = Fmgrid(range(scoord[,1]),  range(scoord[,2]) )

par(mfrow=c(1,2), mar=c(4, 3, 4, 1))
junk = setplot(scoord, ax=T)
deform7  = Ftransdraw(disp=m7disp, Gcrds=scoord ,MDScrds=m7sgest$ncoords,
                       gridstr=coords.grid)
					   
m7Tspline = sinterp( scoord, m7sgest$ncoords, lam =45 )

X11()
Tgrid  = bgrid(start=c(0,0), xmat=scoord , coef=m7Tspline$sol)
tempplot = setplot(scoord, ax=T)
text(scoord, labels = c(1:dim(scoord)[1]))
draw(Tgrid, fs=T)

#################Kriging

#Prepare the original data in the D space and the vgm
m7Ddata <- data.frame(cbind(deform7$Dcrds, o0701))
colnames(m7Ddata) <- c("x", "y", "obs")
coordinates(m7Ddata) <- ~x + y
m7variog <- vgm(psill=m7sgest$variogfit$a[2],"Gau", range=1/m7sgest$variogfit$t0, nugget=m7sgest$variogfit$a[1])
m7gstat <- gstat(id="obs", formula=obs~1, data=m7Ddata, model=m7variog)

#Prepare the grid in the original space
lat10 = seq(min(crs.org[, 3]),max(crs.org[, 3]),length=50)
long10 = seq(max(abs(crs.org[, 4])),min(abs(crs.org[, 4])),length=50)
llgrid =  cbind(rep(lat10,50),c(outer(rep(1,50),long10)))
z = crs.lam
llgrid.lam = Flamb2(llgrid,latrf1=z$latrf1, latrf2=z$latrf2, latref=z$latref, lngref=z$lngref)$xy/10
Dkg.grid <- data.frame(t(seval(llgrid.lam, m7Tspline)$y))
colnames(Dkg.grid) <- c("x", "y")
coordinates(Dkg.grid) <-  c("x", "y")

m7Dpred <- predict(m7gstat, newdata=Dkg.grid)
##Kriging here

m7kgres <- data.frame(cbind(llgrid[,1], -llgrid[,2], m7Dpred$obs.pred,  m7Dpred$obs.var))
colnames(m7kgres) <- c("Lat", "Long", "Pred", "Var")
coordinates(m7kgres) <- c("Long", "Lat")
gridded(m7kgres) <- TRUE

im7<-as.image.SpatialGridDataFrame(m7kgres['Pred'])
cl7<-ContourLines2SLDF(contourLines(im7, nlevels=10))
proj4string(cl7) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 


gpdat7 <- data.frame(cbind(crs.org[,3:4], unlist(dat[dat$X==20120701, 2:19])))
colnames(gpdat7)[3] <- "obs"
coordinates(gpdat7) = c("Long", "Lat")
proj4string(gpdat7) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 

setwd('E:/STAT547L/MidTerm/report/')

colpal7 <- c(rev(brewer.pal(9, "YlGn"))[1:4], brewer.pal(9, "Reds"))
colpal7 <- colpal7[c(3:13, 1, 2)]

gmStat7 <- plotGoogleMaps(gpdat7, add=TRUE, zcol="obs", , mapTypeId='ROADMAP', layerName="Observation")
#Map of the stations
orCl7<- plotGoogleMaps(cl7, previousMap=gmStat7, zcol='level', filename='Q3-g7.htm', colPalette=colpal7,
			strokeWeight = 2, mapTypeId='ROADMAP', layerName='Kriging Prediction') 
#Map of contour, Apr

######Q3 Jim's approach
dat <- read.csv("E:/STAT547L/MidTerm/Data/MaxCaliforniaTemp.csv", header=T)
###############################################################################
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
library(spatial)
library(geoR)
library(gstat)

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
#Spatial correlation
round(corr.est,2)

crs.lam <- Flamb2(abs(crs.org[, 3:4])) #the projected coordinates of the original 18 stations
ldist <- Fdist(crs.lam$xy)

par(mfrow=c(1,1))
plot(-.2,0,xlim=c(0,950),ylim=c(-.2,1),xlab="Dist",ylab="Spatial correlation",type="n")
for (i in 1:17) for (j in (i+1):18) points(ldist[i,j],corr.est[i,j])

disp = 2-2*corr.est
plot(-.2,0,xlim=c(0,950),ylim=c(0,2),xlab="Dist",ylab="Dispersion",type="n")
for (i in 1:17) for (j in (i+1):18) points(ldist[i,j],disp[i,j])


h.lt = ldist[row(ldist) < col(ldist)]
disp.lt = disp[row(disp) < col(disp)]	
variogfit <- Fvariogfit3(disp.lt, h.lt, a0=1.5, t0=.1, model=1)
x = seq(min(h.lt),max(h.lt),1)
a0 = variogfit$a[1]
t0 = variogfit$t0
lines(x, a0+(2-a0)*(1-exp(- (t0* x))))

#
scoord  = crs.lam$xy/10 

sg.est = Falternate3(disp, scoord, max.iter=100,alter.lim=100, model=1) 


apply(scoord, 2, range)
coords.grid = Fmgrid(range(scoord[,1]), range(scoord[,2]))
par(mfrow=c(1,2))
temp = setplot(scoord, ax=T)  
deform  = Ftransdraw(disp=disp, Gcrds=scoord, MDScrds=sg.est$ncoords, gridstr=coords.grid)

Tspline = sinterp(scoord, sg.est$ncoords, lam = 50 )

lat10 = seq(min(crs.org[, 3]),max(crs.org[, 3]),length=50)
long10 = seq(max(abs(crs.org[, 4])),min(abs(crs.org[, 4])),length=50)
llgrid =  cbind(rep(lat10,50),c(outer(rep(1,50),long10)))
u = 2500 # number of new locations
p=1

# Project the new locations using the same Lambert project for stations above,
# ie. using the same reference point.
# Note the same scale factor of 10 is used as before
z = crs.lam
newcrds.lamb = Flamb2(llgrid,latrf1=z$latrf1, latrf2=z$latrf2, latref=z$latref, lngref=z$lngref)$xy/10

#Combine the new locations and stations together begining with new locations
allcrds = rbind(newcrds.lamb,scoord)

corr.est = corrfit(allcrds, Tspline = Tspline, sg.fit  = sg.est, model = 1)

diag(cov.est)
# Non-homogeneous and interpolating using the same thin-plate spline
Tspline.var = sinterp(allcrds[(u+1):(u+18),],matrix(diag(cov.est),ncol=1),lam=50)

varfit = seval(allcrds,Tspline.var)$y
temp = matrix(varfit,length(varfit),length(varfit))
covfit = corr.est$cor * sqrt(temp * t(temp))

hyper.est = staircase.hyper.est(emfit= em.fit,covfit=covfit,u =u, p=1)
x = hyper.est

#Get the Predictive Mean of April 1st
which(dat$X==20120401)
#92
tpt4 = 92
Z4 = x$covariate[tpt4,]
y4 = x$data[tpt4,]

b04 = matrix(rep(c(x$Beta0[,1:p]),u),nrow=length(Z4))

# Predictive mean for on Day Apr1st
mu.u4 = Z4 %*% b04 + unlist(y4- Z4 %*% x$Beta0) %*% x$Xi0.0
pred4 = matrix(mu.u4,byrow=T, ncol=length(lat10))


#Get the Predictive Mean of April 1st
which(dat$X==20120701)
tpt7 = 183
Z7 = x$covariate[tpt7,]
y7 = x$data[tpt7,]

b07 = matrix(rep(c(x$Beta0[,1:p]),u),nrow=length(Z7))

# Predictive mean on July 1st
mu.u7 = Z7 %*% b07 + unlist(y7- Z7 %*% x$Beta0) %*% x$Xi0.0
pred7 = matrix(mu.u7,byrow=T, ncol=length(lat10))

X11()
par(mfrow=c(1,2), mar=c(4, 4, 3, 1))
image(-long10,lat10, pred4, xlab="Long", ylab="Lat", main=paste("April 1st"),col=terrain.colors(30))
contour(-long10,lat10, pred4, add=TRUE, drawlabels=TRUE, col='blue', lwd=1.5, labcex=1)
image(-long10,lat10, pred7, xlab="Long", ylab="Lat", main=paste("July 1st"),col=terrain.colors(30))
contour(-long10,lat10, pred7, add=TRUE, drawlabels=TRUE, col='blue', lwd=1.5, labcex=1)

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