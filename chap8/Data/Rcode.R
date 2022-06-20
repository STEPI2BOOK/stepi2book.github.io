####################################################
#### Modelling and analysis of the England COPD data
####################################################
library(spdep)
library(shapefiles)
library(CARBayes)

setwd("/Users/Gavin/Dropbox/DuncFiles/gavfiles")


##############################
#### Read in and plot the data
##############################
### Read in the data
observed <- read.csv(file="copd mortality.csv", row.names=1)
expected <- read.csv(file="copd mortality expected counts.csv", row.names=1)
SMR <- observed[ ,-1]/expected
colnames(SMR) <- c("SMR2001", "SMR2002", "SMR2003", "SMR2004", "SMR2005", "SMR2006", "SMR2007", "SMR2008", "SMR2009", "SMR2010")

dbf <- read.dbf(dbf.name="england local authority.dbf")
dbf$dbf <- dbf$dbf[ ,c(2,1,3:7)]
shp <- read.shp(shp.name="england local authority.shp")
shp$shp[[23]]$points <- shp$shp[[23]]$points[-c(460, 461, 462, 463, 464, 465), ]
## Gav - this last line removes points in the shapefile for bristol which has
## an odd bit that sticks out into the bristol channel.


### Plot the SMR
SMRspatial <- combine.data.shapefile(SMR, shp, dbf)
range <- seq(min(SMR$SMR2010)-0.01, max(SMR$SMR2010)+0.01, length.out=11)
n.col <- length(range)-1
spplot(SMRspatial, zcol=c("SMR2010"),scales=list(draw=TRUE), 
     xlab="Easting", ylab="Northing", at=range, col="transparent",
     col.regions=hsv(0.6, seq(0.2, 1, length.out=n.col), 1))


### Create the neighbourhood matrix
W.nb <- poly2nb(SMRspatial, row.names = rownames(SMR))
W.list <- nb2listw(W.nb, style="B")
W.mat <- nb2mat(W.nb, style="B")


#### Fit a Leroux CAR smoothing model
formula <- observed$Y2010~offset(log(expected$E2010))
model <- lerouxCAR.re(formula=formula, family="poisson", W=W.mat,
     burnin=20000, n.sample=100000, thin=10)
print(model)
plot(model$samples$rho)
plot(model$samples$beta)
plot(model$samples$tau2)

risk <- model$fitted.values[ ,1] / expected$E2010
SMRspatial@data$risk <- risk
range <- seq(min(SMR$SMR2010)-0.01, max(SMR$SMR2010)+0.01, length.out=11)
n.col <- length(range)-1
spplot(SMRspatial, zcol=c("SMR2010", "risk"),scales=list(draw=TRUE), 
     xlab="Easting", ylab="Northing", at=range, col="transparent",
     col.regions=hsv(0.6, seq(0.2, 1, length.out=n.col), 1))
