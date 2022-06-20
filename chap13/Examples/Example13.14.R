
### The libraries
library(EnviroStat)
library(sp)
library(rgdal)
library(ggmap)

### The data
dat <- read.csv("MaxCaliforniaTemp.csv", header=T)
crs.org <-  read.table("metadataCA.txt", header=T)

### Now we plot the sites
Cal.sp = crs.org
coordinates(Cal.sp) = ~Long+Lat
proj4string(Cal.sp) = CRS("+proj=longlat 
        +ellps=WGS84")
latLongBox = bbox(Cal.sp)
location = c(-126, 32, -114, 42)
CaliforniaMap = get_map(location = location, source = "google", maptype = "roadmap")
CaliforniaMap = ggmap(CaliforniaMap)
CaliforniaMap = CaliforniaMap + geom_point(data = metadata, aes(x = Long, y = Lat), size = 3, col = 2) + xlab("Longitude") + ylab("Latitude")
            
month <- 1:13day <- 1:28
dgrid <- expand.grid(d=day, m=month)
dat <- dat[-365, ]
ZZ <-  model.matrix(~as.factor(dgrid\$m))

X11()
par(mfrow=c(1, 2))
### First approach
sg.est = Falternate3(disp, scoord, max.iter=100,
+ alter.lim=100, model=1) 

### Second approach
apply(scoord, 2, range)
coords.grid = Fmgrid(range(scoord[,1]), range(scoord[,2]))
par(mfrow=c(1,2))
temp = setplot(scoord, ax=T)  
deform  = Ftransdraw(disp=disp, Gcrds=scoord, MDScrds=sg.est\$ncoords, gridstr=coords.grid)

Tspline = sinterp(scoord, sg.est\$ncoords, lam = 50 )

Tspline.var = sinterp(allcrds[(u+1):(u+18),],matrix(diag(cov.est),ncol=1),lam=50)
###
varfit = seval(allcrds,Tspline.var)\$y
temp = matrix(varfit,length(varfit),length(varfit))
covfit = corr.est\$cor * sqrt(temp * t(temp))
###
hyper.est = staircase.hyper.est(emfit= em.fit,covfit=covfit,u =u, p=1)
###
nsel = 5
yy = ldet.eval((hyper.est\$Lambda.0+ t(hyper.est\$Lambda.0))/2,nsel,all =F)