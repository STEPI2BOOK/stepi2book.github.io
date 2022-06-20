#############################################################
### Example 8.4. Fitting a conditional spatial model in R ###
#############################################################

# requires CARBAyes, spdep and shapefiles libraries 
library(CARBAyes)
library(spdep)
library(shapefiles)

# read in the shape file 
shp <- read.shp(shp.name="england local authority.shp") 

# read in the details of the areas 
dbf <- read.dbf(dbf.name="england local authority.dbf") 

# calculate the SMRs and combine with the spatial information 
SMR <- Y/E
SMRspatial <- combine.data.shapefile(SMR, shp, dbf) 

# Create the neighbourhood matrix 
W.nb <- poly2nb(SMRspatial , row.names = rownames(SMR)) W.list <-
nb2listw(W.nb, style="B") 
W.mat <- nb2mat(W.nb, style="B") 

# Fit a CAR smoothing model 
formula <- observed~offset(log(expected)) 
model <- iarCAR.re(formula=formula , family="poisson", W=W.mat, burnin=20000, n.sample=100000, thin=10) 
risk <- model$fitted.values[ ,1] / expected 
