#####################################################################
### Example 8.5. Fitting a conditional spatial model using R–INLA ###
#####################################################################

# requires INLA 
library(INLA) 

# Create the neighbourhood matrix 
W.nb <- poly2nb(SMRspatial , row.names = rownames(SMR)) 
W.list <- nb2listw(W.nb, style="B") 
W.mat <- nb2mat(W.nb, style="B") 

# Convert the adjacency matrix 
into a file in the INLA format 
nb2INLA("UK.adj", W.nb) 

# Create areas IDs to match the values in UK.adj 
data=as.data.frame(cbind(Y, E)) 
data$ID <- 1:324 

# Fun the INLA model 
m1<-inla(Y~f(ID, model="besag", graph="UK.adj"), family="poisson", E=E,data = data, control.predictor=list(compute=TRUE)) 
