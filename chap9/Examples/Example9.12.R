###########################################################################################
### Example 9.12 - Fitting an SPDE model using R–INLA: black smoke monitoring locations ###
###                in the UK                                                            ###
###########################################################################################

### Field std.dev. for theta=0 
sigma0 = 1 

### find the range of the location data 
size = min(c(diff(range(mesh$loc[,1])), diff(range(mesh$loc[,2])))) 

### A fifth of the approximate domain width. 
range0 = size/5 
kappa0 = sqrt(8)/range0
tau0 = 1/(sqrt(4*pi)*kappa0*sigma0) 
spde = inla.spde2.matern(mesh, B.tau=cbind(log(tau0), -1, +1), 
B.kappa=cbind(log(kappa0), 0, -1), theta.prior.mean=c(0,0),constr=TRUE) 

### Running in INLA
formula = logbs ~ 1+ urban.rural +f(site, model=spde) 
model = inla(formula , family="gaussian", data = BSdata, control.predictor = list(compute=TRUE), control.compute = list(dic = TRUE, config=TRUE)) 
