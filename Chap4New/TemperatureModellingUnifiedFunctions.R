# FUNCTION: Selecting stations for training and validation purposes
sampleStations = function(stations, u, seed = 8){
   N = nrow(stations) # total number of stations
   g = N-u # number of gauged stations
   set.seed(seed)
   ss = sample(1:N, size = N) # rearranging stations
   idNew  = ss[1:u]
   idSel  = ss[(u+1):(u+g)]
   return(list(gauged = idSel, ungauged = idNew))
}


######################################################################################################################

meanModelling = function(data,idSel){
   
   newData = subset(data, stID %in% idSel) 
   
   # Centering covariates
   newData$lambx = with(newData, scale(lambx, center = T, scale = F))
   newData$lamby = with(newData, scale(lamby, center = T, scale = F))
   newData$ELEVATION = with(newData, scale(ELEVATION, center = T, scale = F))
   
   # Fitting linear model (TMAX/10 in degrees Celsius)
   lmfit   = lm(TMAX/10 ~ lambx*lamby*as.factor(month) + ELEVATION*lambx, 
                data = newData)
   
   # Residual data
   newData$Res  = lmfit$res
   newData$Yhat = lmfit$fitted.values
   
   # Getting month effects coefficients
   montheff = c(0, lmfit$coeff[c("as.factor(month)2", "as.factor(month)3", 
                                 "as.factor(month)4", "as.factor(month)5", 
                                 "as.factor(month)6")]) 
   
   # Add month effects on residuals (detrended data)
   # Ytilde = Y - B_0(s,t)
   newData$Ytilde = with(newData, ifelse(month == 1, Res+montheff[1], 
                                         ifelse(month == 2, Res+montheff[2],
                                                ifelse(month == 3, Res+montheff[3],
                                                       ifelse(month == 4, Res+montheff[4],
                                                              ifelse(month == 5, Res+montheff[5],
                                                                     ifelse(month == 6, Res+montheff[6], NA)))))))
   return(newData = newData)
   
}

######################################################################################################################

EnviroStatModel = function(gaugedData, predData, lambcoords, lambda = 5, PRISM = 0){
   # Data matrix
   Yuni = NULL
   stations = unique(gaugedData$STATION)
   g = length(stations)
   locs = matrix(NA, nrow = g, ncol = 2)
   for (i in 1:g){
      subresdata = subset(gaugedData, STATION == stations[i])
      locs[i,] = c(unique(subresdata$LATITUDE)[1], unique(subresdata$LONGITUDE)[1])
      if(PRISM == 0){
      datatemp = data.frame(subresdata$Ytilde)}
      else{
      datatemp = data.frame(subresdata$anom) 
      }
      Yuni = cbind(Yuni, as.matrix(datatemp))
   }
   
   # Effects in design matrix
   month28 = subresdata$month 
   
   # Z matrix
   ZZ = model.matrix(~as.factor(month28))
   
   # EM 
   em.fit = staircase2014.EM(Yuni, covariate = ZZ, p = 1, maxit = 2000, tol = .000001, verbose = T)  
   
   # Get the marginal correlation matrices ('corr.est': spatial and 'omega': days)
   cov.est = em.fit$Psi[[1]]
   dim1 = dim(cov.est)[1]
   dim2 = dim(em.fit$Omega)[1]
   
   # Spatial correlation
   corr.est = cov.est / sqrt(matrix(diag(cov.est),dim1,dim1)*t(matrix(diag(cov.est),dim1,dim1)))
   
   # Dispersion
   disp = 2-2*corr.est
   
   # Projecting the coordinates using Lambert projection
   coords = Flamb2(abs(locs), latrf1 = lambcoords$latrf1, latrf2 = lambcoords$latrf2,
                   latref = lambcoords$latref, lngref = lambcoords$lngref)
   
   coords.lamb  = coords$xy/100
   
   # D-Plane (Sampson-Guttorp approach here!)
   sg.est = Falternate3(disp, coords.lamb, max.iter=50,alter.lim=50, model=1, a0 = 0.1, t0 = 0.5, verbose = F, dev.mon = NULL)
   
   # Optimizing thin-plate spline 
   Tspline = sinterp(coords.lamb, sg.est$ncoords, lam = lambda) 
   
   # New stations and their locations
   newlocs = unique(predData[,c("LATITUDE", "LONGITUDE")])
   
   # Lambert projection
   newcrds.lamb = Flamb2(abs(newlocs),latrf1=coords$latrf1,latrf2= coords$latrf2,
                         latref=coords$latref,lngref=coords$lngref)$xy/100 
   
   # Combining new station coords and existing 
   allcrds = rbind(newcrds.lamb, coords.lamb)
   
   ################################
   # Interpolating Variance Field #
   ################################
   
   # Estimated covariance matrix
   cov.est = em.fit$Psi[[1]]
   Tspline.var = sinterp(coords.lamb, matrix(diag(cov.est),ncol=1), lam = lambda)
   
   # The estimated correlations for the locations
   corr.fit = corrfit(allcrds, Tspline = Tspline, sg.fit = sg.est, model = 1)
   
   # Variance 
   varfit = seval(allcrds,Tspline.var)$y
   
   # Combining to get the covariance matrix for all stations
   temp = matrix(varfit,length(varfit),length(varfit))
   covfit = corr.fit$cor * sqrt(temp * t(temp))
   
   # Estimating hyperparameters at the new locations
   u = nrow(newlocs) # number of new locations
   p = 1  # dimension of the multivariate response (p=1 is univariate case)
   
   hyper.est = staircase.hyper.est(emfit = em.fit, covfit = covfit, u = u, p = 1)
   
   return(list(em.fit = em.fit, sg.est = sg.est, hyper.est = hyper.est))
   
}


#######################################################################################

# Prediction via simulation

predSimu = function(predData, hyper.est, PRISM = 0){
   
   # Number of days
   days = unique(predData$DATE)
   ndays = length(days)
   
   # Number of ungaged stations
   u = length(unique(predData$STATION))
   p = 1
   
   mu.u       = matrix(NA, nrow = ndays, ncol = u)
   sd.day1    = matrix(NA, nrow = ndays, ncol = u) 
   ypred.day1 = matrix(NA, nrow = ndays, ncol = u) 
   lwrb.day1  = matrix(NA, nrow = ndays, ncol = u) 
   uppb.day1  = matrix(NA, nrow = ndays, ncol = u) 
   yreal.day1 = matrix(NA, nrow = ndays, ncol = u) 
   
   prop = NULL
   for (i in 1:ndays){
      subpreddata = subset(predData, DATE == days[i])
      tpt1 = 1:182
      simu = pred.dist.simul(hyper.est, tpt = tpt1, N = 1000)
      
      # Mean
      mu.u[i,] = apply(simu,2,mean)[1:(u*p)] 
      ypred.day1[i,] = mu.u[i,]
      
      # Variance
      xvar = simu[,1:(u*p)]
      sd.day1[i,] = sqrt(diag(var(xvar)))
      
      # IC
      lwrb.day1[i,]   = ypred.day1[i,] - 2*sd.day1[i,]
      uppb.day1[i,]   = ypred.day1[i,] + 2*sd.day1[i,]
      
      # Observed
      if (PRISM == 0){
         yreal.day1[i,] = subpreddata$Ytilde} 
      else{
         yreal.day1[i,] = subpreddata$anom}
      
      # Proportion out of 95% bounds
      prop[i]  = length(unique(c(which(yreal.day1[i,] < lwrb.day1[i,]), which(yreal.day1[i,] > uppb.day1[i,]))))/u
      print(prop[i])
   }
   
   MSE = NULL
   for (i in 1:u){
      MSE[i]      = mean((yreal.day1[,i]-ypred.day1[,i])^2)
      
   }
   
   MSEday = NULL
   for (i in 1:182){
      MSEday[i] = mean((yreal.day1[i,]-ypred.day1[i,])^2)
   }
   
   return(list(prop = prop, MSE = MSE, MSEday = MSEday, 
               mu.u = mu.u, sd.day1 = sd.day1, ypred.day1 = ypred.day1, 
               lwrb.day1 = lwrb.day1, uppb.day1 = uppb.day1, 
               yreal.day1 = yreal.day1))
   
}

##########################################################################################

################################################################################
# PREDICTIONS using geoR - KRIGING
################################################################################

predgeoR = function(predData){
   
# Prediction for these locations
newlocs = unique(predData[,c("lambx","lamby")])
u = nrow(newlocs)

# Number of days
days = unique(predData$DATE)
ndays = length(days)

# Initializing variables
ypred.geoR  = matrix(NA, nrow = ndays, ncol = u)
sd.geoR     = matrix(NA, nrow = ndays, ncol = u)
lwrb.geoR   = matrix(NA, nrow = ndays, ncol = u) 
uppb.geoR   = matrix(NA, nrow = ndays, ncol = u) 
yreal.geoR  = matrix(NA, nrow = ndays, ncol = u) 
krige       = list()
prop.geoR   = NULL

for (i in 1:ndays){
   
   # Subset of data containing observations at selected date
   subresdata = subset(predData, DATE == days[i])
   subresdata$lambx = subresdata$lambx*100 # Transforming back to km
   subresdata$lamby = subresdata$lamby*100 # Transforming back to km
   
   # Create geodata object
   geodata = as.geodata(subresdata, coords.col = c("lambx","lamby"), data.col = c("Ytilde")) 
   
   # Variog
   variogb = variog(geodata, option = c("bin"), trend = "cte", uvec = seq(0, 500, by = 50)) # cte because already using Residuals
   
   # Likelihood fit
   likefit = likfit(geodata, ini.cov.pars = c(10, 200), trend = "cte", fix.nugget = TRUE)
   
   # Exponential variog fit
   var.fit = variofit(variogb, ini.cov.pars = likefit$cov.pars, cov.model = "exponential", fix.nugget = TRUE, nugget = likefit$nugget)
   
   # Krige control
   KC = krige.control(type.krige = "ok", cov.model = "exponential", cov.pars = var.fit$cov.pars, nugget = var.fit$nugget)
   
   # Kriging
   krige[[i]] = krige.conv(geodata, locations = newlocs, krige = KC)
   
   # Getting Predictions
   ypred.geoR[i,] = krige[[i]]$predict
   sd.geoR[i,]   = sqrt(krige[[i]]$krige.var)
   
   # Subset of new data (prediction)
   subnewdata = subset(predData, DATE == days[i])
   
   # Observed
   yreal.geoR[i,] = subnewdata$Ytilde 
   
   # IC
   lwrb.geoR[i,]   = ypred.geoR[i,] - 2*sd.geoR[i,]
   uppb.geoR[i,]   = ypred.geoR[i,] + 2*sd.geoR[i,]
   
   # Proportion out of 95% bounds
   prop.geoR[i] = length(unique(c(which(yreal.geoR[i,] < lwrb.geoR[i,]), which(yreal.geoR[i,] > uppb.geoR[i,]))))/u
   
}

   prop.geoR2 = NULL
   MSEgeoR = NULL
   
   for (i in 1:u){
      # Proportion out of 95% bounds
      prop.geoR2[i] = length(unique(c(which(yreal.geoR[,i] < lwrb.geoR[,i]), which(yreal.geoR[,i] > uppb.geoR[,i]))))/ndays
      # Prediction MSE
      MSEgeoR[i] = mean((yreal.geoR[,i]-ypred.geoR[,i])^2)
   }

      # Prediction MSE
   MSEgeoRday = NULL
   for (i in 1:ndays){
      MSEgeoRday[i] = mean((yreal.geoR[i,]-ypred.geoR[i,])^2)
   }   
   
   return(list(prop = prop.geoR, MSE = MSEgeoR, MSEday = MSEgeoRday, 
               sd.day1 = sd.geoR, ypred.day1 = ypred.geoR, 
               lwrb.day1 = lwrb.geoR, uppb.day1 = uppb.geoR, 
               yreal.day1 = yreal.geoR))

}
