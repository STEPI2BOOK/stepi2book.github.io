################################################################################

# INPUT FIRST
# Need resdata to run

###############################################
# Investigating Residual Temporal Correlation #
###############################################

arcoeff = list()
ar1 = 0
ar2 = 0
ar3 = 0

for (i in 1:nstations){
  subresdata = subset(resdata, STATION == stations[i])
  pacf(subresdata$Res)
  arcoeff[[i]] = auto.arima(subresdata$Res, d=0, D=0, max.q=0)$coef
  ar1 = ar1 + ifelse( "ar1" %in% names(arcoeff[[i]]), 1, 0)
  ar2 = ar2 + ifelse( "ar2" %in% names(arcoeff[[i]]), 1, 0)
  ar3 = ar3 + ifelse( "ar3" %in% names(arcoeff[[i]]), 1, 0)
  }

ar1; ar2; ar3

#######################
# Correlation Leakage #
#######################

# Global AR of residuals    resdata$Res

arfit = ar(resdata$Res, aic = F, order = 1)
resdata = cbind(resdata, ARRes = arfit$resid)


# Residual correlation (with and without temporal structure)

lmfit.resid = NULL
ar.resid    = NULL

for (i in 1:nstations){
  subresdata  = subset(resdata, STATION == stations[i])
  lmfit.resid = cbind(lmfit.resid, subresdata$Res)
  ar.resid    = cbind(ar.resid, subresdata$ARRes)
  }
 
lmfit.resid.corr = cor(lmfit.resid, use = "pairwise") 
ar.resid.corr    = cor(ar.resid, use = "pairwise")

# Calculate distance between the locations
dist = Fdist(unique(lambcoords$xy))

# Plot: Distance vs Correlation
par(mfrow=c(1,2))
off = lower.tri(dist, diag = FALSE)
plot(dist[off], lmfit.resid.corr[off], pch = 16, col = "gray",
  xlab = "Distance (km)", ylab = "Spatial correlation (spatiotemporal residuals)", ylim = c(-0.2, 1), cex.lab = 1.2, cex.axis = 1.2)
lines(lowess(dist[off], lmfit.resid.corr[off]), lwd = 2, col = 2)
plot(dist[off], ar.resid.corr[off],    pch = 16, col = "gray",
  xlab = "Distance (km)", ylab = "Spatial correlation (AR(1) residuals)", ylim = c(-0.2, 1), cex.lab = 1.2, cex.axis = 1.2)
lines(lowess(dist[off], ar.resid.corr[off]), lwd = 2, col = 2)
