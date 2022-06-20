##################################################################
### Example 8.1 - Empirical Bayes and Bayes smoothing of COPD  ###
###               mortality for 2010                           ###
##################################################################


### Empirical Bayesian Approach
# requires SpatialEpi package 
library(SpatialEpi) 

# Calculating Relative Risks and plotting them
RRs = eBayes(Y,E)
plot(RRs$SMR, RRs$RR, xlim=c(0,2),ylim=c(0,2), xlab="SMRs",ylab="RRs")
abline(a=0,b=1, col="red", lwd=3) 


### Fully Bayesian Approach
# WinBUGS code to run model in Bayesian Approach
model { 
   for (i in 1 : N) {
	   Y[i] ~ dpois(mu[i])
	   mu[i] <- E[i]*exp(beta0)*theta[i] 
	   RR[i] <- exp(beta0)*theta[i]
	   theta[i] ~ dgamma(alpha ,alpha) 
   } 
   # Priors 
   alpha ~ dgamma(1,1) beta0 ~ dflat () 
   # standard deviation of non--spatial 
   sigma.theta <- sqrt(1/alpha) 
   overall.risk <- exp(beta0) 
} 
