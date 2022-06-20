########################################################################################################
### Example 9.9. Fitting an exponential spatial model to PM10 concentrations in London using WinBUGS ###
########################################################################################################

model { 
for (site in 1:NS) { 
	y[site] ~ dnorm(mean.site[site],tau.v[site]) 
	mean.site[site] <- beta0 + m.adj[site] 
	m[1:S] ~ spatial.exp(mu[],xcoords[],ycoords[],tau.m,phi1 ,phi2) 
	# Set up the priors for the spatial model
	# here we fix the second parameter to be one 
	phi2 <- 1
	phi1 ~ dunif (0.005 ,0.115) 
	tau.m ~ dgamma(1,0.01) sigma.m <- 1/sqrt(tau.m) 
	# Set up the site specific observation precisions 
	for (site in 1:NS) { 
		tau.v[site] ~ dgamma(1,0.001) 
		sigma.v[site] <-1/sqrt(tau.v[site]) 
		} 
		# prior for the intercept term 
		beta0 ~ dnorm (0 ,1000) 
	for (site in 1:NS) { 
		mu[site]<-0
		m.adj[site] <- m[site]-mean(m[1:NS]) 
		sigma.m.adj <- sqrt(pow(sigma.m,2.0)*NS/(NS-1)) 
}
} # end of model 
