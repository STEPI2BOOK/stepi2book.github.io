###################################################################
### Example 8.3. Fitting a conditional spatial model in WinBUGS ###
###################################################################

# WinBUGS code to run conditional Spatial Model
model { 
   for (i in 1 : N) {
	   Y[i] ~ dpois(mu[i]) 
	   log(mu[i]) <- log(E[i]) + beta0 + V[i] + U[i] 
	   RR[i] <- exp(beta0 + V[i] + U[i]) 
	   V[i] ~ dnorm(0,tau.V) 
   } 
   # ICAR prior for the spatial random effects 
   U[1:N] ~ car.normal(adj[], weights[], num[], tau.U) 
   for(k in 1:sumNumNeigh) { 
       weights[k] <- 1 
       } 
   # Other priors 
   alpha0 ~ dflat () 
   alpha1 ~ dnorm(0.0, 0.001) tau ~ dgamma(0.5, 0.0005) 
   sigma <- sqrt(1 / tau) 
} 
