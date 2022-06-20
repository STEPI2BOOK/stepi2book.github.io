################################################
### Example 10.16. Implementation in WinBUGS ###
################################################

model {   for (t in 2:(NT-1)) { 
   # observation model 
   y[t] ~ dnorm(gamma[t],tau.v) 
   } #tloop 
   y[1]~dnorm(gamma[1],tau.v) y[NT]~dnorm(gamma[n],tau.v) 
   tau.v ~ dgamma(1,0.01) 
} 

for (t in 2:(NT-1)) { 
   # system model 
   tmp.gamma[t] <- (gamma[t-1]+gamma[t+1])/2 gamma[t] ~ dnorm(tmp.gamma[t],tau.w2) 
} # t loop 

gamma[1]~dnorm(gamma[2],tau.w) 
gamma[NT]~dnorm(gamma[NT-1],tau.w) 
tau.w ~ dgamma(r.w,d.w) sigma.w <- 1 / sqrt(tau.w) 
} # end of model 


model { 
   for (t in 2:(NT-1)) { 
   # calculate the contribution to the likelihood for # full conditionals 
   tau.w.like[t] <-pow((gamma[t]-tmp.gamma[t]),2) 
   } # t loop 
   tau.w.like[1] <- 0
   tau.w.like[T] <- pow((gamma[T]-gamma[T-1]),2) 
   tau.w2 <- tau.w*2 d <-1 
   r <- 0.01
   d.w <- d+sum(tau.w.like[])/2 
   r.w <- r + n/2
   tau.w ~ dgamma(r.w,d.w) 
} # model 
