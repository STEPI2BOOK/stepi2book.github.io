
 model {
 # beginning of t loop
               for (t in 2:(n-1)) {
	
 				for (poll in 1:4) {	
 					for (site in 1:8) {
 						# 4x8 ys  arise from the 4 underlying thetas, & 8 site effects & measurement error
               			  y.mat[t,poll,site] ~ dnorm(mean.poll.site[t,poll,site],tau.v[poll,site])
 						mean.poll.site[t,poll,site] <- theta[t,poll]  +m.adj[poll,site]    + temp.effect[t,poll]
 						# end of site loop
 										}  
 						# all of  the  underlying thetas are averages of the two neighbours
 						 tmp.theta[t,poll]<- (theta[t-1,poll]+theta[t+1,poll])/2
 							for (poll2 in 1:4) {
 					                  Sigma.p.like[t,poll,poll2] < -  (theta[t,poll]-theta[t-1,poll]) *  (theta[t,poll2]-theta[t-1,poll2]) 	            
 							 					}
 						 temp.effect[t,poll]<- (beta.temp[poll]*temp.adj[t])		+(beta.temp2[poll]*temp2.adj[t])
 # end of poll loop
 									}	
              			  theta[t,1:4] ~ dmnorm(tmp.theta[t,1:4],Sigma.p2[1:4,1:4])
 # temp effects
 temp.adj[t]<-temp[t]-temp.bar
 temp2.adj[t]<- temp2[t]-temp2.bar

 # end of t loop
                					 }
								

 # Set up the priors for 'edges' of the underlying AR process for theta
       theta[1,1:4]~dmnorm(theta[2,1:4],Sigma.p[1:4,1:4])
       theta[n,1:4]~dmnorm(theta[n-1,1:4],Sigma.p[1:4,1:4])

 # Set up the priors for the 'edges' of the y's
 		for (poll in 1:4) {
 				for (site in 1:8) {
 			y.mat[1,poll,site] ~ dnorm(theta[1,poll],tau.v[poll,site])
 			y.mat[n,poll,site] ~ dnorm(theta[n,poll],tau.v[poll,site])
 									}
      						}		

 # Likelihoods for the 'edges'

 		for (poll1 in 1:4) {
 			for (poll2 in 1:4) {
 				Sigma.p.like[1,poll1,poll2]<-0
 				Sigma.p.like[n,poll1,poll2]<-(theta[n,poll1]-theta[n-1,poll1]) *  (theta[n,poll2]-theta[n-1,poll2])
 								 }
 						       }

# Likelihoods for the Wishart parameter
 # initial values of the priors 
 R[1,1] <- 0.2
 R[1,2] <- 0.01
 R[1,3] <- 0.01
 R[1,4] <- 0.01

 R[2,2] <- 0.2
 R[2,1] <- 0.01
 R[2,3] <- 0.01
 R[2,4] <- 0.01

 R[3,3] <- 0.2
 R[3,1] <- 0.01
 R[3,2] <- 0.01
 R[3,4] <- 0.01

 R[4,4] <- 0.2
 R[4,1] <- 0.01
 R[4,2] <- 0.01
 R[4,3] <- 0.01

 for (poll1 in 1:4) {
 	for (poll2 in 1:4) {
 		Rn[poll1,poll2] < - R[poll1,poll2] + sum(Sigma.p.like[1:n,poll1,poll2])
 					}	
 						  }
	
 K <-2
 Kn <- K+ n
	
 	Sigma.p[1:4,1:4] ~ dwish(Rn[1:4,1:4],Kn)

 # mutiply the precision by 2, as variance needs to be divided by 2 (average of 2 thetas)

 for (i in 1:4){
 			for (j in 1:4){
 							Sigma.p2[i,j] <- Sigma.p[i,j]*2
 							}
 			}	
	
 # put in the inverse stuff here, for the sd matrix   / correlation
  
 for (i in 1:4){
 			for (j in 1:4){
 							var.p[i,j] <- inverse( Sigma.p[,],i,j)
 							}
 			}

 for (poll in 1:4) {
   sigma.theta[poll] <- sqrt(var.p[poll,poll])	
 		for (poll2 in 1:4) {
 			corr.theta[poll,poll2] <-  var.p[poll,poll2] / (sigma.theta[poll]*sigma.theta[poll2])								
 					           }
 					}




 # Set up the pollutant/site specific observation precisions
 	for (poll in 1:4) {
 		for (site in 1:8) {
 		    tau.v[poll,site] ~ dgamma(1,0.01)
 		   sigma.v[poll,site] <-1/sqrt(tau.v[poll,site])
 							}
     					 }

 # Set up the priors for the site specific parameters
 #   set them up as spatial.exp prior, different for each site
 for (poll in 1:4) {
 m[poll,1:8] ~ spatial.exp(xcoords[],ycoords[],tau.m[poll],phi1[poll],phi2)
 }

 for (poll in 1:4) {
 sigma.m[poll] <- 1/sqrt(tau.m[poll])
 }

 # and to constrain the sums to be zero - CHECK for quicker approach
 for (poll in 1:4) {
 for (site in 1:8) {
 m.adj[poll,site] <- m[poll,site]-mean(m[poll,1:8])
 }
 }

 phi2 <- 1
 for (poll in 1:4) {
 phi1[poll]~ dunif(0.0026,0.115)
 tau.m[poll] ~ dgamma(1,0.01)
 }
 


 # priors for temp
 temp.bar<-mean(temp[])
 temp2.bar <- mean(temp2[])
 for (poll in 1:4) {
 beta.temp[poll] ~ dnorm(0,0.001)
 beta.temp2[poll] ~ dnorm(0,0.001)
 }

 # Calculate the mean and sd of the thetas
 for (poll in 1:4) {
 		  mean.theta[poll] <- mean(theta[1:n,poll])
 		  sd.theta[poll] <-sd(theta[1:n,poll])
     					 }



    
 # end of model
