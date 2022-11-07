# Three functions for computing the negative log likelihood
# (1) deformloglik.variogram:  function for optimzing over the variogram parameters
# (2) deformloglik.beta:  function for optimizing over the 'beta' coefficients
#   of the partial warps.  This may be a subset of the complete set.
# (3) deformloglik:  Common function called by both of the above, after setting
#   up arguments appropriately.

###
deformloglik.beta.lambda  <- function(beta,B,X,spatialParams,S,t,jWarp,lambda) {
  # Compute log likelihood
  # Two sets of parameters:  
  # 1. variog parameters in "spatialParams"
  # 2. partial warp parameters in "beta"
  # First argument contains parameters to be optimized
  # beta should be passed as a vector.  Convert to matrix.
  #
  # "jWarp" is a vector indicating which partial warps to fit/estimate.
  # It should be a subset of 1:(n-3).  
  
  n <- nrow(X)
  #beta <- matrix(beta,nrow=2,ncol=(n-2))
  beta <- matrix(beta,nrow=2,ncol=1+length(jWarp))
  betafull <- matrix(0,nrow=2,ncol=(n-2))
  betafull[,c(1,jWarp+1)] <- beta
  beta <- betafull
  
  nloglik <- deformloglik.lambda(beta,spatialParams,B,X,S,t,jWarp,lambda)
  return(nloglik) 
}

###
deformloglik.variog.lambda  <- function(spatialParams,B,X,beta,S,t,lambda) {
  # Compute log likelihood
  # Two sets of parameters:  
  # 1. variog parameters in "spatialParams"
  # 2. partial warp parameters in "beta"
  # First argument contains parameters to be optimized
  # beta should be passed as a vector.  Convert to matrix.
  #
  # "jWarp" is a vector indicating which partial warps to fit/estimate.
  # It should be a subset of 1:(n-3).  
  n <- nrow(X)
  beta <- matrix(beta,nrow=2,ncol=(n-2))
  #beta <- matrix(beta,nrow=2,ncol=1+length(jWarp))
  #betafull <- matrix(0,nrow=2,ncol=(n-2))
  #betafull[,c(1,jWarp+1)] <- beta
  #beta <- betafull
  jWarp <- 1:(n-3)
  
  nloglik <- deformloglik.lambda(beta,spatialParams,B,X,S,t,jWarp,lambda)
  return(nloglik) 
}

###
deformloglik.lambda  <- function(beta,spatialParams,B,X,S,t,jWarp,lambda) {
  # creating partial warps
  Beig <- eigen(B)
  g <- Beig$vectors
  l <- Beig$values
  
  g <- g[,order(l)]
  l <- l[order(l)]
  
  parWarpsSum<- matrix(0,nrow=nrow(X),ncol=ncol(X))
  for (j in 3+jWarp) {    # This is 4:n if jWarp is 1:(n-3)
    parWarpsSum <- parWarpsSum + g[,j] %o% beta[,j-2]
  }
  
  # The affine part
  c <- c(0,0)
  A <- matrix(c(1,beta[1,1],0,1+beta[2,1]),byrow=X,2,2)
  
  # Calculating Y
  Xtarg <- X
  htarg <- as.matrix(dist(rbind(X,Xtarg),diag=TRUE,upper=TRUE))
  htarg <- htarg[1:n,(n+1):(n+nrow(Xtarg))]
  sigma <- htarg^2 * log(htarg)
  sigma[htarg==0] <- 0
  
  Y <- t( matrix(c,2,nrow(Xtarg)) +
      A %*% t(Xtarg) + t(parWarpsSum) %*% sigma )
  
  # The parameterized covariance matrix E  (ie, capital Sigma)
  theta     <- spatialParams[1]  # range parameter
  sigmaSqrd <- spatialParams[2]  # scale parameter
  eta       <- spatialParams[3]  # nugget parameter
  D <- as.matrix(dist(Y,upper=T,diag=T))
  
  # Exponential correlation model
  E <- exp(eta)*diag(1,n) + exp(sigmaSqrd) * exp( (-1/exp(theta)) * D)
  
  # negative log likelihood 
  #likelihood <- ( det(E)^((1-t)/2)) * exp((-t/2) * sum(diag(solve(E) %*% S )))
  
  logdet <- sum(log(eigen(E, symmetric = TRUE, only.values = TRUE)$values))
  nloglik <- -(-t*n*log(2*pi) - t*logdet - t*sum(diag(solve(E) %*% S )))/2 +
    lambda*sum(abs(beta[,c(1,1+jWarp)]))
  return(nloglik)
}



