###################################################################
### Example 5.3 - Fitting a Poisson regression model in WinBUGS ###
###################################################################

### Code to fit the Poisson log-linear model seen in Chapter 2
### WinBUGS code to fit the model:
model {
  for (i in 1 : N) {
    Y[i] ~ dpois(mu[i])
    log(mu[i]) <- E[i]+exp(beta0+ beta1*X1[i] + betad*X2[i ]X2)
  }
  # Priors
  beta0 ~ dnorm (0 ,0.001)
  beta1 ~ dnorm (0 ,0.001)
  betad ~ dnorm (0 ,0.001)
  # Functions of interest:
  base <- exp(beta0) 
  RR <- exp(beta1)
}

list(N = 393,
     Y = c(29,27,9,18,24,29,14,...,16,26,25),
     E = c(19.883, 13.525, 7.712, 16.99, 19.635, 11.227, ...,19.287 ,20.839),
     X1 = c(1.4, 2.3, 3.4, 2.3, 2, 2.9, ..., 2.2,2.8, 1.8),
     X2 = c(1.599, 8.123, 5.413, 6.857, 7.772, ..., 10.283, 6.04))

# Initial Estimates for two chains
list(beta0 = 0, beta1=0, betad=0)
list(beta0 = 1, beta1=1, betad=1)}