############################################################
### Example 2.9 - Estimating the SMR using a Poisson GLM ###
############################################################

### Finding MLE and SE of log(SMR) = beta0 on one single area
y <- 29; # Total observed death
E <- 19.88 # Expected deaths
summary(glm(y ~ offset(log(E)),family="poisson"))

### Finding MLE and SE of log(SMR) = beta0 over multiple areas
summary(glm(Y ~ offset(log(E)),family="poisson", data=data))


