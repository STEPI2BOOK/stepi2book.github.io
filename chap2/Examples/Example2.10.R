################################################################
### Example 2.10 - Estimating the SMR using quasi-likelihood ###
################################################################

### Using quasi-likelihood to find the MLE and standard error 
### of log(SMR) = beta0
summary(glm(Y ~ offset(log(E)),family="quasipoisson", data=data))