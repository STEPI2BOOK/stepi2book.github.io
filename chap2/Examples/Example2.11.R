################################################################
### Example 2.11 - Modelling differences in SMRs in relation ###
###                to differences in exposures               ###
################################################################

### Fitting a model to estimate the relative risk associated with
### air pollution
summary(glm(Y ~ offset(log(E)) + X1, family="poisson", data=data))

### Fitting a model to estimate the relative risk associated with
### air pollution using a Quasi-Poisson approach
summary(glm(Y ~ offset(log(E)) + X1, family="quasipoisson", data=data))

### Fitting a Poisson GLM with air pollution and deprivation
summary(glm(Y ~ offset(log(E)) + X1 + X2, family="poisson", data=data))

### Fitting a Quasi-Poisson GLM with air pollution and deprivation
summary(glm(Y ~ offset(log(E)) + X1 + X2, family="quasipoisson", data=data))

### Perform tests between the deviances of two models

# Test 1: Effect on Quasi-Poisson models with and without deprivation
anova(glm(Y ~ offset(log(E))+X1,family="quasipoisson",data=data), # Model 1
      glm(Y ~ offset(log(E))+X1+X2,family="quasipoisson",data=data), # Model2
      test="Chisq") # Chi-Squared test
      
# Test 2: Effect on Quasi-Poisson models with and without air pollution
anova(glm(Y ~ offset(log(E))+X1+X2,family="quasipoisson",data=data), # Model 1
      glm(Y ~ offset(log(E))+X2,family="quasipoisson",data=data), # Model 2
      test="Chisq") # Chi-Squared test

