##################################################################
### Example 5.4 - Fitting a Poisson regression model in R-INLA ###
##################################################################
### Loading INLA library
library(INLA)

### Poisson regression model
formula = Y ~ X1+X2 + f(i, model="iid")

### Call to INLA
model = inla(formula, family="poisson", data=data)

### Model summary
summary(model)