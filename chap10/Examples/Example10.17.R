###############################################
### Example 10.17. Implementation in R–INLA ###
###############################################
formula = y ~ x + f(t, "rw") 
model = inla(formula , data=data) 