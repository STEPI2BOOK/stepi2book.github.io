#################################################################
### Example 2.12 - Modelling the risks associated with lagged ###
###                effects of air pollution                   ###
#################################################################

# Fitting quasi-poisson model
glm(formula = Y ~ offset(log(E)) + X1 + X1t1 + X1t2,
    family  = "quasipoisson", 
    data    = data)