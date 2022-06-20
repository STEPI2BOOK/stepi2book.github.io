##################################################################
### Example 2.13 - Estimating the odds ratio in a case-control ###
###                study using a logistic model                ###
##################################################################

# Fitting Odds Ratio
glm(formula = Y ~ 1, family = "binomial", data = data)