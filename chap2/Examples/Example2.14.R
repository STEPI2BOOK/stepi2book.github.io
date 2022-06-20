#####################################################################
### Example 2.14 - Estimating the odds ratio of asthma associated ###
###                with proximity to roads                        ###
#####################################################################

### Fitting Odds Ratio
summary(glm(Y~X, family="binomial", data=data))