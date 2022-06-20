#########################################################
### Example 9.10. Spatial prediction of NO2 in Europe ###
#########################################################

### Joint prediction: 
y.pred[1:NP] ~ spatial.pred(mu.T[], xcoord.pred[], 
ycoord.pred[], y[]) 

### Single site prediction: 
for(j in 1:NP) { 
y.pred[j] ~ spatial.unipred(mu.T[j], xcoord. pred[j], 
ycoord.pred[j], y[]) } 
