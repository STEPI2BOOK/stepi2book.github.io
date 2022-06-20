##################################################
### Example 9.4. Gaussian random fields (GRFs) ###
##################################################
library(EnviroStat)

ny_data <- read.csv("NY.metadata.txt", sep="")

LambertCoords = Flamb2(ny_data)
LambertCoords