#################################################################################
### Example 9.11. Creating a mesh: black smoke monitoring locations in the UK ###
#################################################################################

mesh = inla.mesh.create(locations[,1:2], extend=list( offset=-0.1), cutoff=1, 
                        # Refined triangulation, minimal angles >=26 degrees , 
                        # interior maximal edge lengths 0.08, 
                        # exterior maximal edge lengths 0.2: 
                        refine=(list(min.angle=26, max.edge.data = 100, max.edge.extra=200))) 

ukmap <- readShapeLines("uk_BNG.shp") plot(mesh , col="gray", main="") 
lines(ukmap)
points(locations , col="red", pch=20,bg="red") 
