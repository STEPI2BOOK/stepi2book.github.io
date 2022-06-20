###########################################
###   This code illustrates use of the geoR package
###   for distribution analysis in Example 8.6

library(gstat) 
data(meuse)
str(meuse) ### Reveals the structure of the dataset.

### library(geoR, warn = F)
### meuse.geo <- as.geodata(meuse, data.col = 5) #data column 5 contains lead

### Next a histogram to check see if the disribution 
### of lead is approximately normal.

library(lattice)
print(histogram(~log(lead), data = meuse, col="gray"))

### We now use q-qplots instead but
### instead of qqnorm() we can use the qqmath()
### function from the lattice package.
qqmath(~log(lead), data = meuse, col="black",panel = function(x, ...) {
        panel.qqmath(x, ...)
        panel.qqmathline(x, distribution = qnorm,col="black") 
        })

qqmath(~log(lead), data = meuse, col="black",panel = function(x, ...) {
       panel.qqmath(x, ...)
       panel.qqmathline(x, distribution = function(p) qt(p, df = 1))}, 
       xlab = "qt, df=1")
        }))

qqmath(~log(lead), col="black", data = meuse, panel = function(x, ...) {
       panel.qqmath(x, ...)
       panel.qqmathlinanel.qqmath(x, ...)
       panel.qqmathline(x, distribution = function(p) qt(p, df = 10), 
      ...}, xlab = "qt, df=10"))

