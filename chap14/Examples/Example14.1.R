bplist00�_WebMainResource�	
_WebResourceFrameName^WebResourceURL_WebResourceData_WebResourceMIMEType_WebResourceTextEncodingNameP_Khttp://www.stat.ubc.ca/~jim/BookShaddickZidek/chapters/chap14/Example14.1.ROh<html><head></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">######This example concerns extreme values. The data are 8640 
######of hourly ozone concentrations (ppm) in 2000 at a site in the US state of Illinois
###### (the missing data were not filled in for this illustrative example)
library(fExtremes)

illinoiso04_14.dat &lt;-read.table("illinois_aqsO3data2004_2014.txt", sep="",header=FALSE)

###################################################################################
####### blockMaxima computes the block maxima for blocks of a specified number
####### of hours Daily, Weekly Two Weekly and Four weekly maxima 

bm24&lt;-blockMaxima(illinoiso04_14.dat\$V1, block = 24,doplot=FALSE)
bm168&lt;-blockMaxima(illinoiso04_14.dat\$V1, block = 168,doplot=FALSE)
bm336&lt;-blockMaxima(illinoiso04_14.dat\$V1, block = 336,doplot=FALSE)
bm672&lt;-blockMaxima(illinoiso04_14.dat\$V1, block = 672,doplot=FALSE)

plot.ts(bm24,xlab="Hour",ylab="Daily ozone maxima (ppm)")

######## Estimated Parameters:
########  xi          mu        beta 
######## -0.09164308  0.04218148  0.01400470 

# Comparing the fitted GEV density to the histogram
hist(bm24, nclass = NULL, freq = FALSE,, xlim = c(0,0.12), ylim=c(0,30),xlab = "O3 daily maxima (ppm)",
     ylab="density", main = "") 
x = seq(0,0.12, by = 0.001)
lines(x, dgev(x, xi = -0.09164308, mu = 0.04218148, beta = 0.01400470), col = "black")

#Constructing the qqplot diagnostic plot.
e&lt;-(1:length(bm24)-1/2)/length(bm24)
empq&lt;-sort(bm24)fitq&lt;-qgev(e,xi = -0.09164308, mu = 0.04218148, beta = 0.01400470)
fitq&lt;-qgev(e,xi = -0.09164308, mu = 0.04218148, beta = 0.01400470)
plot(empq,fitq,xlab='Empirical quantiles (ppm)',ylab='Fitted GEV quanitiles (ppm)')

#Interpreting the plot.
length(e)-length(e[e&gt;=0.99])
### Result: 2827
bigq&lt;-cbind(empq[2828:2856],fitq[2828:2856])
bigq




</pre></body></html>Ztext/plainUUTF-8    ( ? N ` v � � �OZ                           `