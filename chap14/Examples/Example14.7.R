#################################################
### Example 14.7. Hourly ozone extreme values ###
#################################################

library(fExtremes)
illinoiso04_14.dat <-read.table("illinois_aqsO3data2004_2014.txt", sep="",header=FALSE)

### blockMaxima computes the block maxima for blocks of a specified number
### of hours Daily, Weekly Two Weekly and Four weekly maxima 
bm24<-blockMaxima(illinoiso04_14.dat\$V1, block = 24,doplot=FALSE)
bm168<-blockMaxima(illinoiso04_14.dat\$V1, block = 168,doplot=FALSE)
bm336<-blockMaxima(illinoiso04_14.dat\$V1, block = 336,doplot=FALSE)
bm672<-blockMaxima(illinoiso04_14.dat\$V1, block = 672,doplot=FALSE)

fit24<-gevFit(illinoiso04_14.dat\$V1, block = 24, type = "mle")

hist(bm24, nclass = NULL, freq = FALSE,, xlim = c(0,0.12), ylim=c(0,30),xlab = "O3 daily maxima (ppm)",  ylab="density", main = "") 
x = seq(0,0.12, by = 0.001)
lines(x, dgev(x, xi = -0.09164308, mu = 0.04218148, beta = 0.01400470), col = "black")

e<-(1:length(bm24)-1/2)/length(bm24)
empq<-sort(bm24)
fitq<-qgev(e,xi = -0.09164308, mu = 0.04218148, beta = 0.01400470)
plot(empq,fitq,xlab='empirical quantiles (ppm)',ylab='Fitted GEV quantiles (ppm)')

# Interpreting the plot.
length(e)-length(e[e>=0.99])
bigq<-cbind(empq[2828:2856],fitq[2828:2856])
bigq