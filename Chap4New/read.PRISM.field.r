read.PRISM.field <- function(fn){
# fn PRISM field name
# header info is first 6 lines
 NSKIP<- 6
 temp<-matrix(scan( fn, nmax=NSKIP*2, what="a"), ncol=2, byrow=TRUE)
 temp2<- as.numeric( temp[,2])
 names( temp2)<- temp[,1]
 NC<- temp2["nrows"]
 NR<- temp2["ncols"]
# use cell centers instead of cell boundaries
 delta<- temp2["cellsize"]
 xtemp<-  (1:NR)* delta + temp2["xllcorner"] -  delta/2
 ytemp<-  (1:NC)* delta + temp2["yllcorner"] - delta/2
 grid<- list( x = xtemp, y= ytemp)
 hold<-  as.single(scan( fn, skip = NSKIP))
 hold[ hold == temp2["NODATA_value"]] <- NA
# rows of PRISM file are put into columns of R image matrix.
 hold<- matrix( hold, nrow=NR, ncol=NC)
# reverse order of latiudes to go from smallest to largest.
  hold<- hold[, NC:1]
# bundle up as image object
  return( list( x= grid$x, y=grid$y, z=hold))
}