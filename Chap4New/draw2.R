draw2 <- 
  function (data, fs = FALSE, lwidth = c(1,1), lcolor = topo.colors(4), 
            ltype=c(1), cutpts, limits = FALSE, optlist, pts = FALSE,
            legend=FALSE,ptcex=.5)
            # ltype=c(1,5,2,3)
# For drawing biorthogonal grids, default is to use specified colors for
# line types.  If not color, then line type, then line width.
# The number of elements in the cutpts determines cutting of the 
# field magnitude. If not specified in cutpts, 4 cuts will be chosen.
  {
    if (missing(data)) 
      stop("no data was passed to draw")
    if (!all(names(data) == c("grid", "ngrid", "fldmag", "nn"))) 
      stop(paste(substitute(data), "is not a valid grid object"))
    if (!is.matrix(data$grid)) 
      stop(paste(substitute(data), "$grid is not a matrix", 
                 sep = ""))
    grid1 <- data$grid[1, ]
    grid2 <- data$grid[2, ]
    if (pts) 
      points(grid1, grid2)
    if (!fs) {
      lines(grid1, grid2)
      return()
    }
    
    if (!missing(optlist)) {
      cutpts <- optlist$cutpts
      lcolor <- optlist$lcolor
      lwidth <- optlist$lwidth
      ltype <- optlist$ltype
    }
    intcnt <- 4
    if (!missing(cutpts)) { intcnt <- length(cutpts)-1 } else {
    if (length(lcolor)>1) { intcnt <- length(lcolor) } else {
      if (length(ltype>1)>1) { intcnt <- length(ltype) } else {
        if (length(lwidth)>1) { intcnt <- length(lwidth) } else {
          cnt <- 1
        }
      }
    }
    }
    #lcolors <- topo.colors(intcnt)
    lwidths <- seq(min(lwidth),max(lwidth),
                   by=diff(range(lwidth))/(intcnt-1))
    if (length(lwidths)!=intcnt) lwidths <- rep(lwidths[1],intcnt)
    ltypes <- ltype; if (length(ltype)!=intcnt) ltypes <- rep(1,intcnt)
    
    #wid.cnt <- abs(wid.range <- as.integer(lwidth[2] - lwidth[1]))
    #col.cnt <- abs(col.range <- as.integer(lcolor[2] - lcolor[1]))
    #linetype <- (wid.cnt > 0) | (col.cnt > 0)
    gridlen <- data$ngrid
    fmag <- data$fldmag
    gridmiss <- (1:gridlen)[is.na(fmag)]
    fmag[gridmiss] <- fmag[gridmiss - 1]
    fmag.range <- range(fmag)
    if (missing(cutpts)) {  
      fmag.intlen <- (fmag.range[2] - fmag.range[1])/intcnt
      cutpts <- seq(fmag.range[1], fmag.range[2], fmag.intlen)
      cutpts[1] <- 0
      limits <- TRUE
    }
    
    if (!limits) 
      cutpts <- c(0, cutpts, fmag.range[2])
    fmag.breaks <- cut(fmag, cutpts)
    fmag.brk1 <- c(fmag.breaks[2:gridlen], 0)
    #intcnt <- as.integer(length(cutpts) - 1)
    difcnt <- as.integer(intcnt - 1)
    m <- matrix(1:intcnt, nrow = gridlen, ncol = intcnt, byrow = T)
    lt <- matrix(FALSE, nrow = gridlen, ncol = intcnt)
    lt[fmag.breaks == m | fmag.brk1 == m] <- TRUE
    x <- matrix(rep(grid1, intcnt), ncol = intcnt)
    y <- matrix(rep(grid2, intcnt), ncol = intcnt)
    x[lt == FALSE] <- NA
    y[lt == FALSE] <- NA
 
    for (ix in 1:intcnt) { 
      lines(x[, ix], y[, ix], lty = ltypes[ix], 
                  lwd = lwidths[ix], col = lcolors[ix])
      points(x[,ix],y[,ix],col=lcolors[ix],pch=20,cex=ptcex) }
    retgrid <- list(x=x,y=y,lty=ltypes,lwidths=lwidths,lcolors=lcolors)
    if (legend) {
      midpts <- (cutpts[-1]+cutpts[-length(cutpts)])/2
      legend("topright",lty=ltypes,lwd=lwidths,col=lcolors,
             legend=paste(round(midpts,2)))
    }
    optlist <- list(lwidth = lwidths, lcolor = lcolors, ltype = ltypes,
                    cutpts = cutpts, limits = limits)
    return(list(optlist=optlist,grid=retgrid))
    }