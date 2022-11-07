FtransdrawMOD = function (disp, Gcrds, MDScrds, gridstr, sta.names, lambda = 0,
    lsq = FALSE, eye, model = 1, a0 = 0.1, t0 = 0.5)
{
    on.exit(par(par0))
    par0 <- par(no.readonly = TRUE)
    oldpar <- par(xpd = T)
    grid <- gridstr$grid
    if (missing(sta.names))
        sta.names <- format(1:nrow(Gcrds))
    rl.ind <- gridstr$rl.ind
    Ddim <- ncol(MDScrds)
    grid.nm <- !is.na(grid[, 1])
    #plim <- setplot(grid[, 1], grid[, 2], axes = T)
    #text(Gcrds[, 1], Gcrds[, 2], sta.names, cex = 0.75)
    #lines(grid[, 1], grid[, 2])
    #title(main = "Geographic Coordinates")
    #cat("Click anywhere on plot to continue (Left button)\n")
    #junk <- locator(n = 1)
    #par(pin = plim$oldpin)
    if (Ddim == 2)
        par(mfrow = c(2, 2))
    while (length(lambda) == 1) {
        Tspline <- sinterp(t(Gcrds), MDScrds, lam = lambda, lsq = lsq)
        Dcrds <- t(seval(t(Gcrds), Tspline)$y)
        Ddist <- Fdist(Dcrds)
        Dgrid <- matrix(NA, nrow(grid), Ddim)
        Dgrid[grid.nm, ] <- t(seval(t(grid[grid.nm, ]), Tspline)$y)
        h.lt <- Ddist[row(Ddist) < col(Ddist)]
        disp.lt <- disp[row(disp) < col(disp)]
        variogfit <- Fvariogfit3(disp.lt, h.lt, model = model,
            a0 = a0, t0 = t0)
        a0 <- variogfit$a[1]
        t0 <- variogfit$t0
        rmsre <- round(sqrt(mean(((disp.lt[order(h.lt)] - variogfit$fit[order(h.lt)])/variogfit$fit[order(h.lt)])^2)),
            4)
        #plot(h.lt, disp.lt, xlab = paste("D-space distance"),
        #    ylab = paste("Dispersion"),#, rmsre =", format(rmsre)),
        #    ylim = c(0, max(disp.lt)), xlim = c(0, max(h.lt)), pch = 16, col = "gray")
        #title(main = paste(ifelse(model == 1, " Exponential",
        #     " Gaussian"), "Variogram"), cex = 0.75)
        #if (max(disp.lt) > 2)
        #    abline(h = 2, lty = 2)
        #lines(h.lt[order(h.lt)], variogfit$fit[order(h.lt)], lwd = 2, col = 2)
        #points(0, variogfit$a[1], pch = 16, col = "gray")
        #title(sub = substitute(list(lambda) == list(x), list(x = lambda)))
        if (Ddim == 2) {
            plim <- setplot(Dgrid[, 1], Dgrid[, 2], axes = T)   #<---------------
        #    text(Dcrds[, 1], Dcrds[, 2], sta.names, cex = 1)
            lines(Dgrid[, 1], Dgrid[, 2])
            title(xlab = "D-Plane Coordinates", main = substitute(list(lambda) ==   list(x), list(x = lambda)))
            #title(main = "D-plane Coordinates", xlab = substitute(list(lambda) ==
            #    list(x), list(x = lambda)), sub = expression(paste("[",
            #    lambda, " : Spline smoothing parameter]")))
            #par(pin = plim$oldpin)
            #par(mfrow = c(1,2))
        }
        else {
            stop("3-D perspective plot is not yet implemented")
        }
        cat("Enter value for new lambda (Hit return to stop) \n")
        lambda <- scan(n = 1)
        if (length(lambda) > 0) {
            cat("Click anywhere on plot to continue (left button)  \n")
            junk <- locator(n = 1)
        }
    }
    list(Dcrds = Dcrds, Ddist = Ddist)
}



setplot = function (xdata, ydata, pretty.call = TRUE, maxdim, axes = FALSE)
{
    if (missing(xdata))
        stop("no xdata nor ydata was passed to setplot")
    if (is.matrix(xdata)) {
        if (ncol(xdata) != 2)
            stop(paste(substitute(xdata), "has too many columns"))
        ydata <- xdata[, 2]
        xdata <- xdata[, 1]
    }
    else if (is.list(xdata)) {
        ydata <- xdata$y
        if (is.null(ydata))
            stop(paste(substitute(xdata), "has no y component"))
        xdata <- xdata$x
        if (is.null(xdata))
            stop(paste(substitute(xdata), "has no x component"))
    }
    else if (missing(ydata))
        stop("no ydata was passed to setplot")
    if (pretty.call) {
        xdata <- pretty(xdata)
        ydata <- pretty(ydata)
    }
    xlim <- range(xdata)
    ylim <- range(ydata)
    xrng <- xlim[2] - xlim[1]
    yrng <- ylim[2] - ylim[1]
    prng <- max(xrng, yrng)
    oldpin <- par("pin")
    if (missing(maxdim)) {
        if (xrng/yrng > oldpin[1]/oldpin[2]) {
            maxdim <- oldpin[1]
            prng <- xrng
        }
        else {
            maxdim <- oldpin[2]
            prng <- yrng
        }
    }
    newpin <- (maxdim * c(xrng, yrng))/prng
 #   par(pty = "m", pin = newpin)
    plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxs = "i",
        yaxs = "i", axes = axes, cex.axis = 1.2)
    if (interactive()) {
        warning("Plot limits changed.  Reset before making new plots!")
        warning(paste("Old limits: ", paste(oldpin, collapse = "  ")))
        warning(paste("New limits: ", paste(newpin, collapse = "  ")))
        list(xlim = xlim, ylim = ylim, oldpin = oldpin, newpin = newpin)
    }
}