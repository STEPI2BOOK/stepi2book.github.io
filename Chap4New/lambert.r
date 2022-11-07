Flamb2 <- function(geoconfig, latrf1 = NA, latrf2 = NA, latref = NA, lngref = NA)
{
 # Evaluate Lambert projection for geoconfig: (lat, -long)

	geo <- as.matrix(geoconfig)
	if(dim(geo)[[2.]] != 2.)
		stop("the input should be an nx2 matrix")
	xy.coord <- array(0., dim(geo))
	if(is.na(latref))
		latref <- sum(range(geo[, 1.]))/2.
	if(is.na(lngref))
		lngref <- sum(range(geo[, 2.]))/2.
	if(is.na(latrf1))
		latrf1 <- latref - (3./10.) * diff(range(geo[, 1.]))
	if(is.na(latrf2))
		latrf2 <- latref + (3./10.) * diff(range(geo[, 1.]))
	lat <- geo[, 1.]
	long <- geo[, 2.]
	pi <- 3.14159265
	a <- 6378137.
	b <- 6356752.
	radlf1 <- (pi/180.) * latrf1
	radlf2 <- (pi/180.) * latrf2
	radlgf <-  - (pi/180.) * lngref
	radltf <- (pi/180.) * latref
	eccen <- sqrt((a^2. - b^2.)/a^2.)
	capr <- (a * (1. - eccen^2.))/((1. - eccen^2. * sin(radltf)^2.)^1.5)
	n <- logb(cos(radlf1)/cos(radlf2))/(logb(tan(pi/4. + radlf2/2.)/tan(
		pi/4. + radlf1/2.)))
	capf <- (cos(radlf1) * ((tan(pi/4. + radlf1/2.))^n))/n

	rho0 <- (capr * capf)/((tan(pi/4. + radltf/2.))^n)
	radlat <- (pi/180.) * lat
	radlng <-  - (pi/180.) * long
	theta <- n * (radlng - radlgf)
	rho <- (capr * capf)/((tan(pi/4. + radlat/2.))^n)
	x <- (0.001) * rho * sin(theta)
	y <- (0.001) * (rho0 - rho * cos(theta))
	xy <- cbind(x, y)
	list(xy =xy, latrf1=latrf1, latrf2=latrf2, latref=latref, lngref=lngref)

}
