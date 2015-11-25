#' Create an Orientation Arrow
#' @description Returns a list containing:
#' res : coordinates to draw an arrow
#' coordinates of the middle of the arrow (where the "N" will be plotted)
#' @param scaleBar result of createScaleBar()
#' @param length desired length of the arrow
#' @param distance distance between legend rectangles and the bottom of the arrow
#' @param dist.units units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles)
#' @export

createOrientationArrow <- function(scaleBar, length, distance = 1, dist.units = "km"){
	lon <- scaleBar$rectangle2[1,1]
	lat <- scaleBar$rectangle2[1,2]

	# Bottom point of the arrow
	begPoint <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist.units, model = "WGS84")
	lon <- begPoint[1,"long"]
	lat <- begPoint[1,"lat"]

	# Let us create the endpoint
	onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist.units, model = "WGS84")

	leftArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 225, dist = length/5, dist.units = dist.units, model = "WGS84")

	rightArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 135, dist = length/5, dist.units = dist.units, model = "WGS84")

	res <- rbind(
			cbind(x = lon, y = lat, xend = onTop[1,"long"], yend = onTop[1,"lat"]),
			cbind(x = leftArrow[1,"long"], y = leftArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]),
			cbind(x = rightArrow[1,"long"], y = rightArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]))

	res <- as.data.frame(res, stringsAsFactors = FALSE)

	# Coordinates from which "N" will be plotted
	coordsN <- cbind(x = lon, y = (lat + onTop[1,"lat"])/2)

	return(list(res = res, coordsN = coordsN))
}
