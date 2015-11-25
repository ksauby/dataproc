#' Create a Scale Bar for a ggmap
#' @description Return a list whose elements are:
#' rectangle - a data.frame containing the coordinates to draw the first rectangle
#' rectangle2 - a data.frame containing the coordinates to draw the second rectangle
#' legend - a data.frame containing the coordinates of the legend texts, and the texts as well

#' @param lon longitude of the bottom left point of the first rectangle to draw #' @param lat latitude of the bottom left point of the first rectangle to draw
#' @param distanceLon length of each rectangle
#' @param distanceLat width of each rectangle
#' @param distanceLegend distance between rectangles and legend texts
#' @param dist.units units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles)
#' @export

createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){
	# First rectangle
	bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")

	topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
	rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
	lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
	rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)

	# Second rectangle t right of the first rectangle
	bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
	rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]),
	lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
	rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)

	# Now let's deal with the text
	onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
	onTop2 <- onTop3 <- onTop
	onTop2[1,"long"] <- bottomRight[1,"long"]
	onTop3[1,"long"] <- bottomRight2[1,"long"]

	legend <- rbind(onTop, onTop2, onTop3)
	legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
	return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}