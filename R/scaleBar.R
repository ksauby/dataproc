#' Create a Scale Bar
#' @description This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow
#' @param lon longitude of the bottom left point of the first rectangle to draw #' @param lat latitude of the bottom left point of the first rectangle to draw
#' @param distanceLon length of each rectangle
#' @param distanceLat width of each rectangle
#' @param distanceLegend distance between rectangles and legend texts
#' @param dist.units units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles)
#' @param rec.fill filling colour of the rectangles (default to white, and black, resp.)
#' @param rec2.fill filling colour of the rectangles (default to white, and black, resp.)
#' @param rec.colour colour of the rectangles (default to black for both)
#' @param rec2.colour colour of the rectangles (default to black for both)
#' @param legend.colour legend colour (default to black)
#' @param legend.size legend size (default to 3)
#' @param orientation (boolean) if TRUE (default), adds an orientation arrow to the plot
#' @param arrow.length length of the arrow (default to 500 km)
#' @param arrow.distance distance between the scale bar and the bottom of the arrow (default to 300 km)
#' @param arrow.North.size size of the "N" letter (default to 6)
#' @export

scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
	laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
	# First rectangle
	rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)

	# Second rectangle
	rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)

	# Legend
	scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)

	res <- list(rectangle1, rectangle2, scaleBarLegend)

	if(orientation){# Add an arrow pointing North
		coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
		arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
		res <- c(res, arrow)
	}
	return(res)
}