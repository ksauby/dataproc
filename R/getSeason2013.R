#' Assign season for 2013 dates
#' @param DATES
getSeason2013 <- function(DATES) {
    WS <- as.Date("2013-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2013-3-20",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2013-6-20",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2013-9-22",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    #d <- as.Date(strftime(DATES, format="2013-%m-%d"))
	d = DATES

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
