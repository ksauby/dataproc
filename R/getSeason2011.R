#' Assign season for 2011 dates
#' @param DATES
getSeason2011 <- function(DATES) {
    WS <- as.Date("2011-12-22", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2011-3-20",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2011-6-21",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2011-9-23",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    #d <- as.Date(strftime(DATES, format="2012-%m-%d"))
	d = DATES

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
