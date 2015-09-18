#' Assign season for 2010 dates
#' @param DATES
getSeason2010 <- function(DATES) {
    WS <- as.Date("2010-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2010-3-20",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2010-6-21",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2010-9-23",  format = "%Y-%m-%d") # Fall Equinox
	d = DATES

    # Convert dates from any year to 2012 dates
    #d <- as.Date(strftime(DATES, format="2012-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
