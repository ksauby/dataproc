#' Assign season for 2014 dates
#' @param DATES
getSeason2014 <- function(DATES) {
    WS <- as.Date("2014-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2014-3-20",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2014-6-20",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2014-9-22",  format = "%Y-%m-%d") # Fall Equinox

	d = DATES
    # Convert dates from any year to 2012 dates
    # d <- as.Date(strftime(DATES, format="2014-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
