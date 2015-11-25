#' Assign season for 2009 dates
#' @param DATES
#' @export

getSeason2009 <- function(DATES) {
    WS <- as.Date("2009-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2009-3-20",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2009-6-21",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2009-9-22",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
   # d <- as.Date(strftime(DATES, format="2012-%m-%d"))
	d = DATES

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#' Assign season for 2010 dates
#' @param DATES
#' @export

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

#' Assign season for 2011 dates
#' @param DATES
#' @export

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


#' Assign season for 2012 dates
#' @param DATES
#' @export

getSeason2012 <- function(DATES) {
    WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    #d <- as.Date(strftime(DATES, format="2012-%m-%d"))
	d = DATES

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


#' Assign season for 2013 dates
#' @param DATES
#' @export

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

#' Assign season for 2014 dates
#' @param DATES
#' @export

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

