#' Datasets of Start Dates for the Astronomical Seasons for the Years 2009 - 2014
#'
#' This dataset contains the seasonal start dates for the years 2009 - 2014.
#'
#' \itemize{
#'   \item SE. Spring equinox date.
#'   \item SS. Summer solstice date.
#'   \item FE. Fall equinox date.
#'   \item WS. Winter solstice date.
#' }
#'
#' @format A data frame with 6 rows and 4 variables
#' @name seasons
#' @examples
#' seasons <- data.frame(
#'    SE = as.POSIXct(c("2008-03-20", "2009-3-20", "2010-3-20", "2011-3-20",
#' "2012-3-20", "2013-3-20", "2014-3-20", "2015-3-21", "2016-3-20"),
#'  format="%Y-%m-%d"),
#'    SS = as.POSIXct(c("2008-06-20", "2009-6-21", "2010-6-21", "2011-6-21",
#' "2012-6-20", "2013-6-21", "2014-6-21", "2015-6-21", "2016-6-20"),
#'  format="%Y-%m-%d"),
#'    FE = as.POSIXct(c("2008-09-22", "2009-9-22", "2010-9-23", "2011-9-23",
#' "2012-9-22", "2013-9-22", "2014-9-23", "2015-9-21", "2016-9-22"),
#'  format="%Y-%m-%d"),
#'    WS = as.POSIXct(c("2008-12-21", "2009-12-21", "2010-12-21", "2011-12-22",
#' "2012-12-21", "2013-12-21", "2014-12-21", "2015-12-21", "2016-12-21"),
#'  format="%Y-%m-%d")
#' )

NULL