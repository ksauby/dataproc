#' Return NA for vector of NAs
#' @param x Vectors of data.
#' @references found here on Nov. 19, 2014: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
mysum <- function(x) if (all(is.na(x))) NA else sum(x,na.rm=T) 