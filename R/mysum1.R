#' Return NA for sums below 1
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
mysum1 <- function(x) if (sum(x,na.rm=T) > 0) 1 else NA