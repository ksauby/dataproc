#' Return 1 for sums greater than 0.
#' @param x Vector of data.
#' @references inspired by: http://tolstoy.newcastle.edu.au/R/help/02a/3218.html
mysum2 <- function(x) if (all(is.na(x))) NA else if (sum(x,na.rm=T) > 0) 1 else 0
