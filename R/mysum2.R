#' Return 1 for sums greater than 0.
#' @param x Vector of data.
mysum2 <- function(x) if (all(is.na(x))) NA else if (sum(x,na.rm=T) > 0) 1 else 0
