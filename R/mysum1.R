#' Return NA for sums below 1
#' @param x Vector of data.
mysum1 <- function(x) if (sum(x,na.rm=T) > 0) 1 else NA