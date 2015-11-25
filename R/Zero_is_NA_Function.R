#' Replace 0 with NA.
#' @param x Vector of data.
#' @export

Zero_is_NA_Function <- function(x){	
	x[which(x == 0)] <- NA
	return(x)
}