#' Replace 1 with "Yes".
#' @param x Vector of data.
#' @export

Replace_1_w_Yes_Function <- function(x){	
	x[which(x == 1)] <- "Yes"
	return(x)
}