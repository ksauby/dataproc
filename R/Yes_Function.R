#' Standardize "Yes" values.
#' @param x Vector of data.
#' @export

Yes_Function <- function(x){	
	x[which(
		x == "Yes" | 
		x == "yes" |
		x == "y" |
		x == "Egg" | 
		x == "Larva" | 
		x == "Nymph"
	)] <- "1"
	return(x)
}