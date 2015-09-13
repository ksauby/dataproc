#' Standardize "WoodyTrunk" values.
#' @param x Vector of data.
WoodyTrunk_Function <- function(x){	
	x[
		x=="woody trunk" | 
		x=="woody tunk" | 
		x=="Woody Trunk"
	] <- "WoodyTrunk"
	return(x)
}