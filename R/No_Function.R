#' Change "No" values to 0.
#' @param x Vector of data.
No_Function <- function(x){	
	x[which(
		x=="None" | x=="No" | 
		x=="no" | 
		x=="none" | 
		x=="Feeding evidence, but no insects"
	)] <- "0"
	return(x)
}