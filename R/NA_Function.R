#' Replace values with NA.
#' @param x Vector of data.
#' @example
#' x = c(1,2,3,4," ", "Not Recorded")
#' NA_Function(x)
NA_Function <- function(x){	
	x[which(
		x == "Not Recorded" | 
		x == "not recorded" | 
		x == "Not recorded" | 
		x == "unknown" | 
		x == "Unknown" | 
		x == Inf |
		x == -Inf | 
		x == "NaN" | 
		x == NaN | 
		x == 999 | 
		x == 9999 |
		x == -9999 |
		x == "-999.9" |
		x == "9999" |
		x == " "
	)] <- NA
	return(x)
}