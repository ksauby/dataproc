#' Replace 0 with "No".
#' @param x Vector of data.
Replace_0_w_No_Function <- function(x){	
	x[which(x==0)] <- "No"
	return(x)
}