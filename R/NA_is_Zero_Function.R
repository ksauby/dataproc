NA_is_Zero_Function <- function(x){	
	x[which(is.na(x))] <- 0
	return(x)
}