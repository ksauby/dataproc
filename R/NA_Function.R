#' Replace values with NA.
#' @param x Vector of data.
NA_Function <- function(x){	
	x[which(
		x=="Not Recorded" | 
		x=="not recorded" | 
		x=="Not recorded" | 
		x=="unknown" | 
		x=="Unknown" | 
		x=="Inf" |
		x=="-Inf" | 
		x=="NaN" | 
		x==999 | 
		x==-9999
	)] <- NA
	return(x)
}