# Replace NAs no text so they are not printed on datasheet
Replace_NA_w_Period_Function <- function(x){
	x[is.na(x)=="TRUE"] <- ""
	return(x)
}