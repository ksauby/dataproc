#' Rename Convert 1/0 to Yes/No
#'
#' @param x Vector of 0/1 data
#'
#' @export

Yes_No_from_1_0_Function <- function(x){
	x[x > 0] <- "Yes"
	x[x == 0] <- "No"
	return(x)
}
