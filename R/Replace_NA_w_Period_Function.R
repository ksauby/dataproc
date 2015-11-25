#' Replace NA with a ".".
#' @param x Vector of data.
#' @description Replace NAs with no text so they are not printed on datasheets.
#' @export

Replace_NA_w_Period_Function <- function(x){
	x[is.na(x) == "TRUE"] <- ""
	return(x)
}