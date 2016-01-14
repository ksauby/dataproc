#' Confirm a Plant as Dead or Missing
#' 
#' @param x Vector of {0,1} values.
#' @description A plant is confirmed dead/missing if there area at least 2 observations (> 1) of the plant being dead and/or missing.
#' 
#' @export

Confirmed_Dead_Missing_Function <- function(x) {
	ifelse(sum(x, na.rm=TRUE) > 1, 1, 0)
}