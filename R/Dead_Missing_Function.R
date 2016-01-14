#' Function to determine if plant was ever observed dead or missing
#' 
#' @param x Vector of {0,1} data.
#' @description A plant has been observed to be dead/missing if there is at least 1 observation of it being dead or missing.
#' 
#' @export

Dead_Missing_Function <- function(x) {
	ifelse(sum(x, na.rm=TRUE) > 0, 1, 0)
}
