#' A plant has been obvserved to be dead/missing if there is at least 1 observation of it being dead or missing.
#' @param x Vector of {0,1} data.
#' @export

Dead_Missing_Function <- function(x) {
	ifelse(sum(x, na.rm=TRUE) > 0, 1, 0)
}
