#' Return unique values of a vector after removal of NA
#'
#' @param x Vector of data
#'
#' @export

Unique <- function(x) {
	x[!(is.na(x))] %>% unique
}