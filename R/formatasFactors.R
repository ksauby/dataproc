#' Format as Factors
#'
#' @description Format variables as factors.
#' @param x Dataset
#' @param factors variables to be formatted as factors
#'
#' @export
	
formatasFactors <- function(x, factors) {
	for (i in 1:length(factors)) {
		if (factors[i] %in% names(x)) {
			x[, factors[i]] %<>% as.factor
		}
	}
	return(x)
}

