#' Run PCA in R
#' @param x
#' @param variable_list
#' @export

PCA_function <- function(x=x, variable_list=variable_list){
	x = x[, variable_list]
	prcomp(x, center=T, scale=T)
}
