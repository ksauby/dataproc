#' Run PCA in R
#' 
#' @param x dataframe
#' @param variable_list list of variables
#' @description Prepare the data for running a PCA in R.
#'
#' @export

PCA_in_R_function <- function(x=x, variable_list=variable_list){
	x = x[, variable_list]
	prcomp(x, center=T, scale=T)
}
